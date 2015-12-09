(* Implements a "convenient munch" algorithm *)

open Core.Std
open Datatypesv1

(* Note: there's a bunch of "assert(false)'s", because
   we first convert all of the function calls and control
   to infAddr but leave the mem stuff alone. So there are
   constructors for macro mem operations (array access,
   field access, etc) in the infAddr type. But once
   we get to this point, they should all be gone. Derefs
   should be gone too; they turn into movs. *)

let getSizeForType = function
    INT -> BIT32
  | BOOL -> BIT32
  | Pointer _ -> BIT64
  | _ -> assert(false)

let rec munch_bool_instr = function
    TmpInfAddrTest (bool_exp, TmpBoolArg TmpLoc t) ->
        let t' = Tmp (Temp.create()) in
        let (instrs, dest) = munch_exp t' (TmpBoolExpr bool_exp) 0 in
        instrs @ Tmp3AddrBoolInstr (TmpTest (dest, t))::[]
  | TmpInfAddrCmp (opSize, e1, e2) ->
        let t1 = Tmp (Temp.create()) in
        let t2 = Tmp (Temp.create()) in
        let t3 = Tmp (Temp.create()) in 
        let (instrs1, dest1) = munch_exp t1 e1 0 in
        let (instrs2, dest2) = munch_exp t2 e2 0 in
        instrs1 @ instrs2 @ Tmp3AddrMov(opSize, dest2, TmpVar t3) ::
        Tmp3AddrBoolInstr (TmpCmp (opSize, dest1, TmpVar t3))::[]
   (* | TmpInfAddrCmp (opSize, int_exp, TmpIntArg TmpLoc t) -> *)
   (*      let t' = Tmp (Temp.create()) in *)
   (*      let (instrs, dest) = munch_exp t' (TmpIntExpr int_exp) 0 in *)
   (*      instrs @ Tmp3AddrBoolInstr (TmpCmp (opSize, dest, t))::[] *)
  | TmpInfAddrTest (bool_exp, non_tmp_exp) ->
        let t1 = Tmp (Temp.create()) in
        let t2 = Tmp (Temp.create()) in
        let (instrs_for_arg, arg_dest) = munch_exp t1 (TmpBoolExpr bool_exp) 0 in
        let (instrs_for_loc, loc_dest) = munch_exp t2 (TmpBoolExpr non_tmp_exp) 0 in
        (* If loc_dest = t2 then the mov instr will do nothing and will be
           removed later *)
        instrs_for_arg @ instrs_for_loc
        @ Tmp3AddrMov(BIT32, loc_dest, TmpVar t2)
        ::Tmp3AddrBoolInstr (TmpTest (arg_dest, TmpVar t2))::[]
(* d is the suggested destination, but we might have to generate more
   tmps anyway if there is a nested binop *)
and munch_exp d e depth : tmp3AddrInstr list * tmpArg =
  match e with
     TmpIntExpr (TmpInfAddrBinopExpr (int_binop, int_e1, int_e2)) ->
         let t = if depth > 0 then Tmp (Temp.create()) else d in
         (munch_int_binop t (int_binop, int_e1, int_e2) (depth + 1),
          TmpLoc (TmpVar t))
   | TmpIntExpr (TmpIntArg e) -> ([], e)
       (* Int and Bool function calls are identical from now on, I think *)
   | TmpIntExpr (TmpIntSharedExpr (TmpInfAddrFunCall (fName, args))) ->
       let t = Tmp (Temp.create()) in
       let (instrs, dests) = munch_fun_args args in
       (instrs @ Tmp3AddrFunCall (BIT32, fName, dests, Some (TmpVar t))::[],
        TmpLoc (TmpVar t))
   | TmpBoolExpr (TmpBoolArg e) -> ([], e)
         (* Note: no nested bool exps because we unwrap them all in
            toInfAddr! *)
   | TmpBoolExpr (TmpBoolSharedExpr (TmpInfAddrFunCall (fName, args))) ->
       let t = Tmp (Temp.create()) in
       let (instrs, dests) = munch_fun_args args in
       (instrs @ Tmp3AddrFunCall (BIT32, fName, dests, Some (TmpVar t))::[],
        TmpLoc (TmpVar t))
   | TmpPtrExpr (TmpPtrSharedExpr (TmpInfAddrFunCall (fName, args))) ->
       let t = Tmp (Temp.create()) in
       let (instrs, dests) = munch_fun_args args in
       (instrs @ Tmp3AddrFunCall (BIT64, fName, dests, Some (TmpVar t))::[],
        TmpLoc (TmpVar t))
   | TmpPtrExpr (TmpPtrArg e) -> ([], e)
   | TmpPtrExpr (TmpInfAddrPtrBinop (op, e1, e2)) ->
         let t = if depth > 0 then Tmp (Temp.create()) else d in
         (munch_ptr_binop t (op, e1, e2) (depth + 1),
          TmpLoc (TmpVar t))
   | TmpPtrExpr (TmpPtrSharedExpr (TmpInfAddrDeref p)) ->
         let (instrs, pDest) = munch_exp d (TmpPtrExpr p) depth in
         let t = Tmp (Temp.create()) in
         (* type of pointer doesn't matter *)
         (instrs @ Tmp3AddrMov(getSizeForType (Pointer Poop), pDest, TmpVar t)::[],
          TmpLoc (TmpDeref t))
   | TmpIntExpr (TmpIntSharedExpr (TmpInfAddrDeref p)) ->
         let (instrs, pDest) = munch_exp d (TmpPtrExpr p) depth in
         let t = Tmp (Temp.create()) in
         (* type of pointer doesn't matter *)
         (instrs @ Tmp3AddrMov(getSizeForType (Pointer Poop), pDest, TmpVar t)::[],
          TmpLoc (TmpDeref t))
   | TmpBoolExpr (TmpBoolSharedExpr (TmpInfAddrDeref p)) ->
         let (instrs, pDest) = munch_exp d (TmpPtrExpr p) depth in
         let t = Tmp (Temp.create()) in
         (* type of pointer doesn't matter *)
         (instrs @ Tmp3AddrMov(getSizeForType (Pointer Poop), pDest, TmpVar t)::[],
          TmpLoc (TmpDeref t))
   | TmpCharExpr e -> munch_exp d (TmpIntExpr e) depth
   | TmpIntExpr (TmpIntSharedExpr (TmpInfAddrFunPtrCall (funPtr, args))) ->
        munch_fun_ptr_call d (funPtr, args) depth BIT32
   | TmpBoolExpr (TmpBoolSharedExpr (TmpInfAddrFunPtrCall (funPtr, args))) ->
        munch_fun_ptr_call d (funPtr, args) depth BIT32
   | TmpPtrExpr (TmpPtrSharedExpr (TmpInfAddrFunPtrCall (funPtr, args))) ->
        munch_fun_ptr_call d (funPtr, args) depth BIT64
   | TmpPtrExpr (TmpInfAddrAddressOfFunction fName) ->
        let t = Tmp (Temp.create()) in
        (Tmp3AddrGetFunAddress (fName, TmpVar t)::[], TmpLoc (TmpVar t))
   | e -> let () = print_string("failing: " ^ PrintDatatypes.tmpExprToString e
                                  ^ "\n") in
                                  assert(false)

and munch_fun_ptr_call d (funPtr, args) depth opSize =
  let t = Tmp (Temp.create()) in
  let (instrsForPtr, TmpLoc ptrDest) = munch_exp (Tmp (Temp.create()))
      (TmpPtrExpr funPtr) depth in
  let (argInstrs, argDests) = munch_fun_args args in
  (instrsForPtr @ argInstrs @
   Tmp3AddrFunPtrCall (opSize, ptrDest, argDests, Some (TmpVar t))::[],
   TmpLoc (TmpVar t))

     
(* in a ptr binop, the second arg is always an int. *)
and munch_ptr_binop d (ptr_binop, e1, e2) depth =
   let (instrs1, dest1) = munch_exp d (TmpPtrExpr e1) depth in
   let (instrs2, dest2) = munch_exp d (TmpIntExpr e2) depth in
   instrs1 @ instrs2 @ [Tmp3AddrPtrBinop (ptr_binop, dest1, dest2, TmpVar d)] 


(* Note: all bool binops are removed in toInfAddr because we
   are clever :) *)
and munch_int_binop d (int_binop, e1, e2) depth =
    let (instrs1, dest1) = munch_exp d (TmpIntExpr e1) depth in
    let (instrs2, dest2) =
       (match (int_binop, e2) with
         (* can't idivl by constants :( *)
          (FAKEDIV, TmpIntArg (TmpConst  c)) -> 
              let t = Tmp (Temp.create()) in
              (Tmp3AddrMov (BIT32, TmpConst c, TmpVar t)::[], TmpLoc (TmpVar t))
        |(FAKEMOD, TmpIntArg (TmpConst c)) -> 
              let t = Tmp (Temp.create()) in
              (Tmp3AddrMov (BIT32, TmpConst c, TmpVar t)::[], TmpLoc (TmpVar t))
        | _ -> munch_exp d (TmpIntExpr e2) depth) in
    match (dest1, dest2) with
          (TmpLoc (TmpDeref t1), TmpLoc (TmpDeref t2)) ->
               (* Need to make sure we evaluate t1 first *)
               (let t1' = Tmp (Temp.create()) in
               instrs1 @ Tmp3AddrMov(getSizeForType (Pointer Poop),
                          TmpLoc (TmpDeref t1), TmpVar t1')::[]
               @ instrs2 @ [Tmp3AddrBinop (int_binop, TmpLoc (TmpVar t1'), dest2,
                                          TmpVar d)])
       | _ -> (instrs1 @ instrs2
               @ (if false (* TmpLoc d = dest1 *) then []
                  else [Tmp3AddrBinop (int_binop, dest1, dest2, TmpVar d)]))

(* takes a tmpExpr list and returns (instrs, tmpArgs) where instrs is a
   list of all required instructions, and tmpArgs is a list of the
   munched arguments *)
and munch_fun_args = function
    [] -> ([], [])
  | arg::args -> let t = Tmp (Temp.create()) in
                 let (curr_instrs, curr_dest) = munch_exp t arg 0 in
                 let (rest_instrs, rest_dests) = munch_fun_args args in
                 (curr_instrs @ rest_instrs, curr_dest::rest_dests)

let rec munch_lval = function
    TmpVarLVal t -> ([], TmpVar t)
  | TmpDerefLVal (TmpVarLVal t) -> ([], TmpDeref t)
  | TmpDerefLVal lval -> let t = Tmp (Temp.create()) in
                         let (instrs, lvalFinal) = munch_lval lval in
                         (instrs @ Tmp3AddrMov(getSizeForType (Pointer Poop),
                                               TmpLoc lvalFinal, TmpVar t)::[], TmpDeref t)
  | _ -> assert(false)               

(* munch_stm stm generates code to execute stm *)
let munch_instr = function
    TmpInfAddrMov (opSize, e, lval) ->
       let munch_dest = (match lval with
                            TmpVarLVal t -> t
                          | _ -> Tmp (Temp.create())) in
       let (instrsForE, eDest) = munch_exp munch_dest e 0 in
       let (instrsForLVal, lvalDest) = munch_lval lval in
       (* apparently e is evaluated first? *)
       instrsForE @ instrsForLVal @ [Tmp3AddrMov (opSize, eDest, lvalDest)]
  | TmpInfAddrJump j -> Tmp3AddrJump j::[]
  | TmpInfAddrBoolInstr instr -> munch_bool_instr instr
  | TmpInfAddrReturn (argSize, e) ->
       let (instrs, dest) = munch_exp (Tmp (Temp.create())) e 0 in
       instrs @ Tmp3AddrReturn (argSize, dest) :: []
  | TmpInfAddrLabel jumpLabel -> Tmp3AddrLabel jumpLabel :: []
  | TmpInfAddrVoidFunCall (fName, args) -> 
       let (instrs, dests) = munch_fun_args args in
       (* The None means no destination for fun call *)
       (* VoidFunCalls don't have sizes so we'll just say 32-bit
          because w/e *)
       instrs @ Tmp3AddrFunCall(BIT32, fName, dests, None)::[]
  | TmpInfAddrMaskUpper t -> Tmp3AddrMaskUpper t::[]
           
let rec finalPass = function
    [] -> []
  | instr::instrs -> match instr with
        Tmp3AddrMov (opSize, TmpLoc t1, t2) ->
            if t1 = t2 then finalPass instrs
            else instr :: finalPass instrs
  | _ -> instr::finalPass instrs

let rec to3AddrRec = function
    [] -> []
  | instr::instrs -> munch_instr instr @ to3AddrRec instrs

let funInstrsTo3Addr instrs = finalPass (to3AddrRec instrs)

let rec to3Addr = function
     [] -> []
   | TmpInfAddrFunDef(fName, args, instrs)::rest ->
          Tmp3AddrFunDef(fName, args, funInstrsTo3Addr instrs)
          :: to3Addr rest
   | TmpStructDef _ ::rest -> assert(false)
