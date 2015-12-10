open Datatypesv1
module H = Hashtbl    

let intBinopTo2Addr dest = function
    (op, arg1, arg2) ->
       let t = Tmp(Temp.create()) in
       (* The order of where arg1 and arg2 go here is kind of
          weird: remember Tmp2AddrBinop(op, src, dest is
          dest <- dest (op) src) *)
       let op_instr = Tmp2AddrBinop(op, arg2, TmpVar t) in
       Tmp2AddrMov(BIT32, arg1, TmpVar t)::op_instr::
       [Tmp2AddrMov(BIT32, TmpLoc (TmpVar t), dest)]

let ptrBinopTo2Addr dest = function
    (op, arg1, arg2) ->
       let t = Tmp(Temp.create()) in
       let op_instr = Tmp2AddrPtrBinop(op, arg2, TmpVar t) in
       Tmp2AddrMov(BIT64, arg1, TmpVar t)::op_instr::
       [Tmp2AddrMov(BIT64, TmpLoc (TmpVar t), dest)]

let trans_arg paramToTmpMap arg =
    try TmpLoc (H.find paramToTmpMap arg)
    with
        Not_found -> arg

let trans_loc paramToTmpMap loc =
    match loc with
        TmpVar t -> (try H.find paramToTmpMap (TmpLoc loc)
                    with Not_found -> loc)
      | TmpDeref t -> (try let TmpVar tMapResult =
                             H.find paramToTmpMap (TmpLoc (TmpVar t)) in
                           TmpDeref tMapResult
                    (* Need to check by the vars, because that's how the map works *)
                       with Not_found -> loc)

let instrTo2Addr paramToTmpMap = function
    Tmp3AddrMov (opSize, src, dest) ->
        [Tmp2AddrMov (opSize, trans_arg paramToTmpMap src,
                      trans_loc paramToTmpMap dest)]
  | Tmp3AddrReturn (opSize, arg) ->
    [Tmp2AddrReturn (opSize, trans_arg paramToTmpMap arg)]
  | Tmp3AddrBinop (op, arg1, arg2, dest) -> intBinopTo2Addr
         (trans_loc paramToTmpMap dest) (op, trans_arg paramToTmpMap arg1,
                                         trans_arg paramToTmpMap arg2)
  | Tmp3AddrJump j -> Tmp2AddrJump j::[]
  | Tmp3AddrLabel jumpLabel -> Tmp2AddrLabel jumpLabel::[]
  | Tmp3AddrBoolInstr (TmpTest (arg, loc)) ->
        Tmp2AddrBoolInstr (TmpTest (trans_arg paramToTmpMap arg,
                                    trans_loc paramToTmpMap loc))::[]
  | Tmp3AddrBoolInstr (TmpCmp (opSize, arg, loc)) ->
        Tmp2AddrBoolInstr (TmpCmp (opSize, trans_arg paramToTmpMap arg,
                                    trans_loc paramToTmpMap loc))::[]
  | Tmp3AddrFunCall (retSize, fName, args, dest) ->
      (* At the beginning of the function, move all of the args into new tmps.
         Keep live ranges of pre-colored tmps short! We're doing it here because
         we only have to match against one constructor here *)
      let newArgs = List.map (trans_arg paramToTmpMap) args in
      let newDest = (match dest with
                         Some dest' -> Some (trans_loc paramToTmpMap dest')
                       | None -> None) in
      Tmp2AddrFunCall (retSize, fName, newArgs, newDest)::[]
  | Tmp3AddrFunPtrCall (retSize, funPtr, args, dest) ->
      (* same as for normal func calls, but also move the funPtr into a new
         thing first, because what if the funPtr is an argument to the caller?
         Then it could be allocated RDI for example, but it would interfere
         with its first arg, which also needs RDI *)
      let newArgs = List.map (trans_arg paramToTmpMap) args in
      let newDest = (match dest with
                         Some dest' -> Some (trans_loc paramToTmpMap dest')
                       | None -> None) in
      let t = Tmp (Temp.create()) in
      Tmp2AddrMov(BIT64, TmpLoc funPtr, TmpVar t)::
      Tmp2AddrFunPtrCall (retSize, TmpVar t, newArgs, newDest)::[]
  | Tmp3AddrGetFunAddress (id, dest) -> Tmp2AddrGetFunAddress (id, dest)::[]
  | Tmp3AddrPtrBinop (op, arg1, arg2, dest) -> ptrBinopTo2Addr
       (trans_loc paramToTmpMap dest) (op, trans_arg paramToTmpMap arg1,
                                       trans_arg paramToTmpMap arg2)
  | Tmp3AddrMaskUpper t -> Tmp2AddrMaskUpper t::[]

let rec funTo2Addr paramToTmpMap = function
   [] -> []
 | instr::instrs -> instrTo2Addr paramToTmpMap instr @ funTo2Addr paramToTmpMap instrs

(* Returns (moves, map) where moves is a list of moves that move params into
   new tmps. This keeps the live ranges of pre-colored tmps short! map
   maps tmps that were params to the new tmp (the one it ends up in after the moves

   This is important because we need to still be able to use ECX for shifts and
   EDI for division, even though they will contain fun args *)
let rec setUpParams params map =
    match params with
       [] -> []
     | (param_t, param_size)::rest ->
         let t = Tmp (Temp.create()) in
         let () = H.add map (TmpLoc (TmpVar param_t)) (TmpVar t) in
         Tmp2AddrMov(param_size, TmpLoc (TmpVar param_t), TmpVar t)
         ::setUpParams rest map

let rec to2Addr = function
   [] -> []
 | Tmp3AddrFunDef(fName, params, instrs)::rest ->
   let paramToTmpMap = H.create 100 in
   let setUpInstrs = setUpParams params paramToTmpMap in
   let funInstrs = funTo2Addr paramToTmpMap instrs in
   Tmp2AddrFunDef(fName, params, setUpInstrs @ funInstrs)
   :: to2Addr rest
