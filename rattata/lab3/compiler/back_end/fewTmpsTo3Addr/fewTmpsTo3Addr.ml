(* L1 Compiler
 * Assembly Code Generator for FAKE assembly
 * Author: Alex Vaynberg <alv@andrew.cmu.edu>
 * Based on code by: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *
 * Implements a "convenient munch" algorithm
 *)

open Core.Std
open Datatypesv1

let rec munch_bool_instr = function
    TmpInfAddrTest (TmpBoolArg e1, TmpBoolArg TmpLoc t) ->
        Tmp3AddrBoolInstr (TmpTest (e1, t))::[]
  | TmpInfAddrTest (TmpBoolArg e1, TmpBoolArg TmpConst c) ->
        let t = Tmp (Temp.create()) in
        Tmp3AddrMov(TmpConst c, t)
        ::Tmp3AddrBoolInstr (TmpTest (e1, t))::[]
  | TmpInfAddrCmp (int_exp1, int_exp2) ->
        let t1 = Tmp (Temp.create()) in
        let t2 = Tmp (Temp.create()) in
        let t3 = Tmp (Temp.create()) in 
        let (instrs1, dest1) = munch_exp t1 (TmpIntExpr int_exp1) 0 in
        let (instrs2, dest2) = munch_exp t2 (TmpIntExpr int_exp2) 0 in
        instrs1 @ instrs2 @ Tmp3AddrMov(dest2, t3) ::
        Tmp3AddrBoolInstr (TmpCmp (dest1, t3))::[]
                                               
(* d is the suggested destination, but we might have to generate more
   tmps anyway if there is a nested binop *)
and munch_exp d e depth =
  match e with
     TmpIntExpr (TmpInfAddrBinopExpr (int_binop, int_e1, int_e2)) ->
         let t = if depth > 0 then Tmp (Temp.create()) else d in
         (munch_int_binop t (int_binop, int_e1, int_e2) (depth + 1),
          TmpLoc t)
   | TmpBoolExpr (TmpBoolArg e) -> ([], e)
   | TmpIntExpr (TmpIntArg e) -> ([], e)

(* Note: all bool binops are removed in toInfAddr because we
   are clever :) *)
and munch_int_binop d (int_binop, e1, e2) depth =
    let (instrs1, dest1) = munch_exp d (TmpIntExpr e1) depth in
    let (instrs2, dest2) =
       match (int_binop, e2) with
         (* can't idivl by constants :( *)
          (FAKEDIV, TmpIntArg (TmpConst  c)) -> 
              let t = Tmp (Temp.create()) in
              (Tmp3AddrMov (TmpConst c, t)::[], TmpLoc t)
        |(FAKEMOD, TmpIntArg (TmpConst c)) -> 
              let t = Tmp (Temp.create()) in
              (Tmp3AddrMov (TmpConst c, t)::[], TmpLoc t)
        | _ -> munch_exp d (TmpIntExpr e2) depth in
    instrs1 @ instrs2
    @ (if false (* TmpLoc d = dest1 *) then []
       else [Tmp3AddrBinop (int_binop, dest1, dest2, d)])

(* takes a tmpExpr list and returns (instrs, tmpArgs) where instrs is a
   list of all required instructions, and tmpArgs is a list of the
   munched arguments *)
and munch_fun_args = function
    [] -> ([], [])
  | arg::args -> let t = Tmp (Temp.create()) in
                 let (curr_instrs, curr_dest) = munch_exp t arg 0 in
                 let (rest_instrs, rest_dests) = munch_fun_args args in
                 (curr_instrs @ rest_instrs, curr_dest::rest_dests)
    
(* munch_stm stm generates code to execute stm *)
let munch_instr = function
    TmpInfAddrMov (e, t) ->
       let (instrs, intermediate_dest) = munch_exp t e 0 in
       instrs @ [Tmp3AddrMov (intermediate_dest, t)]
  | TmpInfAddrJump j -> Tmp3AddrJump j::[]
  | TmpInfAddrBoolInstr instr -> munch_bool_instr instr
  | TmpInfAddrReturn e ->
       let (instrs, dest) = munch_exp (Tmp (Temp.create())) e 0 in
       instrs @ Tmp3AddrReturn dest :: []
  | TmpInfAddrLabel jumpLabel -> Tmp3AddrLabel jumpLabel :: []
  | TmpInfAddrVoidFunCall (fName, args) -> 
       let (instrs, dests) = munch_fun_args args in
       (* The None means no destination for fun call *)
       instrs @ Tmp3AddrFunCall(fName, dests, None)::[]
  | TmpInfAddrVoidReturn -> Tmp3AddrVoidReturn
           
let rec finalPass = function
    [] -> []
  | instr::instrs -> match instr with
        Tmp3AddrMov (TmpLoc t1, t2) ->
            if t1 = t2 then finalPass instrs
            else instr :: finalPass instrs
  | _ -> instr::finalPass instrs

let rec to3AddrRec = function
    [] -> []
  | instr::instrs -> munch_instr instr @ to3AddrRec instrs

let to3Addr instrs = finalPass (to3AddrRec instrs)
