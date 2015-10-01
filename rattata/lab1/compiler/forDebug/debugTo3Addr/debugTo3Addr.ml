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

module T = Tree

let rec munch_exp d e depth =
  match e with
    T.BINOP (binop, e1, e2) ->
      let t = if depth > 0 then T.TEMP (Temp.create()) else d
      in
      (munch_binop t (binop, e1, e2) (depth + 1), t)
  | _ -> ([], e)

and munch_binop d (binop, e1, e2) depth =
    let (instrs1, dest1) = munch_exp d e1 depth in
    let (instrs2, dest2) =
       match (binop, e2) with
          (T.DIV, T.CONST _) -> (* can't idivl by constants :( *)
              let t = T.TEMP (Temp.create()) in
              ([T.MOVE (t, e2)], t)
        |(T.MOD, T.CONST _) -> (* can't idivl by constants :( *)
              let t = T.TEMP (Temp.create()) in
              ([T.MOVE (t, e2)], t)
        | _ -> munch_exp d e2 depth
          in
    instrs1 @ instrs2
    @ (if d = dest1 then []
       else [T.MOVE (d, T.BINOP(binop, dest1, dest2))])


(* munch_stm stm generates code to execute stm *)
let munch_instr = function
    T.MOVE (T.TEMP t1, e2) ->
        let (instrs, dest) = munch_exp (T.TEMP t1) e2 0 in
        instrs @ [T.MOVE (T.TEMP t1, dest)]
  | T.RETURN e ->
        let (instrs, dest) = munch_exp (T.TEMP (Temp.create())) e 0 in
        instrs @ T.RETURN dest :: []
  | _ -> assert false

(* just removes all move instrs with same src and dest *)
let rec finalPass = function
    [] -> []
  | instr::instrs -> match instr with
                         T.MOVE (T.TEMP t1, T.TEMP t2) ->
                              if t1 = t2 then finalPass instrs
                                  else instr :: finalPass instrs
                       | _ -> instr::finalPass instrs

let rec to3AddrRec = function
    [] -> []
  | instr::instrs -> munch_instr instr @ to3AddrRec instrs

let to3Addr instrs = finalPass (to3AddrRec instrs)
