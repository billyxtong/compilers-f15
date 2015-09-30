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
let rec munch_exp d e =
  match e with
    T.CONST n -> [T.MOVE (d, T.CONST n)]
  | T.TEMP t -> [T.MOVE(d, T.TEMP t)]
  | T.BINOP (binop, e1, e2) ->
      munch_binop d (binop, e1, e2)

(* munch_binop d (binop, e1, e2)
 * generates instruction to achieve d <- e1 binop e2
 * d must be TEMP(t) or REG(r)
 *)
and munch_binop d (binop, e1, e2) =
    let t1 = T.TEMP (Temp.create ())
    and t2 = T.TEMP (Temp.create ()) in
    munch_exp t1 e1
    @ munch_exp t2 e2
    @ [T.MOVE (d, T.BINOP(binop, t1, t2))]


(* munch_stm stm generates code to execute stm *)
let munch_instr = function
    T.MOVE (T.TEMP t1, e2) -> munch_exp (T.TEMP t1) e2
  | T.RETURN e ->
      (* return e is implemented as %eax <- e *)
      let t = T.TEMP(Temp.create()) in
      (munch_exp t e) @ T.RETURN t :: []
  | _ -> assert false

let rec to3Addr = function
    [] -> []
  | instr::instrs -> munch_instr instr @ to3Addr instrs
