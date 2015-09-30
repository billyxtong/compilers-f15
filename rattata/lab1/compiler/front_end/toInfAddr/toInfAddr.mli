(* L1 Compiler
 * AST2 (post elaboration) -> infinite address, infinite tmps
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified by: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

val toInfAddr : Ast.preElabAST -> Tree.stm list
