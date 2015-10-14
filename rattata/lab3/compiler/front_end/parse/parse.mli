(* L1 Compiler
 * Parsing
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *
 * Gluing together the pieces produced by ocamllex and ocamlyacc
 *)

(* First arg is the main source file, second is the header file *)
val parse : string -> string -> Ast.preElabOverallAST
