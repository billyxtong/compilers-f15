(* hello from interface *)
open Datatypesv1

val getDefVars : tmp2AddrInstr array -> int -> int list 
val findPredecessors : ((int list) array) -> tmp2AddrInstr array -> int -> unit
val isDef : int -> tmp2AddrInstr array -> int -> bool
val analyzeLiveness : tmp2AddrInstr list -> int list -> int list -> Graph.graph
(* first int list is the list of tmps defined in the program, second is the params *)
