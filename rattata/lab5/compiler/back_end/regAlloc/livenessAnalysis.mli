(* hello from interface *)
open Datatypesv1

val findPredecessors : ((int list) array) -> tmp2AddrInstr array -> int -> unit
val isDef : int -> tmp2AddrInstr array -> int -> bool
val analyzeLiveness : tmp2AddrInstr list -> int list -> Graph.graph
