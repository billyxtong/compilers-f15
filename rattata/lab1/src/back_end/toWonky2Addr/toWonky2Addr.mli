open Datatypesv1
(* Goes from Two Address code to A slightly superset of
   Two Address code where certain instructions, such as idiv,
   are already turned into assembly-ish stuff.
   This is being done before register allocation so that we
   don't accidentally assign registers required for certain
   wonky instructions to be used in a problematic way.
   For example, we can't assign the divisor for an idiv
   instruction into eax, since eax needs to contain the thing
   we're dividing. *)
val toWonky2Addr: tmp2AddrProg -> tmpWonkyProg
