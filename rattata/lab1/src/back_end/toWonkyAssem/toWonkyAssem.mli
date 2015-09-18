open Datatypesv1
(* Takes an assem prog, and turns the wonky instructions into
   actual assembly, like division and mod.
   For example, we can't assign the divisor for an idiv
   instruction into eax, since eax needs to contain the thing
   we're dividing. *)
val toWonky2Addr: assemProg -> assemProgWithWonky
