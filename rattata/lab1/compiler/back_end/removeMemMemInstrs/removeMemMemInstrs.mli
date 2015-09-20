(* Assembly code doesn't allow memory-to-memory instructions, like
   addl $8(rsp) $4(rsp), so we have to deal with them by using
   the spillage register. Shouldn't be too bad.
   I'm doing this before handling the wonky instructions. *)
open Datatypesv1
val removeMemMemInstrs: assemProg -> assemProg
