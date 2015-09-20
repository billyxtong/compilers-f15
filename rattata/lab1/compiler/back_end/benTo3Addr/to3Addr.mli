(* This step takes InfAddr code to 3Addr code. Currently, I'm taking
   the starter code's original InfAddr datatype as in put.
   Eventually, I will convert everything to use our InfAddr datatype,
   and then I will write another function that takes that as input
   (which is what we'll actually use). *)

val to3AddrFromTheirInfAddr: Tree.stm list -> Datatypesv1.tmp3AddrProg
