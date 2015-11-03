open Datatypesv1

val allocForFun : tmp2AddrFunDef -> assemFunDef
(* We need two spill regs now; see allocForFun.ml for comment *)  
val firstSpillReg: assemLoc (* applied the Reg constructor already *)
val secondSpillReg: assemLoc
