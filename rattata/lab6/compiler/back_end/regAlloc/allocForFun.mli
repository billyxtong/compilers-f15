open Datatypesv1

val getTempList : tmp2AddrInstr list -> int list
val allocForFun : tmp2AddrFunDef -> (ident, size array) Hashtbl.t -> assemFunDef
(* We need two spill regs now; see allocForFun.ml for comment *)  
val firstSpillReg: assemLoc (* applied the Reg constructor already *)
val secondSpillReg: assemLoc
