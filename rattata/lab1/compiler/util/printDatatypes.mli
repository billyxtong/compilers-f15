open Datatypesv1
module A = Ast
val c0typeToString : A.c0type -> string
val constToString : const -> string
val regToString : reg -> string
val memAddrToString : memAddr -> string
val assemLocToString : assemLoc -> string
val assemArgToString : assemArg -> string
val binopToString : binop -> string
val assemBinopInstrToString : assemBinopInstr -> string
val assemInstrToString : assemInstr -> string
val assemProgToString : assemProg -> string
val assemInstrWonkyToString : assemInstrWonky -> string
val assemProgWonkyToString : assemProgWonky -> string
val tmpBinopToString : tmpBinop -> string
val tmpToString : tmp -> string
val tmpArgToString : tmpArg -> string
val tmp2AddrBinopToString : tmp2AddrBinop -> string
val tmp2AddrInstrToString : tmp2AddrInstr -> string
val tmp2AddrProgToString : tmp2AddrProg -> string
val tmp3AddrBinopToString : tmp3AddrBinop -> string
val tmp3AddrInstrToString : tmp3AddrInstr -> string
val tmp3AddrProgToString : tmp3AddrProg -> string
