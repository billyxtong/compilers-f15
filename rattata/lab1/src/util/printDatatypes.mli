open Datatypesv1

val constToString : const -> string
val instrNameToString : instrName -> string
val regToString : reg -> string
val memAddrToString : memAddr -> string (* should I break this down more? *)
val assemArgToString : assemArg -> string
val assemArgsToString : assemArgs -> string
val assemInstrToString : assemInstr -> string
val tmpToString : tmp -> string
val tmpAssemArgToString : tmpAssemArg -> string
val tmpAssemArgsToString : tmpAssemArgs -> string
val tmpTwoAddrInstrToString : tmpTwoAddrInstr -> string
