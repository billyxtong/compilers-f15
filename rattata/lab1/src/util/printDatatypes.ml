open Datatypesv1
open String

let constToString (Int(c) : const) = string_of_int c

let instrNameToString (InstrName(s) : instrName) = s

let regToString (r : reg) = 
  match r with
        RAX -> "rax"
      | RBX -> "rbx"
      | RCX -> "rcx"
      | RDX -> "rdx"
      | RBP -> "rbp"
      | RSP -> "rsp"
      | RSI -> "rsi"
      | RDI -> "rdi"
      | R8  -> "r8"
      | R9  -> "r9"
      | R10 -> "r10"
      | R11 -> "r11"
      | R12 -> "r12"
      | R13 -> "r13"
      | R14 -> "r14"
      | R15 -> "r15"

let memAddrToString ((register, instr) : memAddr) = concat "" [string_of_int(instr); "("; regToString(register); ")"]

let assemArgToString (arg : assemArg) = 
  match arg with
        Reg(r) -> regToString(r)
      | MemAddr(register, instr) -> memAddrToString(register, instr)
      | Const(c) -> constToString(c)

let assemArgsToString(args : assemArgs) = 
  match args with
        NONE -> "none"
      | Arg(arg) -> assemArgToString(arg)
      | SrcDest(src, dest) -> concat "" [assemArgToString(src); ", "; assemArgToString(dest)]

let assemInstrToString((instr, args) : assemInstr) = concat "" [instrNameToString(instr); " "; assemArgsToString(args)]

let tmpToString(t : tmp) = string_of_int t

let tmpAssemArgToString(tmpArg : tmpAssemArg) = 
  match tmpArg with
        Tmp(tmp) -> tmpToString(tmp)
      | AssemArg(arg) -> assemArgToString(arg)

let tmpAssemArgsToString(ts : tmpAssemArgs) = 
  match ts with
        TmpNONE -> "none"
      | TmpArg(tmpArg) -> tmpAssemArgToString(tmpArg)
      | TmpSrcDest(tmpSrc, tmpDest) -> concat "" [tmpAssemArgToString(tmpSrc); " "; tmpAssemArgToString(tmpDest)]

let tmpTwoAddrInstrToString((instr, tmpArgs) : tmpTwoAddrInstr) = concat "" [instrNameToString(instr); " "; 
                                                                              tmpAssemArgsToString(tmpArgs)]
