fun toString (Int(c) : const) = string_of_int c

fun toString (InstrName(s) : instrName) = s

fun toString (r : reg) = 
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

fun toString ((r, i) : memAddr) = concat([string_of_int(i); "("; toString(r); ")"])

fun toString (a : assemArg) = 
  match a with
        Reg(r) -> toString(r)
      | MemAddr(m) -> toString(m)
      | Const(c) -> toString(c)

fun toString(As : assemArgs) = 
  match As with
        NONE -> "none"
      | Arg(arg) -> toString(arg)
      | SrcDest(s,d) -> concat([toString(s); ", "; toString(d)])

fun toString((i, args) : assemInstr) = concat([toString(i); " "; toString(args)])

fun toString(t : tmp) = string_of_int t

fun toString(tmpArg : tmpAssemArg) = 
  match tmpArg with
        Tmp(tmp) -> toString(tmp)
      | AssemArg(arg) -> toString(arg)

fun toString(ts : tmpAssemArgs) = 
  match ts with
        TmpNONE -> "none"
      | TmpArg(tmpArg) -> toString(tmpArg)
      | TmpSrcDest(tmpSrc, tmpDest) -> concat([toString(tmpSrc); " "; toString(tmpDest)])

fun toString((instr, tmpArgs) : tmpTwoAddrInstr) = concat([toString(instr); " "; toString(tmpArgs)])
