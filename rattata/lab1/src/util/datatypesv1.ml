type const = Int of int
type instrName = InstrName of string

type reg = RAX | RBX | RCX | RDX | RBP | RSP | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

type memAddr = reg * int

type assemArg = Reg of reg | MemAddr of memAddr | Const of const
type assemArgs = NONE | Arg of assemArg | SrcDest of assemArg * assemArg
type assemInstr = instrName * assemArgs

type tmp = int
type tmpAssemArg = Tmp of tmp | AssemArg of assemArg
type tmpAssemArgs = TmpNONE | TmpArg of tmpAssemArg | TmpSrcDest of tmpAssemArg * tmpAssemArg
type tmpTwoAddrInstr = instrName * tmpAssemArgs



