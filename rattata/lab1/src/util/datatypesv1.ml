(* Will eventually add Pointer of pointer, etc *)
type const = Int of int
(* movl, addl, cdq, and so on *)

type reg = RAX | RBX | RCX | RDX | RBP | RSP | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

(* the int is the memory offest from the register *)
type memAddr = reg * int

(* These are for actual assembly instructions. Tmps are not allowed. *)
type assemArg = Reg of reg | MemAddr of memAddr | Const of const
type assemBinop = ADD of assemArg * assemArg | MUL of assemArg * assemArg
type assemInstr = MOV of assemArg * assemArg | BINOP of assemBinop

(* These allow tmps, but also allow actual assembly instructions.
   These are used for all the steps that involve tmps.
   Note: Because we allow actual assembly instructions, memory
   addresses are allowed. Memory addresses should not occur prior
   to register allocation, at least not in L1 (unless I'm missing
   something) *)
type tmp = int
type tmpAssemArg = Tmp of tmp | AssemArg of assemArg
type tmp2AddrBinop = Tmp2AddrAdd of tmpAssemArg * tmpAssemArg
                   | Tmp2AddrMul of tmpAssemArg * tmpAssemArg
type tmpTwoAddrInstr = Tmp2Mov of tmpAssemArg * tmpAssemArg
                     | Tmp2AddrBinop of tmp2AddrBinop



