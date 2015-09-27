(* everything in c0 is an int! *)              
type const = int

type reg = EAX | EBX | ECX | EDX | RBP | RSP | ESI | EDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

(* the int is the memory offest from the register *)
type memAddr = reg * int

(* These are for actual assembly instructions. Tmps are not allowed. *)
type binop = ADD | MUL | SUB | FAKEDIV | FAKEMOD
type assemLoc = Reg of reg | MemAddr of memAddr
type assemArg = AssemLoc of assemLoc | Const of const
type assemBinopInstr = binop * assemArg * assemLoc
type assemInstr = MOV of assemArg * assemLoc
                | MOVQ of assemArg * assemLoc
                | BINOP of assemBinopInstr
                | PUSH of reg
                | POP of reg
                | RETURN
type assemProg = assemInstr list

(* Assembly Code with wonky instructions (i.e. idiv, etc) *)
(* This comes after 2Addr in the pipeline, but needs to be below
   here so that they can refer to 2AddrInstrs (since this is a
   strict superset of normal 2Addr *)
type assemInstrWonky = AssemInstr of assemInstr
                   | CDQ (* needed for idiv *)
                   | IDIV of assemArg
type assemProgWonky = assemInstrWonky list

(* Below here allows tmps, but also allows actual assembly instructions.
   Note: Because we allow actual assembly instructions, memory
   addresses are allowed. Memory addresses should not occur prior
   to register allocation, at least not in L1 (unless I'm missing
   something) *)
type tmp = Tmp of int
type tmpArg = TmpLoc of tmp | TmpConst of const
type tmpBinop = TmpBinop of binop

(* Two Address Code *)
type tmp2AddrBinop = tmpBinop * tmpArg * tmp
type tmp2AddrInstr = Tmp2AddrMov of tmpArg * tmp
                   | Tmp2AddrBinop of tmp2AddrBinop
                   | Tmp2AddrReturn of tmpArg
type tmp2AddrProg = tmp2AddrInstr list

(* Three Address Code *)
type tmp3AddrBinop = tmpBinop * tmpArg * tmpArg *  tmp
type tmp3AddrInstr = Tmp3AddrMov of tmpArg *  tmp
                   | Tmp3AddrBinop of tmp3AddrBinop
                   | Tmp3AddrReturn of tmpArg
type tmp3AddrProg = tmp3AddrInstr list

(* Inf Address Code (any number of operands on right hand side *)
type tmpExpr = TmpAssemArg of tmpArg
             | TmpInfAddrBinop of tmpInfAddrBinop
and tmpInfAddrBinop = tmpBinop * tmpExpr * tmpExpr *  tmp
type tmpInfAddrInstr = TmpInfAddrMov of tmpExpr * tmpExpr *  tmp
                    | TmpInfAddrReturn of tmpExpr   
type tmpInfAddrProg = tmpInfAddrInstr list

