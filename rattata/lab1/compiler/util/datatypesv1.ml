type c0type = INT | PTR (* will add more eventually *)
(* everything in c0 is an int! *)              
type const = int

type reg = EAX | EBX | ECX | EDX | EBP | RSP | ESI | EDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

(* the int is the memory offest from the register *)
type memAddr = reg * int

(* These are for actual assembly instructions. Tmps are not allowed. *)
type binop = ADD | MUL | SUB | SUBQ | FAKEDIV | FAKEMOD
type assemLoc = Reg of reg | MemAddr of memAddr
type assemArg = AssemLoc of assemLoc | Const of const
type assemBinopInstr = binop * assemArg * assemLoc
type assemInstr = MOV of assemArg * assemLoc
                | BINOP of assemBinopInstr
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

(* Post-Elab AST
   A restriced grammar from the Pre-Elab AST. See the elaboration
   file (which I have not yet written) for more info. *)
type ident = string * c0type
type expr = ConstExpr of const | Ident of ident | ASTBinop of expr * tmpBinop * expr
and assignStmt = ident * expr 
type stmt = Decl of ident | AssignStmt of assignStmt | Return of expr
type elabAST = stmt list

(* Pre-Elab AST
   Unfortunately, we have to wrap everything in different
   constructors here, in order to keep in separate from
   Post-Elab AST *)
type leftHandIdent = Ident of ident | ParenWrapIdent of leftHandIdent
type assignOp = EQ | PLUSEQ | SUBEQ | MULEQ | DIVEQ | MODEQ
type preElabExpr = ParenWrapExpr of expr | PreElabConstExpr of const
                 | IdentExpr of ident
                 | PreElabBinop of preElabExpr * tmpBinop * preElabExpr
                 | UnaryMinus of preElabExpr
type preElabStmt = PreElabDecl of ident | SimpAssign of assignStmt
                 | PreElabReturn of preElabExpr
type preElabAST = preElabStmt list
