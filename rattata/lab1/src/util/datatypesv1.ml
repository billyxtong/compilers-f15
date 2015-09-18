type c0type = INT | PTR (* will add more eventually *)
 (* the int is the actual value, which we can do because everything
    are ints in C0 (I think?) *)
(* use int or int32? *)              
type const = int * c0type

type reg = EAX | EBX | ECX | EDX | EBP | RSP | ESI | EDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

(* the int is the memory offest from the register *)
type memAddr = reg * int

(* These are for actual assembly instructions. Tmps are not allowed. *)
type assemLoc = Reg of reg | MemAddr of memAddr
type assemArg = AssemLoc of assemLoc | Const of const
type assemBinop = ADD of assemArg * assemLoc | MUL of assemArg * assemLoc
type assemInstr = MOV of assemArg * assemLoc | BINOP of assemBinop
                | RETURN | CDQ | IDIV of assemArg
type assemProg = assemInstr list
    
(* Below here allows tmps, but also allows actual assembly instructions.
   Note: Because we allow actual assembly instructions, memory
   addresses are allowed. Memory addresses should not occur prior
   to register allocation, at least not in L1 (unless I'm missing
   something) *)
type tmp = int
type tmpAssemLoc = Tmp of tmp | AssemLocForTmp of assemLoc
type tmpAssemArg = TmpAssemLoc of tmpAssemLoc | AssemArg of assemArg

(* Two Address Code *)
type tmp2AddrBinop = Tmp2AddrAdd of tmpAssemArg * tmpAssemLoc
                   | Tmp2AddrSub of tmpAssemArg * tmpAssemLoc
                   | Tmp2AddrMul of tmpAssemArg * tmpAssemLoc
                   | Tmp2AddrDiv of tmpAssemArg * tmpAssemLoc
                   | Tmp2AddrMod of tmpAssemArg * tmpAssemLoc    
type tmp2AddrInstr = Tmp2AddrMov of tmpAssemArg * tmpAssemLoc
                   | Tmp2AddrBinop of tmp2AddrBinop
                   | Tmp2AddrReturn of tmpAssemArg
type tmp2AddrProg = tmp2AddrInstr list

(* Two Address Code with wonky instructions (i.e. idiv, etc) *)
(* This comes after 2Addr in the pipeline, but needs to be below
   here so that they can refer to 2AddrInstrs (since this is a
   strict superset of normal 2Addr *)
type tmpWonkyInstr = Tmp2AddrInstr of tmp2AddrInstr
                   | TmpCDQ (* needed for idiv *)
                   | TmpIDIV of tmpAssemArg
type tmpWonkyProg = tmpWonkyInstr list

(* Three Address Code *)
type tmp3AddrBinop = Tmp3AddrAdd of tmpAssemArg * tmpAssemArg *  tmpAssemLoc
                   | Tmp3AddrMul of tmpAssemArg * tmpAssemArg *  tmpAssemLoc
type tmp3AddrInstr = Tmp3AddrMov of tmpAssemArg *  tmpAssemLoc
                   | Tmp3AddrBinop of tmp3AddrBinop
                   | Tmp3AddrReturn of tmpAssemArg
type tmp3AddrProg = tmp3AddrInstr list

(* Inf Address Code (any number of operands on right hand side *)
type tmpExpr = TmpAssemArg of tmpAssemArg
             | TmpInfAddrBinop of tmpInfAddrBinop
and tmpInfAddrBinop = TmpInfAddrAdd of tmpExpr * tmpExpr *  tmpAssemLoc
                    | TmpInfAddrMul of tmpExpr * tmpExpr *  tmpAssemLoc
type tmpInfAddrInstr = TmpInfAddrMov of tmpExpr * tmpExpr *  tmpAssemLoc
                    | TmpInfAddrReturn of tmpExpr   
type tmpInfAddrProg = tmpInfAddrInstr list

(* Post-Elab AST
   A restriced grammar from the Pre-Elab AST. See the elaboration
   file (which I have not yet written) for more info. *)
type ident = string * c0type
type expr = ConstExpr of const | Ident of ident | ASTbinop of expr * astBinop * expr
and assignStmt = ident * expr 
and astBinop = ASTplus | ASTminus | ASTtimes | ASTdiv | ASTmod
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
                 | PreElabBinop of preElabExpr * astBinop * preElabExpr
                 | UnaryMinus of preElabExpr
type preElabStmt = PreElabDecl of ident | SimpAssign of assignStmt
                 | PreElabReturn of preElabExpr
type preElabAST = preElabStmt list
