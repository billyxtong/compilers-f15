type c0type = INT | PTR (* will add more eventually *)
type const = Int of int

type reg = RAX | RBX | RCX | RDX | RBP | RSP | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

(* the int is the memory offest from the register *)
type memAddr = reg * int

(* These are for actual assembly instructions. Tmps are not allowed. *)
type assemArg = Reg of reg | MemAddr of memAddr | Const of const
type assemBinop = ADD of assemArg * assemArg | MUL of assemArg * assemArg
type assemInstr = MOV of assemArg * assemArg | BINOP of assemBinop | RETURN

(* Below here allows tmps, but also allows actual assembly instructions.
   Note: Because we allow actual assembly instructions, memory
   addresses are allowed. Memory addresses should not occur prior
   to register allocation, at least not in L1 (unless I'm missing
   something) *)
type tmp = int
type tmpAssemArg = Tmp of tmp | AssemArg of assemArg

(* For Two Address Code *)
type tmp2AddrBinop = Tmp3AddrAdd of tmpAssemArg * tmpAssemArg
                   | Tmp3AddrMul of tmpAssemArg * tmpAssemArg
type tmp2AddrInstr = Tmp2AddrMov of tmpAssemArg * tmpAssemArg
                   | Tmp2AddrBinop of tmp2AddrBinop
                   | Tmp2AddrReturn of tmpAssemArg

(* For Three Address Code *)
type tmp3AddrBinop = Tmp3AddrAdd of tmpAssemArg * tmpAssemArg
                   | Tmp3AddrMul of tmpAssemArg * tmpAssemArg
type tmp3AddrInstr = Tmp3AddrMov of tmpAssemArg * tmpAssemArg
                   | Tmp3AddrBinop of tmp3AddrBinop
                   | Tmp3AddrReturn of tmpAssemArg

(* TODO For Inf Address Code (any number of operands on right hand side *)

(* Post-Elab AST
   A restriced grammar from the Pre-Elab AST. See the elaboration
   file (which I have not yet written) for more info. *)
type ident = string * c0type
type expr = ConstExpr
and assignSmt = ident * expr | Ident of ident | ASTbinop of expr * astBinop * expr
and astBinop = ASTplus | ASTminus | ASTtimes | ASTdiv | ASTmod
type stmt = Decl of ident | AssignStmt of assignStmt
type elabAST = stmt list

(* Pre-Elab AST *)
    (*
type stmt = Decl of ident | AssignStmt of assignStmt
type preElabAST = stmt list
*)
