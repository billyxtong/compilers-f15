(* L2 Compiler
 * Abstract Syntax Trees
 * Authors: Ben Plaut, William Tong
 * 
 * Datatypes for pre-elaboration ASTs, untyped post-elaboration ASTs,
 * and typed post-elaboration ASTs.
 *)

open Datatypesv1

(* Typed Post-Elab AST
   A restriced grammar from the Pre-Elab AST. See the elaboration
   file (which I have not yet written) for more info. *)
type ident = string
type intExpr = IntConst of const | IntIdent of ident
             | ASTBinop of intExpr * intBinop * intExpr
type boolExpr = BoolConst of const | BoolIdent of ident
              | GreaterThan of intExpr * intExpr
              | IntEquals of intExpr * intExpr
              | BoolEquals of boolExpr * boolExpr
              | LogNot of boolExpr
              | LogAnd of boolExpr * boolExpr
type typedPostElabExpr = IntExpr of intExpr | BoolExpr of boolExpr               
type typedPostElabStmt = TypedPostElabDecl of ident * c0type
                  | TypedPostElabAssignStmt of ident * typedPostElabExpr
                  | TypedPostElabIf of boolExpr * typedPostElabAST * 
                                       typedPostElabAST
                  | TypedPostElabWhile of boolExpr * typedPostElabAST
                  | TypedPostElabReturn of intExpr
                  | TypedJumpUncond of label
 and typedPostElabAST = typedPostElabStmt list

(* Untyped Post-Elab AST
   A restricted grammar from the Pre-Elab AST. See the elaboration
   file for more info. *)
type generalBinop = IntBinop of intBinop | DOUBLE_EQ | GT | LOG_AND 
type untypedPostElabExpr = UntypedPostElabConstExpr of const * c0type
                         | UntypedPostElabIdentExpr of ident
                         | UntypedPostElabBinop of untypedPostElabExpr * 
                                                   generalBinop * 
                                                   untypedPostElabExpr
                         | UntypedPostElabNot of untypedPostElabExpr
type untypedPostElabStmt = UntypedPostElabDecl of ident * c0type
                         | UntypedPostElabAssignStmt of ident * 
                                                        untypedPostElabExpr
                         | UntypedPostElabIf of untypedPostElabExpr * 
                                                untypedPostElabAST * 
                                                untypedPostElabAST
                         | UntypedPostElabWhile of untypedPostElabExpr * 
                                                   untypedPostElabAST
                         | UntypedPostElabReturn of untypedPostElabExpr
                         | UntypedPostElabJumpUncond of label
 and untypedPostElabAST = untypedPostElabStmt list

(* Pre-Elab AST *)
type postOp = PLUSPLUS | MINUSMINUS    
type assignOp = EQ | PLUSEQ | SUBEQ | MULEQ | DIVEQ | MODEQ
              | AND_EQ | OR_EQ | XOR_EQ | LSHIFT_EQ | RSHIFT_EQ
type preElabExpr = PreElabConstExpr of const * c0type
                 | PreElabIdentExpr of ident
                 | PreElabBinop of preElabExpr * generalBinop * preElabExpr
                 | PreElabNot of preElabExpr
type preElabDecl = NewVar of ident * c0type
                 | Init of ident * c0type * preElabExpr
type simpStmt = PreElabDecl of preElabDecl                        
              | SimpAssign of ident * preElabExpr
              | SimpStmtExpr of preElabExpr
type simpOpt = EmptySimp | HasSimpStmt of simpStmt
type elseOpt = EmptyElse | PreElabElse of preElabStmt
 and control = PreElabIf of preElabExpr * preElabStmt * elseOpt
             | PreElabWhile of preElabExpr * preElabStmt
             | PreElabFor of simpOpt * preElabExpr * simpOpt *
                             preElabStmt
             | PreElabReturn of preElabExpr
 and preElabStmt = SimpStmt of simpStmt
                 | Control of control
                 | Block of block
 and block = preElabStmt list
type preElabAST = block
