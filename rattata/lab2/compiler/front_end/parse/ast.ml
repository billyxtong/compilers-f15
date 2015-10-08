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
type shiftOp = ASTrshift | ASTlshift
type ident = string
(* intExpr and boolExpr have to be mutually recursive because
   of damn ternary operators *)
type intExpr = IntConst of const | IntIdent of ident
             | ASTBinop of intExpr * intBinop * intExpr
             | IntTernary of boolExpr * intExpr * intExpr
(* We need the BaseCaseShift thing, because we turn ASTBinops of shifts
   into and if/else statement that divs by zero if the shift is
   too large, but we need a base case. This, and shiftOp, are like
   the jump_uncond type in that I just added them here because
   I need them in toInfAddr, so don't write print things for
   these *)
             | BaseCaseShift of intExpr * shiftOp * intExpr
 and boolExpr = BoolConst of const | BoolIdent of ident
              | GreaterThan of intExpr * intExpr
              | LessThan of intExpr * intExpr
              | IntEquals of intExpr * intExpr
              | BoolEquals of boolExpr * boolExpr
              | LogNot of boolExpr
              | LogAnd of boolExpr * boolExpr
           (* BoolTernary (e1, e2, e3) means
              if e1 then e2 else e3. Similarly for the other
              ternary constructors *)
              | BoolTernary of boolExpr * boolExpr * boolExpr
type typedPostElabExpr = IntExpr of intExpr | BoolExpr of boolExpr     
type typedPostElabStmt = TypedPostElabDecl of ident * c0type
                  | TypedPostElabAssignStmt of ident * typedPostElabExpr
                  | TypedPostElabIf of boolExpr * typedPostElabAST * 
                                       typedPostElabAST
                  | TypedPostElabWhile of boolExpr * typedPostElabAST
                  | TypedPostElabReturn of intExpr
                  | JumpUncond of label
 and typedPostElabAST = typedPostElabStmt list

 (* Untyped Post-Elab AST
   A restricted grammar from the Pre-Elab AST. See the elaboration
   file for more info. *)
type generalBinop = IntBinop of intBinop | DOUBLE_EQ | GT | LOG_AND 
                   (* Billy just ignore these ones underneath,
                      I'm just using them for parsing *)
                  | LT | LEQ | GEQ | LOG_OR | NEQ
type untypedPostElabExpr = UntypedPostElabConstExpr of const * c0type
                         | UntypedPostElabIdentExpr of ident
                         | UntypedPostElabBinop of untypedPostElabExpr *
                                                   generalBinop *
                                                   untypedPostElabExpr
                         | UntypedPostElabNot of untypedPostElabExpr
                         | UntypedPostElabTernary of untypedPostElabExpr *
                            untypedPostElabExpr * untypedPostElabExpr
type untypedPostElabStmt = UntypedPostElabDecl of ident * c0type
                         | UntypedPostElabAssignStmt of ident * 
                                                        untypedPostElabExpr
                         | UntypedPostElabIf of untypedPostElabExpr * 
                                                untypedPostElabAST * 
                                                untypedPostElabAST
                         | UntypedPostElabWhile of untypedPostElabExpr * 
                                                   untypedPostElabAST *
                                                   untypedPostElabAST
                         | UntypedPostElabReturn of untypedPostElabExpr
 and untypedPostElabAST = untypedPostElabStmt list

(* Pre-Elab AST *)
type postOp = PLUSPLUS | MINUSMINUS    
type assignOp = EQ | PLUSEQ | SUBEQ | MULEQ | DIVEQ | MODEQ
              | AND_EQ | OR_EQ | XOR_EQ | LSHIFT_EQ | RSHIFT_EQ
type preElabExpr = PreElabConstExpr of const * c0type
                 | PreElabIdentExpr of ident
                 | PreElabBinop of preElabExpr * generalBinop * preElabExpr
                 | PreElabNot of preElabExpr
                 | PreElabTernary of preElabExpr * preElabExpr * preElabExpr
type preElabDecl = NewVar of ident * c0type
                 | Init of ident * c0type * preElabExpr
type simpStmt = PreElabDecl of preElabDecl                        
              | SimpAssign of ident * preElabExpr
              | SimpStmtExpr of preElabExpr
type simpOpt = EmptySimp | HasSimpStmt of simpStmt
type elseOpt = EmptyElse | PreElabElse of preElabStmt
 and control = PreElabIf of preElabExpr * preElabStmt * elseOpt
             | PreElabWhile of preElabExpr * preElabStmt
             | PreElabFor of simpOpt * preElabExpr * simpOpt * preElabStmt
             | PreElabReturn of preElabExpr
 and preElabStmt = SimpStmt of simpStmt
                 | Control of control
                 | Block of block
 and block = preElabStmt list
type preElabAST = block
