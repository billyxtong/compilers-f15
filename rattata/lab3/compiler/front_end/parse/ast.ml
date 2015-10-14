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
type c0type = INT | BOOL | VOID | TypedefType of ident    
and ident = string
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
              | IntFunCall of ident * typedPostElabExpr list
                       (* the list is the arg list *)
              | BoolEquals of boolExpr * boolExpr
              | LogNot of boolExpr
              | LogAnd of boolExpr * boolExpr
           (* BoolTernary (e1, e2, e3) means
              if e1 then e2 else e3. Similarly for the other
              ternary constructors *)
              | BoolTernary of boolExpr * boolExpr * boolExpr
and typedPostElabExpr = IntExpr of intExpr | BoolExpr of boolExpr
type typedPostElabStmt = TypedPostElabDecl of ident * c0type
                  | TypedPostElabAssignStmt of ident * typedPostElabExpr
                  | TypedPostElabIf of boolExpr * typedPostElabBlock * 
                                       typedPostElabBlock
                  | TypedPostElabWhile of boolExpr * typedPostElabBlock
                  | TypedPostElabReturn of typedPostElabExpr
                      (* functions can now return any type! *)
                  | TypedPostElabAssert of typedPostElabExpr
                  | TypedPostElabVoidReturn (* takes no args *)
                  | JumpUncond of label
and typedPostElabBlock = typedPostElabStmt list
type typedPostElabGlobalDecl =
    (* After typechecking, we can throw out declarations and typedefs *)
    TypedPostElabFunDef of c0type * ident * ident list * typedPostElabBlock
        (* the ident is the function name, the ident list is the params.
           It's not a param list because we don't care about the c0type
           anymore (I think?) *)
                               
(* Note that typedAST doesn't have an "overall" version that contains
   two asts. This is because we can combine the header ast and main ast
   into one, after typechecking *)

 (* Untyped Post-Elab AST
   A restricted grammar from the Pre-Elab AST. See the elaboration
   file for more info. *)
type param = c0type * ident (* do we want a constructor here? *)
type generalBinop = IntBinop of intBinop | DOUBLE_EQ | GT | LOG_AND 
                   (* Billy just ignore these ones underneath,
                      I'm just using them for parsing *)
                  | LT | LEQ | GEQ | LOG_OR | NEQ
type untypedPostElabExpr =
     UntypedPostElabConstExpr of const * c0type
   | UntypedPostElabIdentExpr of ident
   | UntypedPostElabBinop of untypedPostElabExpr *
                             generalBinop * untypedPostElabExpr
   | UntypedPostElabNot of untypedPostElabExpr
   | UntypedPostElabTernary of untypedPostElabExpr *
                   untypedPostElabExpr * untypedPostElabExpr
   | UntypedPostElabFunCall of ident * untypedPostElabExpr list
type untypedPostElabStmt = UntypedPostElabDecl of ident * c0type
                         | UntypedPostElabAssignStmt of ident * 
                                                        untypedPostElabExpr
                         | UntypedPostElabIf of untypedPostElabExpr * 
                                                untypedPostElabBlock * 
                                                untypedPostElabBlock
                         | UntypedPostElabWhile of untypedPostElabExpr * 
                                                   untypedPostElabBlock *
                                                   untypedPostElabBlock
                         | UntypedPostElabReturn of untypedPostElabExpr
                         | UntypedPostElabVoidReturn (* no args *)
                         | UntypedPostElabAssert of untypedPostElabExpr
and untypedPostElabBlock = untypedPostElabStmt list
                             
type untypedPostElabGlobalDecl =
      UntypedPostElabFunDecl of c0type * ident * param list
    | UntypedPostElabFunDef of c0type * ident * param list *
                               untypedPostElabBlock
    | UntypedPostElabTypdef of c0type * ident                               
type untypedPostElabAST = untypedPostElabGlobalDecl list
type untypedPostElabOverallAST = untypedPostElabAST * untypedPostElabAST

(* Pre-Elab AST *)
type postOp = PLUSPLUS | MINUSMINUS    
type assignOp = EQ | PLUSEQ | SUBEQ | MULEQ | DIVEQ | MODEQ
              | AND_EQ | OR_EQ | XOR_EQ | LSHIFT_EQ | RSHIFT_EQ
type preElabExpr = PreElabConstExpr of const * c0type
                 | PreElabIdentExpr of ident
                 | PreElabBinop of preElabExpr * generalBinop * preElabExpr
                 | PreElabNot of preElabExpr
                 | PreElabTernary of preElabExpr * preElabExpr * preElabExpr
                 | PreElabFunCall of ident * preElabExpr list
                       (* the preElabExpr list is the function arguments *)
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
             | PreElabVoidReturn (* this takes no args *)
             | PreElabAssert of preElabExpr
 and preElabStmt = SimpStmt of simpStmt
                 | Control of control
                 | Block of block
 and block = preElabStmt list
type globalDecl = FunDecl of c0type * ident * param list
           | FunDef of c0type * ident * param list * block
           | Typedef of c0type * ident (* old type, and ident
                                          for new type name *)
type preElabAST = globalDecl list
type preElabOverallAST = preElabAST * preElabAST    
