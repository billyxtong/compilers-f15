(* L4 Compiler
 * Abstract Syntax Trees
 * Authors: Ben Plaut, William Tong
 * 
 * Datatypes for pre-elaboration ASTs, untyped post-elaboration ASTs,
 * and typed post-elaboration ASTs.
 *)

open Datatypesv1

(* Typed Post-Elab AST
   A restricted grammar from the Pre-Elab AST. See the elaboration
   file for more info. *)
type shiftOp = ASTrshift | ASTlshift
type param = c0type * ident 
type field = c0type * ident (* for structs: new for L4 *)
(* new in L4: lvalues are no longer just var names *)
type intExpr = IntConst of const | IntIdent of ident
             | ASTBinop of intExpr * intBinop * intExpr
             | IntTernary of boolExpr * intExpr * intExpr
             | BaseCaseShift of intExpr * shiftOp * intExpr
             | IntFunCall of ident * typedPostElabExpr list
 and boolExpr = BoolConst of const | BoolIdent of ident
              | GreaterThan of intExpr * intExpr
              | LessThan of intExpr * intExpr
              | IntEquals of intExpr * intExpr
              | BoolFunCall of ident * typedPostElabExpr list
              | BoolEquals of boolExpr * boolExpr
              | LogNot of boolExpr
              | LogAnd of boolExpr * boolExpr
              | BoolTernary of boolExpr * boolExpr * boolExpr
type memExpr =  TypedPostElabFieldAccess of typedPostElabExpr 
                                          * typedPostElabExpr 
              (* new in L4 *)
              | TypedPostElabPointerAlloc of c0type 
              (* new in L4 *)
              | TypedPostElabPointerDereference of typedPostElabExpr
              (* new in L4 *)
              | TypedPostElabArrayAlloc of c0type * intExpr 
              (* new in L4 
               * Must allocate some integer number of cells *)
              | TypedPostElabArrayAccess of typedPostElabExpr * intExpr
              (* new in L4 
               * Index must be some integer number *)
 and typedPostElabLVal = TypedPostElabVarLVal of ident |
              TypedPostElabFieldLVal of typedPostElabLVal * ident |
              TypedPostElabDerefLVal of typedPostElabLVal |
              TypedPostElabArrayAccessLVal of typedPostElabLVal 
                                            * typedPostElabExpr
(* are these correct? 
 * 1. Having a memExpr type
 * 2. Keeping the memExpr and typedPostElabLVal types more or less identical to 
 *    the analogous untypedAST ones
 * 3. Note: Functions can now return pointers, structs, and arrays, so we need
 *    to expand the typechecking of function calls (more new datatypes?)  *)
and typedPostElabExpr = IntExpr of intExpr
                       | BoolExpr of boolExpr
                       | VoidExpr of typedPostElabStmt (* for void func calls *)
                       | NullExpr (* new in L4: "NULL;" is a valid stmt *)
                       | MemExpr of memExpr
and typedPostElabStmt = TypedPostElabDecl of ident * c0type
                  | TypedPostElabAssignStmt of ident * typedPostElabExpr
                  | TypedPostElabIf of boolExpr * typedPostElabBlock * 
                                       typedPostElabBlock
                  | TypedPostElabWhile of boolExpr * typedPostElabBlock
                  | TypedPostElabReturn of typedPostElabExpr
                  | TypedPostElabAssert of boolExpr
                  | TypedPostElabVoidReturn 
                  | VoidFunCall of ident * typedPostElabExpr list
                  | JumpUncond of label
and typedPostElabBlock = typedPostElabStmt list
type typedPostElabGlobalDecl =
    TypedPostElabFunDef of c0type * ident * param list * typedPostElabBlock
type typedPostElabAST = typedPostElabGlobalDecl list

 (* Untyped Post-Elab AST
   A restricted grammar from the Pre-Elab AST. See the elaboration
   file for more info. *)
type generalBinop = IntBinop of intBinop | DOUBLE_EQ | GT | LOG_AND 
                  | LT | LEQ | GEQ | LOG_OR | NEQ
type untypedPostElabLVal = UntypedPostElabVarLVal of ident |
              UntypedPostElabFieldLVal of untypedPostElabLVal * ident |
              UntypedPostElabDerefLVal of untypedPostElabLVal |
              UntypedPostElabArrayAccessLVal of untypedPostElabLVal 
                                              * untypedPostElabExpr 
(* new in L4: similar to preElab *)
and untypedPostElabExpr =
     UntypedPostElabConstExpr of const * c0type
   | UntypedPostElabNullExpr
   | UntypedPostElabIdentExpr of ident
   | UntypedPostElabBinop of untypedPostElabExpr *
                             generalBinop * untypedPostElabExpr
   | UntypedPostElabNot of untypedPostElabExpr
   | UntypedPostElabTernary of untypedPostElabExpr *
                   untypedPostElabExpr * untypedPostElabExpr
   | UntypedPostElabFunCall of ident * untypedPostElabExpr list
   | UntypedPostElabFieldAccess of untypedPostElabExpr * ident 
                 (* new in L4 
                  * Changed idents to untypedPostElabExprs
                  * because of what you mentioned earlier. -Billy *)
                 | UntypedPostElabPointerAlloc of c0type 
                 (* new in L4 *)
                 | UntypedPostElabPointerDereference of untypedPostElabExpr
                 (* new in L4 *)
                 | UntypedPostElabArrayAlloc of c0type * untypedPostElabExpr 
                 (* new in L4 *)
                 | UntypedPostElabArrayAccess of untypedPostElabExpr 
                                               * untypedPostElabExpr
                 (* new in L4 *)
type untypedPostElabStmt = UntypedPostElabDecl of ident * c0type
      (* Decls are int x, AssignStmts are x = 4, InitDecls are int x = 4.
         We can't elaborate InitDecls to Decl + Assign for the following
         super annoying reason: if "f" is a function, then int f = f()
         is ok, because the var f isn't in scope yet when we
         typecheck the RHS. BUT if we elaborate this to
         "int f; f = f()", then the var is in scope and it will
         throw an typechecking error *)
                         | UntypedPostElabInitDecl of ident * c0type 
                                       * untypedPostElabExpr
                         | UntypedPostElabAssignStmt of untypedPostElabLVal * 
                                                        untypedPostElabExpr
                         | UntypedPostElabIf of untypedPostElabExpr * 
                                                untypedPostElabBlock * 
                                                untypedPostElabBlock
                         | UntypedPostElabWhile of untypedPostElabExpr * 
                                                   untypedPostElabBlock *
                                                   untypedPostElabBlock
                         | UntypedPostElabReturn of untypedPostElabExpr
                         | UntypedPostElabVoidReturn 
                         | UntypedPostElabAssert of untypedPostElabExpr
                         | UntypedPostElabExprStmt of untypedPostElabExpr
                         | UntypedPostElabBlock of untypedPostElabBlock
and untypedPostElabBlock = untypedPostElabStmt list
                             
type untypedPostElabGlobalDecl =
      UntypedPostElabFunDecl of c0type * ident * param list
    | UntypedPostElabFunDef of c0type * ident * param list *
                               untypedPostElabBlock
    | UntypedPostElabTypedef of c0type * ident         
    | UntypedPostElabStructDecl of ident (* new for L4 *)
    | UntypedPostElabStructDef of ident * field list (* new for L4 *)
type untypedPostElabAST = untypedPostElabGlobalDecl list
type untypedPostElabOverallAST = untypedPostElabAST * untypedPostElabAST

(* Pre-Elab AST *)

type postOp = PLUSPLUS | MINUSMINUS    
type assignOp = EQ | PLUSEQ | SUBEQ | MULEQ | DIVEQ | MODEQ
              | AND_EQ | OR_EQ | XOR_EQ | LSHIFT_EQ | RSHIFT_EQ
(* new in L4: lvalues take the place of idents b/c we can now assign to 
 *            pointers, struct fields, and array cells *)
type preElabLVal = PreElabVarLVal of ident | 
              PreElabFieldLVal of preElabLVal * ident |
              PreElabDerefLVal of preElabLVal | 
              PreElabArrayAccessLVal of preElabLVal * preElabExpr 
and preElabExpr = PreElabConstExpr of const * c0type
                 | PreElabNullExpr (* new in L4: represents NULL *)
                 | PreElabIdentExpr of preElabLVal (* new in L4: changed from ident *)
                 | PreElabBinop of preElabExpr * generalBinop * preElabExpr
                 | PreElabNot of preElabExpr
                 | PreElabTernary of preElabExpr * preElabExpr * preElabExpr
                 | PreElabFunCall of ident * preElabExpr list
                 (* new in L4: everything below *)
                 | PreElabFieldAccessExpr of preElabExpr * ident 
                 | PreElabAlloc of c0type 
                 | PreElabDerefExpr of preElabExpr
                 | PreElabArrayAlloc of c0type * preElabExpr 
                 | PreElabArrayAccessExpr of preElabExpr * preElabExpr
type preElabDecl = NewVar of ident * c0type
                 | Init of ident * c0type * preElabExpr
type simpStmt = PreElabDecl of preElabDecl     
              | SimpAssign of preElabLVal * assignOp * preElabExpr
              (* changed in L4: preElabLVal in place of ident, also we need the assignOp now *)
              | SimpStmtExpr of preElabExpr
type simpOpt = EmptySimp | HasSimpStmt of simpStmt
type elseOpt = EmptyElse | PreElabElse of preElabStmt
 and control = PreElabIf of preElabExpr * preElabStmt * elseOpt
             | PreElabWhile of preElabExpr * preElabStmt
             | PreElabFor of simpOpt * preElabExpr * simpOpt * preElabStmt
             | PreElabReturn of preElabExpr
             | PreElabVoidReturn
             | PreElabAssert of preElabExpr
 and preElabStmt = SimpStmt of simpStmt
                 | Control of control
                 | Block of block
 and block = preElabStmt list
type globalDecl = FunDecl of c0type * ident * param list
           | FunDef of c0type * ident * param list * block
           | Typedef of c0type * ident 
           | PreElabStructDecl of ident (* new for L4 *)
           | PreElabStructDef of ident * field list (* new for L4 *)
type preElabAST = globalDecl list
type preElabOverallAST = preElabAST * preElabAST    
