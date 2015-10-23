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
type param = c0type * ident (* do we want a constructor here? *)
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
             | IntFunCall of ident * typedPostElabExpr list
 and boolExpr = BoolConst of const | BoolIdent of ident
              | GreaterThan of intExpr * intExpr
              | LessThan of intExpr * intExpr
              | IntEquals of intExpr * intExpr
              | BoolFunCall of ident * typedPostElabExpr list
                       (* the list is the arg list *)
              | BoolEquals of boolExpr * boolExpr
              | LogNot of boolExpr
              | LogAnd of boolExpr * boolExpr
           (* BoolTernary (e1, e2, e3) means
              if e1 then e2 else e3. Similarly for the other
              ternary constructors *)
              | BoolTernary of boolExpr * boolExpr * boolExpr
and typedPostElabExpr = IntExpr of intExpr | 
                        BoolExpr of boolExpr | 
                        VoidExpr of typedPostElabStmt (* for void function calls ONLY *)
and typedPostElabStmt = TypedPostElabDecl of ident * c0type
                  | TypedPostElabAssignStmt of ident * typedPostElabExpr
                  | TypedPostElabIf of boolExpr * typedPostElabBlock * 
                                       typedPostElabBlock
                  | TypedPostElabWhile of boolExpr * typedPostElabBlock
                  | TypedPostElabReturn of typedPostElabExpr
                      (* functions can now return any type! *)
                  | TypedPostElabAssert of boolExpr
                  | TypedPostElabVoidReturn (* takes no args *)
                  | VoidFunCall of ident * typedPostElabExpr list
                  | JumpUncond of label
and typedPostElabBlock = typedPostElabStmt list
type typedPostElabGlobalDecl =
    (* After typechecking, we can throw out declarations and typedefs *)
    TypedPostElabFunDef of c0type * ident * param list * typedPostElabBlock
        (* the ident is the function name, the ident list is the params.
           It's not a param list because we don't care about the c0type
           anymore (I think?) *)
type typedPostElabAST = typedPostElabGlobalDecl list      
(* Note that typedAST doesn't have an "overall" version that contains
   two asts. This is because we can combine the header ast and main ast
   into one, after typechecking *)

 (* Untyped Post-Elab AST
   A restricted grammar from the Pre-Elab AST. See the elaboration
   file for more info. *)
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
      (* Decls are int x, AssignStmts are x = 4, InitDecls are int x = 4.
         We can't elaborate InitDecls to Decl + Assign for the following
         super annoying reason: if "f" is a function, then int f = f()
         is ok, because the var f isn't in scope yet when we
         typecheck the RHS. BUT if we elaborate this to
         "int f; f = f()", then the var is in scope and it will
         throw an typechecking error *)
                         | UntypedPostElabInitDecl of ident * c0type 
                                       * untypedPostElabExpr
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
                         | UntypedPostElabExprStmt of untypedPostElabExpr
                         | UntypedPostElabBlock of untypedPostElabBlock
and untypedPostElabBlock = untypedPostElabStmt list
                             
type untypedPostElabGlobalDecl =
      UntypedPostElabFunDecl of c0type * ident * param list
    | UntypedPostElabFunDef of c0type * ident * param list *
                               untypedPostElabBlock
    | UntypedPostElabTypedef of c0type * ident                               
type untypedPostElabAST = untypedPostElabGlobalDecl list
type untypedPostElabOverallAST = untypedPostElabAST * untypedPostElabAST

(* Pre-Elab AST *)

(* new in L4: lvalues are no longer just var names *)
type lvalue = Var of ident | (* when we're assigning to a var *)
              Field of lvalue * ident | (* handles both struct.fieldName and struct -> fieldName *)
              Dereference of lvalue | (* handles ( *pointerName ) *)
              ArrayAccess of lvalue * preElabExpr (* handles array[index] *)
              (* assuming you're stripping away of parens in parsing? 
               * so (lvalue) doesn't need to be a type *)
type postOp = PLUSPLUS | MINUSMINUS    
type assignOp = EQ | PLUSEQ | SUBEQ | MULEQ | DIVEQ | MODEQ
              | AND_EQ | OR_EQ | XOR_EQ | LSHIFT_EQ | RSHIFT_EQ
type preElabExpr = PreElabConstExpr of const * c0type
                 | PreElabNullExpr (* new in L4: represents NULL *)
                 | PreElabIdentExpr of ident
                 | PreElabBinop of preElabExpr * generalBinop * preElabExpr
                 | PreElabNot of preElabExpr
                 | PreElabTernary of preElabExpr * preElabExpr * preElabExpr
                 | PreElabFunCall of ident * preElabExpr list
                 (* new in L4: everything below. Replaced a lot of "exp" with idents. Is that correct? *)
                 | PreElabFieldAccess of ident * ident 
                 (* new in L4: handles exp.ident and exp -> ident*)
                 | PreElabPointerAlloc of c0type 
                 (* new in L4: allocates one pointer of type c0type *)
                 | PreElabPointerDereference of ident
                 (* new in L4: dereferences ident. assuming your parsing makes it unambiguous... *)
                 | PreElabArrayAlloc of c0type * preElabExpr 
                 (* new in L4: allocates an array*)
                 | PreElabArrayAccess of ident * preElabExpr
                 (* new in L4: accesses array of name ident at index preElabExpr *) 
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
             | PreElabVoidReturn
             | PreElabAssert of preElabExpr
 and preElabStmt = SimpStmt of simpStmt
                 | Control of control
                 | Block of block
 and block = preElabStmt list
type field = Field of c0type * ident (* new for L4 *)
type globalDecl = FunDecl of c0type * ident * param list
           | FunDef of c0type * ident * param list * block
           | Typedef of c0type * ident 
           | StructDecl of ident (* new for L4 *)
           | StructDef of ident * field list (* new for L4 *)
type preElabAST = globalDecl list
type preElabOverallAST = preElabAST * preElabAST    
