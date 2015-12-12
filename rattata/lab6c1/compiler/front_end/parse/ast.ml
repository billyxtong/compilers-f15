(* L6 Compiler
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
type stringConst = int list
(* strings eventually become char arrays, and chars are represented by ints in ASCII *)
type shiftOp = ASTrshift | ASTlshift
type param = c0type * ident
type field = c0type * ident (* for structs *)
type assignOp = EQ | PLUSEQ | SUBEQ | MULEQ | DIVEQ | MODEQ
              | AND_EQ | OR_EQ | XOR_EQ | LSHIFT_EQ | RSHIFT_EQ
type sharedTypeExpr = Ternary of boolExpr * typedPostElabExpr * typedPostElabExpr
                    | FunCall of ident * typedPostElabExpr list
                    | FuncPointerDeref of ptrExpr * typedPostElabExpr list (* L6: calling func ptr *)
                    | FieldAccess of ident * ptrExpr * ident
                (* FieldAccess here is an arrow! *)
                       (* first ident: type name of struct
                        * ptrExpr: the pointer expression
                        * second ident: actual struct field
                        *
                        * example: p->x where p is a struct s*.
                        * first ident: s
                        * ptrExpr: PtrSharedExpr(Ident(p))
                        * second ident: x
                        *
                        * example: p->q->x, where p is a struct s0*,
                        *                         s0 has a field s1* q,
                        *                         and s1 has a field x.
                        *
                        * first ident: s1
                        * ptrExpr: PtrSharedExpr(FieldAccess(s0, PtrSharedExpr(Ident(p)), q))
                        * second ident: x
                        * *)
                    | ArrayAccess of ptrExpr * intExpr
   (* intExpr is the index, not the mem offset. We can get the mem offset
      because we know the type of the array based on what
                          constructor this is wrapped in *)
                    | Deref of ptrExpr
                    | Ident of ident
and ptrExpr = Null
            | PtrSharedExpr of sharedTypeExpr
            | Alloc of c0type
            | AllocArray of c0type * intExpr
            | AddressOfFunction of ident (* L6: address of operator for function names *)
and intExpr = IntConst of const
            | IntSharedExpr of sharedTypeExpr
            | ASTBinop of intExpr * intBinop * intExpr
(* We need the BaseCaseShift thing, because we turn ASTBinops of shifts
   into and if/else statement that divs by zero if the shift is
   too large, but we need a base case. This, and shiftOp, are like
   the jump_uncond type in that I just added them here because
   I need them in toInfAddr, so don't write print things for
   these *)
            | BaseCaseShift of intExpr * shiftOp * intExpr
 and boolExpr = BoolConst of const
              | BoolSharedExpr of sharedTypeExpr
              | IntGreaterThan of intExpr * intExpr
              | IntLessThan of intExpr * intExpr
              | CharGreaterThan of charExpr * charExpr
              | CharLessThan of charExpr * charExpr
              | IntEquals of intExpr * intExpr
              | BoolEquals of boolExpr * boolExpr
              | CharEquals of charExpr * charExpr
              | PtrEquals of ptrExpr * ptrExpr
              | LogNot of boolExpr
              | LogAnd of boolExpr * boolExpr
 and stringExpr = StringConst of stringConst
                (* strings are transformed into char arrays,
                 * which are just int arrays because ASCII representation is a thing *)
                | StringSharedExpr of sharedTypeExpr
 and charExpr = CharConst of const
              | CharSharedExpr of sharedTypeExpr
 and alphaExpr = AlphaSharedExpr of sharedTypeExpr
and typedPostElabLVal = TypedPostElabVarLVal of ident |
              TypedPostElabFieldLVal of ident * typedPostElabLVal * ident |
              TypedPostElabDerefLVal of typedPostElabLVal |
              TypedPostElabArrayAccessLVal of typedPostElabLVal * intExpr
and typedPostElabExpr = IntExpr of intExpr
                       | BoolExpr of boolExpr
                       | VoidExpr of typedPostElabStmt (* for void function calls ONLY *)
                       | PtrExpr of ptrExpr
                       | StringExpr of stringExpr
                       | CharExpr of charExpr
                       | AlphaExpr of alphaExpr
and typedPostElabStmt = TypedPostElabDecl of ident * c0type
                  | TypedPostElabAssignStmt of typedPostElabLVal *
                                          assignOp * typedPostElabExpr
                  | TypedPostElabIf of boolExpr * typedPostElabBlock * 
                                       typedPostElabBlock
                  | TypedPostElabWhile of boolExpr * typedPostElabBlock
                  | TypedPostElabReturn of typedPostElabExpr
                  | TypedPostElabAssert of boolExpr
                  | TypedPostElabVoidReturn
                  | VoidFunCall of ident * typedPostElabExpr list
                  | JumpUncond of label
and typedPostElabBlock = typedPostElabStmt ref list
type typedPostElabGlobalDecl =
    (* After typechecking, we can throw out function declarations and typedefs *)
    TypedPostElabFunDef of c0type * ident * param list * typedPostElabBlock
  | TypedPostElabStructDef of ident * field list
    (* we need to hang onto struct declarations for a bit *)
type typedPostElabAST = typedPostElabGlobalDecl list      
(* typedAST combines the header ast and main ast into one *)

 (* Untyped Post-Elab AST
   A restricted grammar from the Pre-Elab AST. See the elaboration
   file for more info. *)
type generalBinop = IntBinop of intBinop | DOUBLE_EQ | GT | LOG_AND
                   (* Billy just ignore these ones underneath,
                      I'm just using them for parsing *)
                  | LT | LEQ | GEQ | LOG_OR | NEQ
type untypedPostElabLVal = UntypedPostElabVarLVal of ident |
             (* Field access here is a dot! *)
              UntypedPostElabFieldLVal of untypedPostElabLVal * ident |
              UntypedPostElabDerefLVal of untypedPostElabLVal |
              UntypedPostElabArrayAccessLVal of untypedPostElabLVal * untypedPostElabExpr
and untypedPostElabExpr =
     UntypedPostElabConstExpr of const * c0type (* handles int, bool, char constants *)
   | UntypedPostElabStringConstExpr of stringConst
   | UntypedPostElabNullExpr
   | UntypedPostElabIdentExpr of ident
   | UntypedPostElabBinop of untypedPostElabExpr *
                             generalBinop * untypedPostElabExpr
   | UntypedPostElabNot of untypedPostElabExpr
   | UntypedPostElabTernary of untypedPostElabExpr *
                   untypedPostElabExpr * untypedPostElabExpr
   | UntypedPostElabFunCall of ident * untypedPostElabExpr list
   | UntypedPostElabFunPtrCall of untypedPostElabExpr * untypedPostElabExpr list (* L6: calling func ptr *)
   | UntypedPostElabAddressOfFunction of ident (* L6: address of operator (&) for function names *)
   | UntypedPostElabFieldAccessExpr of untypedPostElabExpr * ident
   | UntypedPostElabAlloc of c0type
   | UntypedPostElabDerefExpr of untypedPostElabExpr
   | UntypedPostElabArrayAlloc of c0type * untypedPostElabExpr
   | UntypedPostElabArrayAccessExpr of untypedPostElabExpr * untypedPostElabExpr
type untypedPostElabStmt = UntypedPostElabDecl of ident * c0type (* L6: type declaration is no longer necessary *)
      (* Decls are int x, AssignStmts are x = 4, InitDecls are int x = 4.
         We can't elaborate InitDecls to Decl + Assign for the following
         super annoying reason: if "f" is a function, then int f = f()
         is ok, because the var f isn't in scope yet when we
         typecheck the RHS. BUT if we elaborate this to
         "int f; f = f()", then the var is in scope and it will
         throw an typechecking error *)
                         | UntypedPostElabInitDecl of ident * c0type
                                       * untypedPostElabExpr (* L6: type declaration is no longer necessary *)
                         | UntypedPostElabAssignStmt of untypedPostElabLVal * 
                                              assignOp * untypedPostElabExpr
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
      UntypedPostElabFunDecl of c0type * ident * param list (* L6: type declaration is no longer necessary *) 
    | UntypedPostElabFunDef of c0type * ident * param list *
                               untypedPostElabBlock (* L6: type declaration is no longer necessary *)
    | UntypedPostElabTypedef of c0type * ident         
    | UntypedPostElabFuncTypedef of c0type * ident * param list
    | UntypedPostElabStructDecl of ident
    | UntypedPostElabStructDef of ident * field list
type untypedPostElabAST = untypedPostElabGlobalDecl list
type untypedPostElabOverallAST = untypedPostElabAST * untypedPostElabAST

(* Pre-Elab AST *)

type postOp = PLUSPLUS | MINUSMINUS    
type preElabLVal = PreElabVarLVal of ident | (* assigning to a var *)
              PreElabFieldLVal of preElabLVal * ident |
              (* assigning to struct.fieldName or struct -> fieldName *)
              PreElabDerefLVal of preElabLVal | (* assigning to ( *pointerName ) *)
              PreElabArrayAccessLVal of preElabLVal * preElabExpr (* assigning to array[index] *)
and preElabExpr = PreElabConstExpr of const * c0type 
                 (* handles int, bool, char constants *)
                 | PreElabStringConstExpr of stringConst (* string literals *)
                 | PreElabNullExpr
                 | PreElabIdentExpr of ident
                 | PreElabBinop of preElabExpr * generalBinop * preElabExpr
                 | PreElabNot of preElabExpr
                 | PreElabTernary of preElabExpr * preElabExpr * preElabExpr
                 | PreElabFunCall of ident * preElabExpr list
                 | PreElabFunPtrCall of preElabExpr * preElabExpr list (* L6: calling a function pointer *)
                 | PreElabAddressOfFunction of ident (* L6: address of operator (&) for function names *)
                 | PreElabFieldAccessExpr of preElabExpr * ident (* field access here is a dot *)
                 | PreElabAlloc of c0type
                 | PreElabDerefExpr of preElabExpr
                 | PreElabArrayAlloc of c0type * preElabExpr
                 | PreElabArrayAccessExpr of preElabExpr * preElabExpr
type preElabDecl = NewVar of ident * c0type (* L6: type declaration is no longer necessary *)
                 | Init of ident * c0type * preElabExpr (* L6: type declaration is no longer necessary *)
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
type globalDecl = FunDecl of c0type * ident * param list (* L6: type declaration is no longer necessary *)
           | FunDef of c0type * ident * param list * block (* L6: type declaration is no longer necessary *)
           | Typedef of c0type * ident
           | FuncTypedef of c0type * ident * param list 
           | PreElabStructDecl of ident
           | PreElabStructDef of ident * field list
type preElabAST = globalDecl list
type preElabOverallAST = preElabAST * preElabAST
