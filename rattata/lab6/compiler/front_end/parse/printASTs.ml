open Datatypesv1
open PrintDatatypes
open Ast
open String

(* ============ Pre-Elab AST Print Functions ================= *)


let generalBinopToString(op : generalBinop) = 
  match op with
     (* I want mul to be printed differently so that I know that
        I'm handling the x*y parsing correctly *)
        IntBinop(MUL) -> " mul "
      | IntBinop(i) -> intBinopToString(i)
      | DOUBLE_EQ -> " == "
      | GT -> " > "
      | LT -> " < "
      | LOG_AND -> " && "
      | _ -> assert(false)

let rec preElabLValToString(lval : preElabLVal) =
  match lval with
        PreElabVarLVal(i) -> identToString(i)
      | PreElabFieldLVal(p,i) -> 
          (match p with
                 PreElabDerefLVal(inner) -> "*(" ^ preElabLValToString(inner) ^ ")." ^ identToString(i)
               | _ -> preElabLValToString(p) ^ "." ^ identToString(i))
      | PreElabDerefLVal(p) -> "*(" ^ preElabLValToString(p) ^ ")"
      | PreElabArrayAccessLVal(p,e) -> preElabLValToString(p) ^ "[" ^ preElabExprToString(e) ^ "]"


and preElabExprToString(preelabexpr : preElabExpr) =
  match preelabexpr with
        PreElabConstExpr(c,t) -> constToString c
      | PreElabNullExpr -> "NULL"
      | PreElabIdentExpr(id) -> identToString id
      | PreElabBinop(expr1, op, expr2) -> concat "" ["("; preElabExprToString expr1; " ";
                                                     generalBinopToString op; 
                                                     preElabExprToString expr2; ")"]
      | PreElabNot(expr1) -> concat "" ["!"; preElabExprToString(expr1)]
      | PreElabTernary(e1, e2, e3) -> "(" ^ (preElabExprToString e1) ^ " ? " ^
              (preElabExprToString e2) ^ " : " ^ (preElabExprToString e3) ^ ")"
      | PreElabFunCall(func, args) -> identToString(func) ^ "(" ^ (concat ", " (List.map preElabExprToString args)) ^ ")"
      | PreElabFieldAccessExpr(e,i) -> 
          (match e with
                 PreElabDerefExpr(inner) -> "*(" ^ preElabExprToString(inner) ^ ")." ^ identToString(i)
               | _ -> preElabExprToString(e) ^ "." ^ identToString(i))
      | PreElabAlloc(t) -> "alloc(" ^ c0typeToString(t) ^ ")"
      | PreElabDerefExpr(e) -> "*(" ^ preElabExprToString(e) ^ ")"
      | PreElabArrayAlloc(t,e) -> "alloc_array(" ^ c0typeToString(t) ^ ", " ^ preElabExprToString(e) ^ ")"
      | PreElabArrayAccessExpr(e1,e2) -> preElabExprToString(e1) ^ "[" ^ preElabExprToString(e2) ^ "]"

let preElabDeclToString(preelabdecl : preElabDecl) =
  match preelabdecl with
        NewVar(i,t) -> c0typeToString(t) ^ identToString(i)
      | Init(i,t,p) -> concat "" [c0typeToString(t); identToString(i); " = "; preElabExprToString(p)]

let assignOpToString(op : assignOp) =
  match op with
        EQ -> " = "
     |  PLUSEQ -> " += "
     |  SUBEQ -> " -= "
     |  MULEQ -> " *= "
     |  DIVEQ -> " /= "
     |  MODEQ -> " %= "
     |  AND_EQ -> " &= "
     |  OR_EQ -> " |= "
     |  XOR_EQ -> " ^= "
     |  LSHIFT_EQ -> " <<= "
     |  RSHIFT_EQ -> " >>= "


let simpStmtToString(statement : simpStmt) =
  match statement with
        PreElabDecl(p) -> preElabDeclToString(p)
      | SimpAssign(l,a,e) -> concat "" [preElabLValToString(l); assignOpToString(a); preElabExprToString(e)]
      | SimpStmtExpr(p) -> preElabExprToString(p)

let simpOptToString(sOpt : simpOpt) = 
  match sOpt with
        EmptySimp -> ""
      | HasSimpStmt(statement) -> simpStmtToString(statement)

let rec elseOptToString(eOpt : elseOpt) =
  match eOpt with
        EmptyElse -> ""
      | PreElabElse(p) -> preElabStmtToString(p)

and controlToString(c : control) = 
  match c with
        PreElabIf(pExpr,pStmt,eOpt) -> concat "" ["if("; preElabExprToString(pExpr); 
                                                  ") {\n  "; preElabStmtToString(pStmt); 
                                                  "} \nelse {\n  "; elseOptToString(eOpt); "\n}"]
      | PreElabWhile(pExpr,pStmt) -> concat "" ["while("; preElabExprToString(pExpr);
                                                ") {\n  "; preElabStmtToString(pStmt); "\n}"]
      | PreElabFor(sOpt1,pExpr,sOpt2,pStmt) -> concat "" ["for("; simpOptToString(sOpt1); "; ";
                                                        preElabExprToString(pExpr); "; ";
                                                        simpOptToString(sOpt2); ") {\n  ";
                                                        preElabStmtToString(pStmt); "\n}"]
      | PreElabReturn(pExpr) -> "return " ^ preElabExprToString(pExpr)
      | PreElabVoidReturn -> "return"
      | PreElabAssert(pExpr) -> "assert(" ^ preElabExprToString(pExpr) ^ ")"

and preElabStmtToString(pStmt : preElabStmt) = 
  match pStmt with
        SimpStmt(s) -> simpStmtToString(s)
      | Control(c) -> controlToString(c)
      | Block(b) -> blockToString(b)

and blockToString(blk : block) = concat "\n  " (List.map preElabStmtToString blk) ^ "\n" 

let paramToString(c, i) = c0typeToString(c) ^ identToString(i)

let fieldToString(c, i) = "  " ^ c0typeToString(c) ^ identToString(i) ^ ";\n"

let globalDeclToString(g : globalDecl) =
  match g with
        FunDecl(c, i, params) -> c0typeToString(c) ^ identToString(i) ^ "(" ^ 
                                 (concat ", " (List.map paramToString params)) 
                                 ^ ")"
      | FunDef(c, i, params, stmts) -> 
          c0typeToString(c) ^ identToString(i) ^ "(" ^ 
          (concat ", " (List.map paramToString params)) ^ ") {\n" ^ 
          blockToString(stmts) ^ "}"
      | Typedef(c, i) -> "typedef " ^ c0typeToString(c) ^ identToString(i) ^ ";"
      | PreElabStructDecl(i) -> "struct " ^ identToString(i) ^ ";"
      | PreElabStructDef(i, fs) -> "struct " ^ identToString(i) ^ "{\n" ^ 
                                   (concat "" (List.map fieldToString fs)) ^ "};"

let preElabASTToString (mainDecls, headerDecls) =
    "MAIN:\n\n" ^ (concat "\n" (List.map globalDeclToString mainDecls) ^ "\n\n")
    ^ "HEADER:\n" ^ (concat "\n" (List.map globalDeclToString headerDecls) ^ "\n")

(* ============ Untyped Post-Elab AST Print Functions ================= *)

let rec untypedPostElabLValToString(lval : untypedPostElabLVal) =
  match lval with
        UntypedPostElabVarLVal(i) -> identToString(i)
      | UntypedPostElabFieldLVal(p,i) -> "(" ^ untypedPostElabLValToString(p) ^ ")."  
                                            ^ identToString(i)
      | UntypedPostElabDerefLVal(p) -> "*" ^ untypedPostElabLValToString(p)
      | UntypedPostElabArrayAccessLVal(p,e) -> untypedPostElabLValToString(p)
                               ^ "[" ^ untypedPostElabExprToString(e) ^ "]"


and untypedPostElabExprToString(expression : untypedPostElabExpr) =
  match expression with
        UntypedPostElabConstExpr(c,t) -> constToString c
      | UntypedPostElabNullExpr -> "NULL"
      | UntypedPostElabIdentExpr(id) -> id
      | UntypedPostElabBinop(expr1, op, expr2) -> concat "" ["("; 
                          untypedPostElabExprToString expr1; " ";
                          generalBinopToString op;
                          untypedPostElabExprToString expr2; ")"]
      | UntypedPostElabNot(expr1) -> concat "" ["!"; 
                untypedPostElabExprToString(expr1)]
      | UntypedPostElabTernary(e1, e2, e3) -> "(" ^ 
              (untypedPostElabExprToString e1) ^ " ? " ^
              (untypedPostElabExprToString e2) ^ " : " ^ 
              (untypedPostElabExprToString e3) ^ ")" 
      | UntypedPostElabFunCall(func, args) -> 
          identToString(func) ^ "(" ^ (concat ", " 
                (List.map untypedPostElabExprToString args)) ^ ")"
      | UntypedPostElabFieldAccessExpr(e,i) -> untypedPostElabExprToString(e) 
                                                  ^ "." ^ identToString(i)
      | UntypedPostElabAlloc(t) -> "alloc(" ^ c0typeToString(t) ^ ")"
      | UntypedPostElabDerefExpr(e) -> "*(" ^ untypedPostElabExprToString(e) ^ ")"
      | UntypedPostElabArrayAlloc(t,e) -> "alloc_array(" ^ c0typeToString(t) 
                                ^ ", " ^ untypedPostElabExprToString(e) ^ ")"
      | UntypedPostElabArrayAccessExpr(e1,e2) -> untypedPostElabExprToString(e1)
                              ^ "[" ^ untypedPostElabExprToString(e2) ^ "]"

let rec untypedPostElabStmtToString(s : untypedPostElabStmt) =
  match s with
        UntypedPostElabDecl(identifier,constant) -> concat "" 
              [c0typeToString(constant); identToString(identifier)]
      | UntypedPostElabInitDecl(identifier,constant, e) -> concat "" 
                    [c0typeToString(constant); 
                     identToString(identifier); " = "; 
                     untypedPostElabExprToString e]
      | UntypedPostElabAssignStmt(lval, op, untypedexpr) -> concat "" 
                [untypedPostElabLValToString(lval); assignOpToString(op);
                 untypedPostElabExprToString(untypedexpr)]
      | UntypedPostElabIf(expression,postelabast1,postelabast2) -> concat "" ["if("; untypedPostElabExprToString(expression);
                                                  ") {\n  "; untypedPostElabBlockToString(postelabast1);
                                                  "} \nelse {\n  "; untypedPostElabBlockToString(postelabast2); "\n}"]
      | UntypedPostElabWhile(expression,postelabast,init) -> concat "" ["while("; untypedPostElabExprToString(expression);
                                                ", "; untypedPostElabBlockToString(init); ") {\n  ";
                                                      untypedPostElabBlockToString(postelabast); "\n}"]
      | UntypedPostElabReturn(i) -> "return " ^ untypedPostElabExprToString(i)
      | UntypedPostElabBlock(blockAst) -> "  " ^ untypedPostElabBlockToString blockAst
      | UntypedPostElabAssert(pExpr) -> "assert(" ^ untypedPostElabExprToString(pExpr) ^ ")"
      | UntypedPostElabVoidReturn -> "return"
      | UntypedPostElabExprStmt(pExpr) -> untypedPostElabExprToString(pExpr) ^ "\n"

                                          
and untypedPostElabBlockToString(stmts : untypedPostElabBlock) =
  concat "\n  " (List.map untypedPostElabStmtToString stmts) ^ "\n"

let untypedPostElabGlobalDeclToString(g : untypedPostElabGlobalDecl) =
  match g with
        UntypedPostElabFunDecl(c, i, params) -> c0typeToString(c) ^ identToString(i) ^ "(" ^ 
                                 (concat ", " (List.map paramToString params)) 
                                 ^ ")"
      | UntypedPostElabFunDef(c, i, params, stmts) -> 
          c0typeToString(c) ^ identToString(i) ^ "(" ^ 
          (concat ", " (List.map paramToString params)) ^ ") {\n" ^ 
          untypedPostElabBlockToString(stmts) ^ "}"
      | UntypedPostElabTypedef(c, i) -> "typedef " ^ c0typeToString(c) ^ identToString(i) ^ ";"
      | UntypedPostElabStructDecl(i) -> "struct " ^ identToString(i) ^ ";"
      | UntypedPostElabStructDef(i, fs) -> "struct " ^ identToString(i) ^ "{\n  " ^ 
                                   (concat "  " (List.map fieldToString fs)) ^ "};"

let untypedPostElabOverallASTToString (mainDecls, headerDecls) =
    "MAIN:\n" ^ (concat "\n" (List.map untypedPostElabGlobalDeclToString mainDecls) ^ "\n\n")
    ^ "HEADER:\n" ^ (concat "\n" (List.map untypedPostElabGlobalDeclToString headerDecls) ^ "\n")

(* ============ Typed Post-Elab AST Print Functions ================= *)

let shiftOpToString(s : shiftOp) =
  match s with
        ASTrshift -> " >> "
      | ASTlshift -> " << "

let rec sharedTypeExprToString(s : sharedTypeExpr) =
  match s with
        Ternary(b,e1,e2) -> concat "" [boolExprToString(b); " ? ";
                                       typedPostElabExprToString(e1); " : ";
                                       typedPostElabExprToString(e2)]
      | FunCall(func,args) -> identToString(func) ^ "(" ^ (concat ", " 
                      (List.map typedPostElabExprToString args)) ^ ")"
      | FieldAccess(i1,p,i2) -> ptrExprToString(p) ^ "." ^ identToString(i2) (*^
                                  " (type = " ^ identToString(i2) ^ ")"*)
      | ArrayAccess(p,i) -> ptrExprToString(p) ^ "[" ^ intExprToString(i) ^ "]"
      | Deref(p) -> "*(" ^ ptrExprToString(p) ^ ")"
      | Ident(i) -> identToString(i)
      
and ptrExprToString(p : ptrExpr) =
  match p with
        Null -> "NULL"
      | PtrSharedExpr(s) -> sharedTypeExprToString(s)
      | Alloc(c) -> "alloc(" ^ c0typeToString(c) ^ ")"
      | AllocArray(c,i) -> "alloc_array(" ^ c0typeToString(c) ^ ", " ^
                                      intExprToString(i) ^ ")"

and intExprToString(iExpr : intExpr) =
  match iExpr with
        IntConst(c) -> constToString(c)
      | IntSharedExpr(s) -> sharedTypeExprToString(s)
      | ASTBinop(expr1, op, expr2) -> concat "" ["("; intExprToString(expr1);
                                                      intBinopToString(op);
                                                      intExprToString(expr2);
                                                 ")"]
      
and boolExprToString(bExpr : boolExpr) =
  match bExpr with
        BoolConst(c) -> if c = 0 then "false" else "true"
      | BoolSharedExpr(s) -> sharedTypeExprToString(s)
      | GreaterThan(iExpr1,iExpr2) -> concat "" [intExprToString(iExpr1); " > "; intExprToString(iExpr2)]
      | LessThan(iExpr1,iExpr2) -> concat "" [intExprToString(iExpr1); " < "; intExprToString(iExpr2)]
      | IntEquals(iExpr1,iExpr2) -> concat "" [intExprToString(iExpr1); " == "; intExprToString(iExpr2)]
      | BoolEquals(bExpr1,bExpr2) -> concat "" [boolExprToString(bExpr1); " == "; boolExprToString(bExpr2)]
      | PtrEquals(pExpr1,pExpr2) -> concat "" [ptrExprToString(pExpr1); " == "; ptrExprToString(pExpr2)]
      | LogNot(bExpr) -> concat "" ["!"; boolExprToString bExpr]
      | LogAnd(bExpr1,bExpr2) -> concat "" [boolExprToString(bExpr1); " && "; boolExprToString(bExpr2)]
      
and typedPostElabLValToString(lval : typedPostElabLVal) =
  match lval with
        TypedPostElabVarLVal(i) -> identToString(i)
      | TypedPostElabFieldLVal(i1, p,i2) -> typedPostElabLValToString(p) ^ "."  
                                            ^ identToString(i2) ^ "("
                                            ^ identToString(i1) ^ ")"
      | TypedPostElabDerefLVal(p) -> "*" ^ typedPostElabLValToString(p)
      | TypedPostElabArrayAccessLVal(p,e) -> typedPostElabLValToString(p)
                               ^ "[" ^ intExprToString(e) ^ "]"

and typedPostElabExprToString(e : typedPostElabExpr) =
  match e with
        IntExpr(i) -> intExprToString(i)
      | BoolExpr(b) -> boolExprToString(b)
      | VoidExpr(stmt) -> typedPostElabStmtToString(stmt)
      | PtrExpr(p) -> ptrExprToString(p)

and typedPostElabStmtToString(s : typedPostElabStmt) =
  match s with
        TypedPostElabDecl(i,c) -> concat "" [c0typeToString(c); identToString(i)]
      | TypedPostElabAssignStmt(lval, op, e) -> concat "" [typedPostElabLValToString(lval); assignOpToString(op);
                                                    typedPostElabExprToString(e)]
      | TypedPostElabIf(b,p1,p2) -> concat "" ["if(";
                                               boolExprToString(b);
                                               ") {\n  ";
                                               typedPostElabBlockToString(p1);
                                               "} else {\n  ";
                                               typedPostElabBlockToString(p2); "\n}"]
      | TypedPostElabWhile(b,p) -> concat "" ["while("; boolExprToString(b);
                                                ") {\n  "; typedPostElabBlockToString(p); "\n}"]
      | TypedPostElabReturn(i) -> "return " ^ typedPostElabExprToString(i)
      | JumpUncond(l) -> "jmp .L" ^ labelToString(l)
      | TypedPostElabAssert(pExpr) -> "assert(" ^ boolExprToString(pExpr) ^ ")"
      | TypedPostElabVoidReturn -> "return"
      | VoidFunCall(func,args) -> 
          identToString(func) ^ "(" ^ (concat ", " (List.map typedPostElabExprToString args)) ^ ")"

and typedPostElabBlockToString(stmts : typedPostElabStmt list) = 
  concat "\n  " (List.map typedPostElabStmtToString stmts) ^ "\n"


let typedPostElabGlobalDeclToString(decl : typedPostElabGlobalDecl) =
  match decl with 
        TypedPostElabFunDef(c, i, params, stmts) ->
          c0typeToString(c) ^ identToString(i) ^ "(" ^ 
          (concat ", " (List.map paramToString params)) ^ ") {\n" ^ 
          typedPostElabBlockToString(stmts) ^ "}"
      | TypedPostElabStructDef(i, fs) -> "struct " ^ identToString(i) ^ "{\n" ^ 
                                   (concat "" (List.map fieldToString fs)) ^ "};"

let typedPostElabASTToString (typedFunDefs : typedPostElabGlobalDecl list) =
    concat "\n" (List.map typedPostElabGlobalDeclToString typedFunDefs) ^ "\n"
