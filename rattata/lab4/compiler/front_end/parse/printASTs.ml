open Datatypesv1
open PrintDatatypes
open Ast
open String

let generalBinopToString(op : generalBinop) = 
  match op with
        IntBinop(i) -> intBinopToString(i)
      | DOUBLE_EQ -> " == "
      | GT -> " > "
      | LT -> " < "
      | LOG_AND -> " && "
      | _ -> assert(false)

let rec preElabExprToString(preelabexpr : preElabExpr) =
  match preelabexpr with
        PreElabConstExpr(c,t) -> constToString c
      | PreElabIdentExpr(i) -> identToString i
      | PreElabBinop(expr1, op, expr2) -> concat "" ["("; preElabExprToString expr1; " ";
                                                     generalBinopToString op; 
                                                     preElabExprToString expr2; ")"]
      | PreElabNot(expr1) -> concat "" ["!"; preElabExprToString(expr1)]
      | PreElabTernary(e1, e2, e3) -> "(" ^ (preElabExprToString e1) ^ " ? " ^
              (preElabExprToString e2) ^ " : " ^ (preElabExprToString e3) ^ ")"
      | PreElabFunCall(func, args) -> identToString(func) ^ "(" ^ (concat ", " (List.map preElabExprToString args)) ^ ")"

let preElabDeclToString(preelabdecl : preElabDecl) =
  match preelabdecl with
        NewVar(i,t) -> c0typeToString(t) ^ identToString(i)
      | Init(i,t,p) -> concat "" [c0typeToString(t); identToString(i); " = "; preElabExprToString(p)]

let simpStmtToString(statement : simpStmt) =
  match statement with
        PreElabDecl(p) -> preElabDeclToString(p)
      | SimpAssign(i,p) -> concat "" [identToString(i); " = "; preElabExprToString(p)]
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
                                                  ") {\n\t"; preElabStmtToString(pStmt); 
                                                  "} \nelse {\n\t"; elseOptToString(eOpt); "\n}"]
      | PreElabWhile(pExpr,pStmt) -> concat "" ["while("; preElabExprToString(pExpr);
                                                ") {\n\t"; preElabStmtToString(pStmt); "\n}"]
      | PreElabFor(sOpt1,pExpr,sOpt2,pStmt) -> concat "" ["for("; simpOptToString(sOpt1); "; ";
                                                        preElabExprToString(pExpr); "; ";
                                                        simpOptToString(sOpt2); ") {\n\t";
                                                        preElabStmtToString(pStmt); "\n}"]
      | PreElabReturn(pExpr) -> "return " ^ preElabExprToString(pExpr)
      | PreElabVoidReturn -> "return"
      | PreElabAssert(pExpr) -> "assert(" ^ preElabExprToString(pExpr) ^ ")"

and preElabStmtToString(pStmt : preElabStmt) = 
  match pStmt with
        SimpStmt(s) -> simpStmtToString(s)
      | Control(c) -> controlToString(c)
      | Block(b) -> blockToString(b)

and blockToString(blk : block) = concat "\n" (List.map preElabStmtToString blk) ^ "\n" 

let paramToString(c, i) = c0typeToString(c) ^ identToString(i)

let globalDeclToString(g : globalDecl) =
  match g with
        FunDecl(c, i, params) -> c0typeToString(c) ^ identToString(i) ^ "(" ^ 
                                 (concat ", " (List.map paramToString params)) 
                                 ^ ")"
      | FunDef(c, i, params, stmts) -> 
          c0typeToString(c) ^ identToString(i) ^ "(" ^ 
          (concat ", " (List.map paramToString params)) ^ ") {\n" ^ 
          blockToString(stmts) ^ "}"
      | Typedef(c, i) -> "typedef " ^ c0typeToString(c) ^ identToString(i)

let preElabASTToString (mainDecls, headerDecls) =
    "MAIN:\n" ^ (concat "\n" (List.map globalDeclToString mainDecls) ^ "\n\n")
    ^ "HEADER:\n" ^ (concat "\n" (List.map globalDeclToString headerDecls) ^ "\n")

(* ============ Untyped Post-Elab AST Print Functions ================= *)

let rec untypedPostElabExprToString(expression : untypedPostElabExpr) =
  match expression with
        UntypedPostElabConstExpr(c,t) -> constToString c
      | UntypedPostElabIdentExpr(i) -> identToString i
      | UntypedPostElabBinop(expr1, op, expr2) -> concat "" ["("; untypedPostElabExprToString expr1; " ";
                                                     generalBinopToString op;
                                                     untypedPostElabExprToString expr2; ")"]
      | UntypedPostElabNot(expr1) -> concat "" ["!"; untypedPostElabExprToString(expr1)]
      | UntypedPostElabTernary(e1, e2, e3) -> "(" ^ (untypedPostElabExprToString e1) ^ " ? " ^
              (untypedPostElabExprToString e2) ^ " : " ^ (untypedPostElabExprToString e3) ^ ")" 
      | UntypedPostElabFunCall(func, args) -> 
          identToString(func) ^ "(" ^ (concat ", " (List.map untypedPostElabExprToString args)) ^ ")"


let rec untypedPostElabStmtToString(s : untypedPostElabStmt) =
  match s with
        UntypedPostElabDecl(identifier,constant) -> concat "" [c0typeToString(constant); identToString(identifier)]
      | UntypedPostElabInitDecl(identifier,constant, e) -> concat "" [c0typeToString(constant); identToString(identifier); " = "; untypedPostElabExprToString e]
      | UntypedPostElabAssignStmt(identifier,untypedexpr) -> concat "" [identToString(identifier); " = ";
                                                                        untypedPostElabExprToString(untypedexpr)]
      | UntypedPostElabIf(expression,postelabast1,postelabast2) -> concat "" ["if("; untypedPostElabExprToString(expression);
                                                  ") {\n\t"; untypedPostElabBlockToString(postelabast1);
                                                  "} \nelse {\n\t"; untypedPostElabBlockToString(postelabast2); "\n}"]
      | UntypedPostElabWhile(expression,postelabast,init) -> concat "" ["while("; untypedPostElabExprToString(expression);
                                                ", "; untypedPostElabBlockToString(init); ") {\n\t";
                                                      untypedPostElabBlockToString(postelabast); "\n}"]
      | UntypedPostElabReturn(i) -> "return " ^ untypedPostElabExprToString(i)
      | UntypedPostElabBlock(blockAst) -> "\t" ^ untypedPostElabBlockToString blockAst
      | UntypedPostElabAssert(pExpr) -> "assert(" ^ untypedPostElabExprToString(pExpr) ^ ")"
      | UntypedPostElabVoidReturn -> "return"
      | UntypedPostElabExprStmt(pExpr) -> untypedPostElabExprToString(pExpr) ^ "\n"

                                          
and untypedPostElabBlockToString(stmts : untypedPostElabBlock) =
  concat "\n" (List.map untypedPostElabStmtToString stmts) ^ "\n"

let untypedPostElabGlobalDeclToString(g : untypedPostElabGlobalDecl) =
  match g with
        UntypedPostElabFunDecl(c, i, params) -> c0typeToString(c) ^ identToString(i) ^ "(" ^ 
                                 (concat ", " (List.map paramToString params)) 
                                 ^ ")"
      | UntypedPostElabFunDef(c, i, params, stmts) -> 
          c0typeToString(c) ^ identToString(i) ^ "(" ^ 
          (concat ", " (List.map paramToString params)) ^ ") {\n" ^ 
          untypedPostElabBlockToString(stmts) ^ "}"
      | UntypedPostElabTypedef(c, i) -> "typedef " ^ c0typeToString(c) ^ identToString(i)

let untypedPostElabOverallASTToString (mainDecls, headerDecls) =
    "MAIN:\n" ^ (concat "\n" (List.map untypedPostElabGlobalDeclToString mainDecls) ^ "\n\n")
    ^ "HEADER:\n" ^ (concat "\n" (List.map untypedPostElabGlobalDeclToString headerDecls) ^ "\n")

(* ============ Typed Post-Elab AST Print Functions ================= *)

let shiftOpToString(s : shiftOp) =
  match s with
        ASTrshift -> " >> "
      | ASTlshift -> " << "

let rec intExprToString(iExpr : intExpr) =
  match iExpr with
        IntConst(c) -> constToString(c)
      | IntIdent(i) -> identToString(i)
      | ASTBinop(expr1, op, expr2) -> concat "" ["("; intExprToString(expr1);
                                                      intBinopToString(op);
                                                      intExprToString(expr2);
                                                 ")"]
      | IntTernary(b,i1,i2) -> concat "" [boolExprToString(b); " ? ";
                                          intExprToString(i1); " : ";
                                          intExprToString(i2)]
      | BaseCaseShift(i1, shifty, i2) -> concat "" [intExprToString(i1);
                                                    shiftOpToString(shifty);
                                                    intExprToString(i2)]
      | IntFunCall(func, args) -> 
          identToString(func) ^ "(" ^ (concat ", " (List.map typedPostElabExprToString args)) ^ ")"
and boolExprToString(bExpr : boolExpr) =
  match bExpr with
        BoolConst(c) -> if c = 0 then "false" else "true"
      | BoolIdent(i) -> identToString(i)
      | GreaterThan(iExpr1,iExpr2) -> concat "" [intExprToString(iExpr1); " > "; intExprToString(iExpr2)]
      | LessThan(iExpr1,iExpr2) -> concat "" [intExprToString(iExpr1); " < "; intExprToString(iExpr2)]
      | IntEquals(iExpr1,iExpr2) -> concat "" [intExprToString(iExpr1); " == "; intExprToString(iExpr2)]
      | BoolEquals(bExpr1,bExpr2) -> concat "" [boolExprToString(bExpr1); " == "; boolExprToString(bExpr2)]
      | LogNot(bExpr) -> concat "" ["!"; boolExprToString bExpr]
      | LogAnd(bExpr1,bExpr2) -> concat "" [boolExprToString(bExpr1); " && "; boolExprToString(bExpr2)]
      | BoolTernary(b1, b2, b3) -> concat "" [boolExprToString(b1); " ? ";
                                              boolExprToString(b2); " : ";
                                              boolExprToString(b3)]
      | BoolFunCall(func, args) -> 
          identToString(func) ^ "(" ^ (concat ", " (List.map typedPostElabExprToString args)) ^ ")"

and typedPostElabExprToString(e : typedPostElabExpr) =
  match e with
        IntExpr(i) -> intExprToString(i)
      | BoolExpr(b) -> boolExprToString(b)
      | VoidExpr(stmt) -> typedPostElabStmtToString(stmt)

and typedPostElabStmtToString(s : typedPostElabStmt) =
  match s with
        TypedPostElabDecl(i,c) -> concat "" [c0typeToString(c); identToString(i)]
      | TypedPostElabAssignStmt(i, e) -> concat "" [identToString(i); " = ";
                                                    typedPostElabExprToString(e)]
      | TypedPostElabIf(b,p1,p2) -> concat "" ["if(";
                                               boolExprToString(b);
                                               ") {\n\t";
                                               typedPostElabBlockToString(p1);
                                               "} else {\n\t";
                                               typedPostElabBlockToString(p2); "\n}"]
      | TypedPostElabWhile(b,p) -> concat "" ["while("; boolExprToString(b);
                                                ") {\n\t"; typedPostElabBlockToString(p); "\n}"]
      | TypedPostElabReturn(i) -> "return " ^ typedPostElabExprToString(i)
      | JumpUncond(l) -> "jmp .L" ^ labelToString(l)
      | TypedPostElabAssert(pExpr) -> "assert(" ^ boolExprToString(pExpr) ^ ")"
      | TypedPostElabVoidReturn -> "return"
      | VoidFunCall(func,args) -> 
          identToString(func) ^ "(" ^ (concat ", " (List.map typedPostElabExprToString args)) ^ ")"

and typedPostElabBlockToString(stmts : typedPostElabStmt list) = 
  concat "\n" (List.map typedPostElabStmtToString stmts) ^ "\n"


let typedPostElabGlobalDeclToString(TypedPostElabFunDef(c, i, params, stmts) : typedPostElabGlobalDecl) =
    c0typeToString(c) ^ identToString(i) ^ "(" ^ 
    (concat ", " (List.map paramToString params)) ^ ") {\n" ^ 
    typedPostElabBlockToString(stmts) ^ "}"

let typedPostElabASTToString (typedFunDefs : typedPostElabGlobalDecl list) =
    concat "\n" (List.map typedPostElabGlobalDeclToString typedFunDefs) ^ "\n"
