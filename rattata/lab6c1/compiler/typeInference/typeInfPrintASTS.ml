open Datatypesv1
open PrintDatatypes
open Ast
open TypeInfAst
open String
module P = PrintASTs
module C = Char


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
      | FuncPointerDeref(expr, exprs) -> "(" ^ ptrExprToString(expr) ^ ")(" 
            ^ concat ", " (List.map typedPostElabExprToString exprs) ^ ")"
      | FieldAccess(i1,p,i2) -> ptrExprToString(p) ^ "." ^ identToString(i2)
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
      | AddressOfFunction(id) -> "&" ^ identToString(id)

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
      | IntGreaterThan(iExpr1,iExpr2) -> concat "" [intExprToString(iExpr1); " > "; intExprToString(iExpr2)]
      | IntLessThan(iExpr1,iExpr2) -> concat "" [intExprToString(iExpr1); " < "; intExprToString(iExpr2)]
      | IntEquals(iExpr1,iExpr2) -> concat "" [intExprToString(iExpr1); " == "; intExprToString(iExpr2)]
      | BoolEquals(bExpr1,bExpr2) -> concat "" [boolExprToString(bExpr1); " == "; boolExprToString(bExpr2)]
      | CharEquals(cExpr1,cExpr2) -> concat "" [charExprToString(cExpr1); " == "; charExprToString(cExpr2)]
      | PtrEquals(pExpr1,pExpr2) -> concat "" [ptrExprToString(pExpr1); " == "; ptrExprToString(pExpr2)]
      | LogNot(bExpr) -> concat "" ["!"; boolExprToString bExpr]
      | LogAnd(bExpr1,bExpr2) -> concat "" [boolExprToString(bExpr1); " && "; boolExprToString(bExpr2)]
      
and stringExprToString(sExpr : stringExpr) =
  match sExpr with
    StringConst(asciiChars) ->  
          let charList = List.map (fun c -> P.preElabExprToString
                       (PreElabConstExpr(c, CHAR))) asciiChars in
          let str = concat "" charList in
          concat "" ["\""; str; "\""]
  | StringSharedExpr(s) -> sharedTypeExprToString(s)

and charExprToString(cExpr : charExpr) =
  match cExpr with
    CharConst(c) ->  let charString = if c >= 0 then make 1 (C.chr c) else string_of_int c in
               (* there are some characters that we need to handle
                  that don't have ascii numbers apparently,
                  like \f, so I'm just making them negative numbers. - Ben *)
            concat "" ["\'"; charString; "\'"]
  | CharSharedExpr(s) -> sharedTypeExprToString(s)

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
      | CharExpr(c) -> charExprToString(c)
      | StringExpr(s) -> stringExprToString(s)

and typedPostElabStmtToString(s : typedPostElabStmt) =
  match s with
        TypedPostElabDecl(i,c) -> concat "" [c0typeToString(c); identToString(i)]
      | TypedPostElabAssignStmt(lval, op, e) -> concat "" [typedPostElabLValToString(lval); P.assignOpToString(op);
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

and typedPostElabBlockToString(stmts : typedPostElabStmt ref list) = 
  concat "\n  " (List.map (fun stmt -> typedPostElabStmtToString !stmt) stmts) ^ "\n"

let typedPostElabGlobalDeclToString(decl : typedPostElabGlobalDecl) =
  match decl with 
        TypedPostElabFunDef(c, i, params, stmts) ->
          c0typeToString(c) ^ identToString(i) ^ "(" ^ 
          (concat ", " (List.map P.paramToString params)) ^ ") {\n" ^ 
          typedPostElabBlockToString(stmts) ^ "}"
      | TypedPostElabStructDef(i, fs) -> "struct " ^ identToString(i) ^ "{\n" ^ 
                                   (concat "" (List.map P.fieldToString fs)) ^ "};"

let typedPostElabASTToString (typedFunDefs : typedPostElabGlobalDecl list) =
    concat "\n" (List.map typedPostElabGlobalDeclToString typedFunDefs) ^ "\n"
