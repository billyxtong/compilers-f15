open Datatypesv1
open PrintDatatypes
open Ast
open String

let identToString(i : ident) = i

let generalBinopToString(op : generalBinop) = 
  match op with
        IntBinop(i) -> intBinopToString(i)
      | DOUBLE_EQ -> " == "
      | GT -> " > "
      | LOG_AND -> " && "

let rec preElabExprToString(preelabexpr : preElabExpr) =
  match preelabexpr with
        PreElabConstExpr(c,t) -> constToString c
      | IdentExpr(i) -> identToString i
      | PreElabBinop(expr1, op, expr2) -> concat "" ["("; preElabExprToString expr1; " ";
                                                     generalBinopToString op; 
                                                     preElabExprToString expr2; ")"]
      | PreElabNot(expr1) -> concat "" ["!("; preElabExprToString(expr1); ")"]
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
                                                  "} else {\n\t"; elseOptToString(eOpt); "\n}"]
      | PreElabWhile(pExpr,pStmt) -> concat "" ["while("; preElabExprToString(pExpr);
                                                ") {\n\t"; preElabStmtToString(pStmt); "\n}"]
      | PreElabFor(sOpt1,pExpr,sOpt2,pStmt) -> concat "" ["for("; simpOptToString(sOpt1); "; ";
                                                        preElabExprToString(pExpr); "; ";
                                                        simpOptToString(sOpt2); ") {\n\t";
                                                        preElabStmtToString(pStmt); "\n}"]
      | PreElabReturn(pExpr) -> "return " ^ preElabExprToString(pExpr)

and preElabStmtToString(pStmt : preElabStmt) = 
  match pStmt with
        SimpStmt(s) -> simpStmtToString(s)
      | Control(c) -> controlToString(c)
      | Block(b) -> blockToString(b)

and blockToString(blk : block) = concat "\n" (List.map preElabStmtToString blk) ^ "\n" 

let preElabASTToString(blk) = blockToString blk

(* ============ Post-Elab AST Print Functions ================= *)

let rec intExprToString(iExpr : intExpr) = 
  match iExpr with
        IntConst(c) -> constToString(c)
      | IntIdent(i) -> identToString(i)
      | ASTBinop(expr1, op, expr2) -> concat "" ["("; intExprToString(expr1); 
                                                      intBinopToString(op); 
                                                      intExprToString(expr2); 
                                                 ")"]

let rec boolExprToString(bExpr : boolExpr) =
  match bExpr with
        BoolConst(c) -> constToString(c)
      | BoolIdent(i) -> identToString(i)
      | GreaterThan(iExpr1,iExpr2) -> concat "" [intExprToString(iExpr1); " > "; intExprToString(iExpr2)]
      | IntEquals(iExpr1,iExpr2) -> concat "" [intExprToString(iExpr1); " == "; intExprToString(iExpr2)]
      | BoolEquals(bExpr1,bExpr2) -> concat "" [boolExprToString(bExpr1); " == "; boolExprToString(bExpr2)]
      | LogNot(bExpr) -> concat "" ["!"; intExprToString(iExpr2)]
      | LogAnd(bExpr1,bExpr2) -> concat "" [boolExprToString(bExpr1); " && "; boolExprToString(bExpr2)]

let exprToString(e : expr) = 
  match e with
        IntExpr(i) -> intExprToString(i)
      | BoolExpr(b) -> boolExprToString(b)

let assignStmtToString(i,e) = concat "" [identToString(i); " = "; exprToString(e)]

let stmtToString(s : stmt) = 
  match s with
        Decl(i,c,p) -> concat "" [c0typeToString(c); identToString(i); postElabAstToString(p)]
      | AssignStmt(a) -> assignStmtToString(a)
      | If(b,p,p) -> ""
      | While(b,p) -> ""
      | Nop -> ""
      | Return(i) -> ""

and postElabAstToString(stmts : stmt list) = concat "\n" (List.map stmtToString stmts) ^ "\n"








