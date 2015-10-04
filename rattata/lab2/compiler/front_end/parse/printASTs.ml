open Datatypesv1
open PrintDatatypes
open Ast

let identToString(i : ident) = i

let intExprToString(intexpr : intExpr) =
  match intexpr with
        IntConst(c) -> ""
      | IntIdent(i) -> ""
      | ASTBinop(expr1, op, expr2) -> ""

let preElabExprToString(preelabexpr : preElabExpr) =
  match preelabexpr with
        PreElabConstExpr(c,t) -> concat "" [c0TypeToString t; " "; constToString c]
      | IdentExpr(i) -> identToString i
      | PreElabBinop(expr1, op, expr2) -> concat "" [preElabExprToString expr1; 
                                                     intBinopToString op; 
                                                     preElabExprToString expr2]
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

let elseOptToString(eOpt : elseOpt) =
  match eOpt with
        EmptyElse -> ""
      | PreElabElse(p) -> preElabStmtToString(p)

and controlToString(c) = 
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

and preElabStmtToString(pStmt) = 
  match pStmt with
        SimpStmt(s) -> simpStmtToString(s)
      | Control(c) -> controlToString(c)
      | Block(b) -> blockToString(b)

and blockTostring(blk) =
  match blk with
        [] -> ""
      | pStmt :: stmts -> ""

let preElabASTToString(blk) = blockToString blk
