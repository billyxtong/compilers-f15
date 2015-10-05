open Ast

let elaboratePreElabDecl(decl : preElabDecl) = 
  match decl with
        NewVar(identifier, typee) -> [Decl(identifier, typee)]
      | Init(identifier, typee, expression) -> 
          [Decl(identifier, typee), AssignStmt(identifier, elaboratePreElabExpr(expression))]

let rec elaboratePreElabExpr(expression : preElabExpr) =
  match expression with
        PreElabConstExpr(constant, typee) -> (match typee with
                                                    INT -> IntConst(constant)
                                                  | BOOL -> BoolConst(constant))
      | IdentExpr(identifier) -> 
      | PreElabBinop(expr1, op, expr2) -> ""
      | PreElabNot(expression') -> ""

let elaboratePreElabStmt (statement : preElabStmt) =
  match statement with
        SimpStmt(s) -> elaborateSimpStmt(s)
      | Control(c) -> elaborateControl(c)
      | Block(b) -> elaborateBlock(b)

and elaborateSimpStmt (simpleStatement : simpStmt) =
  match simpleStatement with
        PreElabDecl(p) -> elaboratePreElabDecl(p)
      | SimpAssign(identifier, expression) -> 
      | SimpStmtExpr(p) -> elaboratePreElabExpr(p)

and elaborateControl (ctrl : control) =
  match ctrl with
        PreElabIf(pExpr, pStmt, eOpt) -> ""
      | PreElabWhile(pExpr, pStmt) -> ""
      | PreElabFor(sOpt,pExpr,sOpt,pStmt) -> ""
      | PreElabReturn(pExpr) -> ""

and elaborateBlock (statements : preElabStmt list) = List.map elaboratePreElabStmt statements


let elaborateAST (statements : preElabAST) = elaborateBlock(statements)
