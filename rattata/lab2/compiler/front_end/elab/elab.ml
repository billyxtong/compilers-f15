open Ast

let elaboratePreElabStmt (statement : preElabStmt) =
  match statement with
        SimpStmt(s) -> elaborateSimpStmt(s)
      | Control(c) -> elaborateControl(c)
      | Block(b) -> elaborateBlock(b)

and elaborateSimpStmt (simpleStatement : simpStmt) =
  match simpleStatement with
        PreElabDecl(p) -> ""
      | SimpAssign(i, p) -> ""
      | SimpStmtExpr(p) -> ""

and elaborateControl (ctrl : control) =
  match ctrl with
        PreElabIf(pExpr, pStmt, eOpt) -> ""
      | PreElabWhile(pExpr, pStmt) -> ""
      | PreElabFor(sOpt,pExpr,sOpt,pStmt) -> ""
      | PreElabReturn(pExpr) -> ""

and elaborateBlock (blk : block) = elaborateAST blk


let elaborateAST (statements : preElabAST) =
  match statements with
        [] -> []
      | statement :: statements' -> elaboratePreElabStmt(statement) :: elaborateAST(statements')
