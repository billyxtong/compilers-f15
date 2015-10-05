open Ast

let rec elaboratePreElabExpr(expression : preElabExpr) =
  match expression with
        PreElabConstExpr(constant, typee) -> UntypedPostElabConstExpr(constant, typee)
      | PreElabIdentExpr(identifier) -> UntypedPostElabIdentExpr(identifier)
      | PreElabBinop(expr1, op, expr2) -> UntypedPostElabBinop(elaboratePreElabExpr(expr1), 
                                                           op, elaboratePreElabExpr(expr2))
      | PreElabNot(expression') -> UntypedPostElabNot(elaboratePreElabExpr(expression'))

let elaboratePreElabDecl(decl : preElabDecl) = 
  match decl with
        NewVar(identifier, typee) -> [UntypedPostElabDecl(identifier, typee)]
      | Init(identifier, typee, expression) -> 
          [UntypedPostElabDecl(identifier, typee); 
           UntypedPostElabAssignStmt(identifier, 
                        elaboratePreElabExpr(expression))]


let addSimpStmtToPreElabStmt(pStmt,sStmt) = 
  match (sStmt, pStmt) with
        (EmptySimp, _) -> pStmt
      | (HasSimpStmt(s), Block(b)) -> Block(List.append b [SimpStmt(s)])
      | (HasSimpStmt(s), _) -> Block([pStmt; SimpStmt(s)])

let elaborateSimpStmt (simpleStatement : simpStmt) =
  match simpleStatement with
        PreElabDecl(p) -> elaboratePreElabDecl(p)
      | SimpAssign(identifier, expression) -> [UntypedPostElabAssignStmt(identifier, elaboratePreElabExpr(expression))]
      | SimpStmtExpr(p) -> [UntypedPostElabAssignStmt(GenUnusedID.create(), elaboratePreElabExpr(p))] (* fix this *)

let elaborateSimpOpt(sOpt : simpOpt) = 
  match sOpt with
        EmptySimp -> [] (* problems here? *)
      | HasSimpStmt(stmt) -> elaborateSimpStmt(stmt)




let rec elaborateElseOpt(eOpt : elseOpt) =
  match eOpt with
        EmptyElse -> []
      | PreElabElse(pStmt) -> elaboratePreElabStmt(pStmt)

and elaboratePreElabStmt (statement : preElabStmt) =
  match statement with
        SimpStmt(s) -> elaborateSimpStmt(s)
      | Control(c) -> elaborateControl(c)
      | Block(b) -> elaborateBlock(b)


and elaborateControl (ctrl : control) : (Ast.untypedPostElabStmt list) =
  match ctrl with
        PreElabIf(pExpr, pStmt, eOpt) -> [UntypedPostElabIf(elaboratePreElabExpr(pExpr), 
                                                            elaboratePreElabStmt(pStmt), 
                                                            elaborateElseOpt(eOpt))]
      | PreElabWhile(pExpr, pStmt) -> [UntypedPostElabWhile(elaboratePreElabExpr(pExpr), elaboratePreElabStmt(pStmt))]
      | PreElabFor(sOpt1,pExpr,sOpt2,pStmt) -> elaborateSimpOpt(sOpt1) @ [UntypedPostElabWhile(elaboratePreElabExpr(pExpr), 
                                                elaboratePreElabStmt(addSimpStmtToPreElabStmt(pStmt,sOpt2)))]
      | PreElabReturn(pExpr) -> [UntypedPostElabReturn(elaboratePreElabExpr(pExpr))]

and elaborateBlock (statements : preElabStmt list) = List.flatten (List.map elaboratePreElabStmt statements)

let elaborateAST (statements : preElabAST) = elaborateBlock(statements)
