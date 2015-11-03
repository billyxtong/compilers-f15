open Ast

let rec elaboratePreElabLVal(lval : preElabLVal) =
  match lval with
        PreElabVarLVal(i) -> UntypedPostElabVarLVal(i)
      | PreElabFieldLVal(p,i) -> 
          (match p with
                 PreElabDerefLVal(_) -> UntypedPostElabFieldLVal(elaboratePreElabLVal(p), i)
               | _ -> UntypedPostElabFieldLVal(UntypedPostElabDerefLVal(elaboratePreElabLVal(p)), i))
      | PreElabDerefLVal(p) -> UntypedPostElabDerefLVal(elaboratePreElabLVal(p))
      | PreElabArrayAccessLVal(p,e) -> UntypedPostElabArrayAccessLVal(elaboratePreElabLVal(p),elaboratePreElabExpr(e))

and elaboratePreElabExpr(expression : preElabExpr) =
  match expression with
        PreElabConstExpr(constant, typee) -> UntypedPostElabConstExpr(constant, typee)
      | PreElabNullExpr -> UntypedPostElabNullExpr
      | PreElabIdentExpr(id) -> UntypedPostElabIdentExpr(id)
      | PreElabBinop(expr1, op, expr2) -> UntypedPostElabBinop(elaboratePreElabExpr(expr1), 
                                                           op, elaboratePreElabExpr(expr2))
      | PreElabNot(expression') -> UntypedPostElabNot(elaboratePreElabExpr(expression'))
      | PreElabTernary(e1, e2, e3) -> UntypedPostElabTernary(elaboratePreElabExpr e1,
                                          elaboratePreElabExpr e2, elaboratePreElabExpr e3)
      | PreElabFunCall(i, exprs) -> UntypedPostElabFunCall(i, List.map elaboratePreElabExpr exprs)
      | PreElabFieldAccessExpr(e,i) -> 
          (match e with
                 PreElabDerefExpr(_) -> UntypedPostElabFieldAccessExpr(elaboratePreElabExpr(e), i)
               | _ -> UntypedPostElabFieldAccessExpr(UntypedPostElabDerefExpr(elaboratePreElabExpr(e)), i))
      | PreElabAlloc(c) -> UntypedPostElabAlloc(c)
      | PreElabDerefExpr(e) -> UntypedPostElabDerefExpr(elaboratePreElabExpr(e))
      | PreElabArrayAlloc(c,e) -> UntypedPostElabArrayAlloc(c, elaboratePreElabExpr(e))
      | PreElabArrayAccessExpr(e1, e2) -> UntypedPostElabArrayAccessExpr(elaboratePreElabExpr(e1), elaboratePreElabExpr(e2))

let elaboratePreElabDecl(decl : preElabDecl) = 
  match decl with
        NewVar(identifier, typee) -> [UntypedPostElabDecl(identifier, typee)]
      | Init(identifier, typee, expression) -> [UntypedPostElabInitDecl(identifier, typee,
                                                 elaboratePreElabExpr expression)]


let addSimpStmtToPreElabStmt(pStmt,sStmt) = 
  match (sStmt, pStmt) with
        (EmptySimp, _) -> pStmt
    (* When you add a simpStmt to the end of a block, the block statements need
       to be a separate inner block so that they have their own scope *)
      | (HasSimpStmt(s), Block(b)) -> Block(Block b::SimpStmt(s)::[])
      | (HasSimpStmt(s), _) -> Block([pStmt; SimpStmt(s)])

let elaborateSimpStmt (simpleStatement : simpStmt) =
  match simpleStatement with
        PreElabDecl(p) -> elaboratePreElabDecl(p)
      | SimpAssign(lval,op,exp) -> [UntypedPostElabAssignStmt(elaboratePreElabLVal(lval), 
                                    op, elaboratePreElabExpr(exp))]
      | SimpStmtExpr(p) -> [UntypedPostElabExprStmt(elaboratePreElabExpr p)]

let elaborateInitFor(sOpt : simpOpt) =
  match sOpt with
        EmptySimp -> ([], [])
      | HasSimpStmt(sStmt) -> 
          (match sStmt with
                 PreElabDecl(p) -> ([], elaborateSimpStmt(sStmt))
               | SimpAssign(lval,op,exp) -> (elaborateSimpStmt(sStmt), [])
               | SimpStmtExpr(p) -> (elaborateSimpStmt(sStmt), []))

let elaborateSimpOpt(sOpt : simpOpt) = 
  match sOpt with
        EmptySimp -> []
      | HasSimpStmt(stmt) -> elaborateSimpStmt(stmt)

let rec elaborateElseOpt(eOpt : elseOpt) =
  match eOpt with
        EmptyElse -> []
      | PreElabElse(pStmt) -> elaboratePreElabStmt(pStmt)

and elaboratePreElabStmt (statement : preElabStmt) =
  match statement with
        SimpStmt(s) -> elaborateSimpStmt(s)
      | Control(c) -> elaborateControl(c)
      | Block(b) -> (* elaborateBlock(b) *) 
          (match b with
                [Block(b')] -> elaborateBlock(b')
               | _ -> elaborateBlock(b))

and elaborateControl (ctrl : control) : (Ast.untypedPostElabStmt list) =
  match ctrl with
        PreElabIf(pExpr, pStmt, eOpt) -> [UntypedPostElabIf(elaboratePreElabExpr(pExpr), 
                                                            elaboratePreElabStmt(pStmt), 
                                                            elaborateElseOpt(eOpt))]
      | PreElabWhile(pExpr, pStmt) -> [UntypedPostElabWhile(elaboratePreElabExpr(pExpr), 
                                                            elaboratePreElabStmt(pStmt), 
                                                            [])]
      | PreElabFor(sOpt1,pExpr,sOpt2,pStmt) -> 
          let (outside, inside) = elaborateInitFor(sOpt1) in 
          (outside @ [UntypedPostElabWhile(elaboratePreElabExpr(pExpr), 
                                           elaboratePreElabStmt(addSimpStmtToPreElabStmt(pStmt,sOpt2)),
                                           inside)])
      | PreElabReturn(pExpr) -> [UntypedPostElabReturn(elaboratePreElabExpr(pExpr))]
      | PreElabVoidReturn -> [UntypedPostElabVoidReturn]
      | PreElabAssert(e) -> [UntypedPostElabAssert(elaboratePreElabExpr e)]

and elaborateBlock (stmts : preElabStmt list) = UntypedPostElabBlock
    (List.flatten (List.map elaboratePreElabStmt stmts))::[]

let elaborateDecl (decl : globalDecl) =
  match decl with
        FunDecl(c,i,ps) -> UntypedPostElabFunDecl(c,i,ps)
      | FunDef(c,i,ps,body) -> UntypedPostElabFunDef(c,i,ps,elaborateBlock body)
      | Typedef(c,i) -> UntypedPostElabTypedef(c,i)
      | PreElabStructDecl(i) -> UntypedPostElabStructDecl(i)
      | PreElabStructDef(i,fs) -> UntypedPostElabStructDef(i,fs)

let elaborateAST (decls : preElabAST) = List.map elaborateDecl decls

let elaborateOverallAST ((preElab1, preElab2) : preElabOverallAST) =
    (elaborateAST preElab1, elaborateAST preElab2)
