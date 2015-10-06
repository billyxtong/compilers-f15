(* L2 Compiler
 * TypeChecker
 * Authors: Ben Plaut, William Tong
 * Handles undefined variables in unreachable code, significant simplifications
 *)

module A = Ast
open Datatypesv1
module M = Core.Std.Map

let rec tc_expression env (expression : untypedPostElabExpr) ext =
  match expression with
    A.UntypedPostElabConstExpr (constant, typee) -> 
      (match typee with
             INT -> IntExpr(IntConst(constant))
           | BOOL -> BoolExpr(BoolConst(constant)))
  | A.UntypedPostElabIdentExpr id ->
     (try
         let (typee, IsInitialized) = M.find env id in (* is it declared? *)
         if isInitialized (* declared and initialized, all good *)
         then 
           (match typee with
                  INT -> IntExpr(IntIdent(id))
                | BOOL -> BoolExpr(BoolIdent(id)))
         else (ErrorMsg.error None ("uninitialized variable " ^ id ^ "\n");
              raise ErrorMsg.Error)
       with Not_found -> 
              (ErrorMsg.error None ("undeclared variable " ^ id ^ "\n");
              raise ErrorMsg.Error))
  | A.UntypedPostElabBinop (e1, op, e2) -> 
      let tcExpr1 = tc_expression env e1 ext in
      let tcExpr2 = tc_expression env e2 ext in
      (match op with
             GT -> (match (tcExpr1, tcExpr2) with
                          (IntExpr(_), IntExpr(_)) -> BoolExpr(GreaterThan(tcExpr1, tcExpr2))
                        | _ -> ErrorMsg.error None ("expressions didn't typecheck \n");
                               raise ErrorMsg.Error)
           | DOUBLE_EQ -> (match (tcExpr1, tcExpr2) with
                                 (IntExpr(_), IntExpr(_)) -> BoolExpr(IntEquals(tcExpr1, tcExpr2))
                               | (BoolExpr(_), BoolExpr(_)) -> BoolExpr(BoolEquals(tcExpr1, tcExpr2))
                               | _ -> ErrorMsg.error None ("expressions didn't typecheck \n");
                                      raise ErrorMsg.Error)
           | LOG_AND -> (match (tcExpr1, tcExpr2) with
                               (BoolExpr(_), BoolExpr(_)) -> BoolExpr(LogAnd(tcExpr1, tcExpr2))
                             | _ -> ErrorMsg.error None ("expressions didn't typecheck \n");
                                    raise ErrorMsg.Error)
           | _ -> (match (tcExpr1, tcExpr2) with
                          (IntExpr(_), IntExpr(_)) -> IntExpr(ASTBinop(tcExpr1, op, tcExpr2))
                        | _ -> ErrorMsg.error None ("expressions didn't typecheck \n");
                               raise ErrorMsg.Error))
  | A.UntypedPostElabNot e' -> 
      let tcExpr = tc_expression env e' ext in
      (match tcExpr with
             BoolExpr(_) -> BoolExpr(LogNot(tcExpr))
           | _ -> ErrorMsg.error None ("expressions didn't typecheck \n");
                  raise ErrorMsg.Error)
  | A.UntypedPostElabTernary(e1, e2, e3) ->
      let tcExpr1 = tc_expression env e1 ext in
      let tcExpr2 = tc_expression env e2 ext in
      let tcExpr3 = tc_expression env e1 ext in
      (match (tcExpr1, tcExpr2, tcExpr3) with
             (BoolExpr(_), IntExpr(_), IntExpr(_)) -> IntExpr(IntTernary(tcExpr1, tcExpr2, tcExpr3))
           | (BoolExpr(_), BoolExpr(_), BoolExpr(_)) -> BoolExpr(BoolTernary(tcExpr1, tcExpr2, tcExpr3))
           | _ -> ErrorMsg.error None ("expressions didn't typecheck \n");
                  raise ErrorMsg.Error)

let rec tc_statements env (ast : untypedPostElabAST) ext ret =
  match ast with
    [] -> ret
  | A.UntypedPostElabDecl(id, typee)::stms ->
      (try let _ = M.find env id in 
               ErrorMsg.error None ("redeclared variable " ^ id ^ "\n");
               raise ErrorMsg.Error
           with Not_found ->
               let () = M.add env id (typee, false) in
               tc_statements env stms ext ret)

(* UNFINISHED FROM HERE DOWN *)
  | A.UntypedPostElabAssignStmt(id, e)::stms ->
      let tcExpr = tc_expression env e ext in
          (try
              let (typee, _) = M.find env id in (* it's declared, good *)
              let _ = H.replace env id true (* it's now initialized *) in
              tc_statements env stms ext ret
           with Not_found -> 
               ErrorMsg.error None ("undeclared variable " ^ id ^ "\n");
               raise ErrorMsg.Error)
  | A.UntypedPostElabIf(expression, ast1, ast2) -> ""
  | A.UntypedPostElabWhile(expression, ast1) -> ""
  | A.UntypedPostElabReturn(e)::stms -> 
      let () = tc_expression env e ext in
      (* apparently all variables defined before the first return
         get to be treated as initialized...although those declared
         after don't *)
      let () = H.iter (fun id _ -> H.replace env id true) env in
      tc_statements env stms ext true
        
let typecheck prog =
  let environment = String.Map.empty() in
  if tc_statements environment prog None false then ()
  else (ErrorMsg.error None "main does not return\n"; raise ErrorMsg.Error)
