(* L2 Compiler
 * TypeChecker
 * Authors: Ben Plaut, William Tong
 * Handles undefined variables in unreachable code, significant simplifications
 *)

module A = Ast
open Datatypesv1
module M = Core.Std.Map

let rec tc_expression env (expression : untypedPostElabExpr) =
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
      let tcExpr1 = tc_expression env e1 in
      let tcExpr2 = tc_expression env e2 in
      (match op with
             GT -> (match (tcExpr1, tcExpr2) with
                          (IntExpr(_), IntExpr(_)) -> BoolExpr(GreaterThan(tcExpr1, tcExpr2))
                        | _ -> ErrorMsg.error None ("greater than expression didn't typecheck \n");
                               raise ErrorMsg.Error)
           | DOUBLE_EQ -> 
               (match (tcExpr1, tcExpr2) with
                      (IntExpr(_), IntExpr(_)) -> BoolExpr(IntEquals(tcExpr1, tcExpr2))
                    | (BoolExpr(_), BoolExpr(_)) -> BoolExpr(BoolEquals(tcExpr1, tcExpr2))
                    | _ -> ErrorMsg.error None ("double equals expressions didn't typecheck \n");
                           raise ErrorMsg.Error)
           | LOG_AND -> (match (tcExpr1, tcExpr2) with
                               (BoolExpr(_), BoolExpr(_)) -> BoolExpr(LogAnd(tcExpr1, tcExpr2))
                             | _ -> ErrorMsg.error None ("logical and expressions didn't typecheck \n");
                                    raise ErrorMsg.Error)
           | _ -> (match (tcExpr1, tcExpr2) with
                          (IntExpr(_), IntExpr(_)) -> IntExpr(ASTBinop(tcExpr1, op, tcExpr2))
                        | _ -> ErrorMsg.error None ("int binop expressions didn't typecheck \n");
                               raise ErrorMsg.Error))
  | A.UntypedPostElabNot e' -> 
      let tcExpr = tc_expression env e' in
      (match tcExpr with
             BoolExpr(_) -> BoolExpr(LogNot(tcExpr))
           | _ -> ErrorMsg.error None ("not expression didn't typecheck \n");
                  raise ErrorMsg.Error)
  | A.UntypedPostElabTernary(e1, e2, e3) ->
      let tcExpr1 = tc_expression env e1 in
      let tcExpr2 = tc_expression env e2 in
      let tcExpr3 = tc_expression env e1 in
      (match (tcExpr1, tcExpr2, tcExpr3) with
             (BoolExpr(_), IntExpr(_), IntExpr(_)) -> IntExpr(IntTernary(tcExpr1, tcExpr2, tcExpr3))
           | (BoolExpr(_), BoolExpr(_), BoolExpr(_)) -> BoolExpr(BoolTernary(tcExpr1, tcExpr2, tcExpr3))
           | _ -> ErrorMsg.error None ("ternary expression didn't typecheck \n");
                  raise ErrorMsg.Error)

let rec tc_statements env (untypedAST : untypedPostElabAST) (typedAST : typedPostElabAST)=
  match untypedAST with
    [] -> typedAST
  | A.UntypedPostElabDecl(id, typee)::stms ->
      (try let _ = M.find env id in 
               ErrorMsg.error None ("redeclared variable " ^ id ^ "\n");
               raise ErrorMsg.Error
           with Not_found ->
               let () = M.add env id (typee, false) in
               tc_statements env stms (typedAST @ [TypedPostElabDecl(id, typee)]))
  | A.UntypedPostElabAssignStmt(id, e)::stms ->
      let tcExpr = tc_expression env e in
          (try
              let (typee, _) = M.find env id in (* it's declared, good *)
              (match (tcExpr, typee) with
                     (IntExpr(_), INT) -> 
                       let _ = M.replace env id (typee, true) in              
                       tc_statements env stms (typedAST @ [TypedPostElabAssignStmt(id, tcExpr)])
                   | (BoolExpr(_), BOOL) -> 
                       let _ = M.replace env id (typee, true) in              
                       tc_statements env stms (typedAST @ [TypedPostElabAssignStmt(id, tcExpr)])
                   | _ -> ErrorMsg.error None ("assignment expression didn't typecheck \n");
                          raise ErrorMsg.Error)
           with Not_found -> 
               ErrorMsg.error None ("undeclared variable \n");
               raise ErrorMsg.Error)
  | A.UntypedPostElabIf(e, ast1, ast2)::stms -> 
      let tcExpr = tc_expression env e in
      (match tcExpr with
             BoolExpr(_) -> tc_statements env stms (typedAST @ [A.TypedPostElabIf(tcExpr, typecheck ast1, typecheck ast2)])
           | _ -> ErrorMsg.error None ("if expression didn't typecheck\n");
                  raise ErrorMsg.Error)
  | A.UntypedPostElabWhile(e, ast1)::stms -> 
      let tcExpr = tc_expression env e in
      (match tcExpr with
             BoolExpr(_) -> tc_statements env stms (typedAST @ [A.TypedPostElabWhile(tcExpr, typecheck ast1)])
           | _ -> ErrorMsg.error None ("while expression didn't typecheck\n");
                  raise ErrorMsg.Error)
  | A.UntypedPostElabReturn(e)::stms -> 
      let tcExpr = tc_expression env e in
      (* apparently all variables defined before the first return
         get to be treated as initialized...although those declared
         after don't *)
      let () = M.iter (fun id _ -> M.replace env id true) env in
      (match tcExpr with
             IntExpr(_) -> tc_statements env stms (typedAST @ [A.TypedPostElabReturn(tcExpr)]
           | _ -> ErrorMsg.error None ("return expression didn't typecheck\n");
                  raise ErrorMsg.Error)
             (* there was something else here related to the previous comment that I don't quite understand *)
        
let typecheck prog =
  let environment = String.Map.empty() in
  tc_statements environment prog false []
  (* else (ErrorMsg.error None "main does not return\n"; raise ErrorMsg.Error) *)
