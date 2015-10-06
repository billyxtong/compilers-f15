(* L2 Compiler
 * TypeChecker
 * Authors: Ben Plaut, William Tong
 * Handles undefined variables in unreachable code, significant simplifications
 *)

module A = Ast
module H = Hashtbl
module M = Core.Std.Map

let rec tc_expression env (expression : untypedPostElabExpr) ext =
  match expression with
    A.UntypedPostElabIdentExpr id ->
     (try
         let init = H.find env id in (* is it declared? *) (* change H because we're no longer using a hashtable *)
         if init then () (* declared and initialized, all good *)
         else (ErrorMsg.error None ("uninitialized variable " ^ id ^ "\n");
              raise ErrorMsg.Error)
       with Not_found -> 
              (ErrorMsg.error None ("undeclared variable " ^ id ^ "\n");
              raise ErrorMsg.Error))
  | A.UntypedPostElabConstExpr (constant, typee) -> ()
  | A.UntypedPostElabBinop (e1, op, e2) -> (* need to check the type of op *)
      tc_expression env e1 ext;
      tc_expression env e2 ext
  | A.UntypedPostElabNot e' -> tc_expression env e' ext


let rec tc_statements env (ast : untypedPostElabAST) (ext) (ret) =
  match ast with
    [] -> ret
  | A.UntypedPostElabDecl(d)::stms ->
      (match d with
        A.NewVar (id, idType) ->
          (try let _ = H.find env id in 
               ErrorMsg.error None ("redeclared variable " ^ id ^ "\n");
               raise ErrorMsg.Error
           with Not_found ->
               let () = H.add env id false in
               tc_statements env stms ext ret)
      | A.Init (id, idType, e) ->
          tc_statements env ((A.PreElabDecl (A.NewVar (id, idType)))
          ::(A.SimpAssign(id, A.EQ, e))::stms) ext ret)
  | A.UntypedPostElabAssignStmt(id, op, e)::stms ->
      tc_expression env e ext;
          (try
              let _ = H.find env id in (* it's declared, good *)
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
