(* L2 Compiler
 * TypeChecker
 * Authors: Ben Plaut, William Tong
 * Handles undefined variables in unreachable code, significant simplifications
 *)

open Ast
module A = Ast
open Datatypesv1
module M = Core.Std.Map
open String

let isValidVarDecl (identifier : ident) = 
  if sub identifier 0 1 = "\\" 
  then true 
  else false

let rec tc_expression env (expression : A.untypedPostElabExpr) =
  match expression with
    A.UntypedPostElabConstExpr (constant, typee) -> 
      (match typee with
             INT -> A.IntExpr(IntConst(constant))
           | BOOL -> A.BoolExpr(BoolConst(constant)))
  | A.UntypedPostElabIdentExpr id -> 
      (match M.find env id with (* is it declared? *)
                   Some(typee, isInitialized) -> (if isInitialized (* declared and initialized, all good *)
                                                 then (match typee with
                                                       INT -> A.IntExpr(IntIdent(id))
                                                     | BOOL -> A.BoolExpr(BoolIdent(id)))
                                                 else (ErrorMsg.error None ("uninitialized variable " ^ id ^ "\n");
                                                       raise ErrorMsg.Error))
                 | None -> (ErrorMsg.error None ("undeclared variable " ^ id ^ "\n");
                            raise ErrorMsg.Error))
  | A.UntypedPostElabBinop (e1, op, e2) -> 
      let tcExpr1 = tc_expression env e1 in
      let tcExpr2 = tc_expression env e2 in
      (match op with
             GT -> (match (tcExpr1, tcExpr2) with
                          (IntExpr(exp1), IntExpr(exp2)) -> BoolExpr(GreaterThan(exp1, exp2))
                        | _ -> ErrorMsg.error None ("greater than expression didn't typecheck \n");
                               raise ErrorMsg.Error)
           | DOUBLE_EQ -> 
               (match (tcExpr1, tcExpr2) with
                      (IntExpr(exp1), IntExpr(exp2)) -> BoolExpr(IntEquals(exp1, exp2))
                    | (BoolExpr(exp1), BoolExpr(exp2)) -> BoolExpr(BoolEquals(exp1, exp2))
                    | _ -> ErrorMsg.error None ("double equals expressions didn't typecheck \n");
                           raise ErrorMsg.Error)
           | LOG_AND -> (match (tcExpr1, tcExpr2) with
                               (BoolExpr(exp1), BoolExpr(exp2)) -> BoolExpr(LogAnd(exp1, exp2))
                             | _ -> ErrorMsg.error None ("logical and expressions didn't typecheck \n");
                                    raise ErrorMsg.Error)
           | IntBinop(intOp) -> (match (tcExpr1, tcExpr2) with
                          (IntExpr(exp1), IntExpr(exp2)) -> IntExpr(ASTBinop(exp1, intOp, exp2))
                        | _ -> ErrorMsg.error None ("int binop expressions didn't typecheck \n");
                               raise ErrorMsg.Error))
  | A.UntypedPostElabNot e' -> 
      let tcExpr = tc_expression env e' in
      (match tcExpr with
             BoolExpr(exp1) -> BoolExpr(LogNot(exp1))
           | _ -> ErrorMsg.error None ("not expression didn't typecheck \n");
                  raise ErrorMsg.Error)
  | A.UntypedPostElabTernary(e1, e2, e3) ->
      let tcExpr1 = tc_expression env e1 in
      let tcExpr2 = tc_expression env e2 in
      let tcExpr3 = tc_expression env e1 in
      (match (tcExpr1, tcExpr2, tcExpr3) with
             (BoolExpr(exp1), IntExpr(exp2), IntExpr(exp3)) -> IntExpr(IntTernary(exp1, exp2, exp3))
           | (BoolExpr(exp1), BoolExpr(exp2), BoolExpr(exp3)) -> BoolExpr(BoolTernary(exp1, exp2, exp3))
           | _ -> (ErrorMsg.error None ("ternary expression didn't typecheck \n");
                  raise ErrorMsg.Error))

let rec tc_statements env (untypedAST : untypedPostElabAST) (typedAST : typedPostElabAST)=
  match untypedAST with
    [] -> typedAST
  | A.UntypedPostElabDecl(id, typee)::stms ->
      (match M.find env id with
                   Some _ -> (ErrorMsg.error None ("redeclared variable " ^ id ^ "\n");
                              raise ErrorMsg.Error)
                 | None -> (let newMap = M.add env id (typee, false) 
                           in tc_statements newMap stms (typedAST @ [TypedPostElabDecl(id, typee)])))
  | A.UntypedPostElabAssignStmt(id, e)::stms ->
       (let tcExpr = tc_expression env e in
          (match M.find env id with (* it's declared, good *)
                 Some(typee, _) ->  
                   (match (tcExpr, typee) with
                          (IntExpr(_), INT) -> let newMap = M.add env id (typee, true) in              
                                               tc_statements newMap stms (typedAST @ [TypedPostElabAssignStmt(id, tcExpr)])
                        | (BoolExpr(_), BOOL) -> let newMap = M.add env id (typee, true) in              
                                                 tc_statements newMap stms (typedAST @ [TypedPostElabAssignStmt(id, tcExpr)])
                        | _ -> (ErrorMsg.error None ("assignment expression didn't typecheck \n");
                               raise ErrorMsg.Error))
               | None -> (if ((isValidVarDecl id))
                          then (let newMap = M.add env id (INT, true) in              
                                tc_statements newMap stms (typedAST @ [TypedPostElabAssignStmt(id, tcExpr)]))
                          else (ErrorMsg.error None ("undeclared test variable " ^ id ^ "\n");
                                raise ErrorMsg.Error))))
  | A.UntypedPostElabIf(e, ast1, ast2)::stms -> 
      let tcExpr = tc_expression env e in
      (match tcExpr with
             BoolExpr(exp1) -> tc_statements env stms 
              (typedAST @ [A.TypedPostElabIf(exp1, tc_statements env ast1 [], tc_statements env ast2 [])])
           | _ -> ErrorMsg.error None ("if expression didn't typecheck\n");
                  raise ErrorMsg.Error)
  | A.UntypedPostElabWhile(e, ast1)::stms -> 
      let tcExpr = tc_expression env e in
      (match tcExpr with
             BoolExpr(exp1) -> tc_statements env stms 
                (typedAST @ [A.TypedPostElabWhile(exp1, tc_statements env ast1 [])])
           | _ -> ErrorMsg.error None ("while expression didn't typecheck\n");
                  raise ErrorMsg.Error)
  | A.UntypedPostElabReturn(e)::stms -> 
      let tcExpr = tc_expression env e in
      (* apparently all variables defined before the first return
         get to be treated as initialized...although those declared
         after don't *)
      let newMap = M.map env (fun (typee, _) -> (typee, true)) in
      (match tcExpr with
             IntExpr(exp1) -> tc_statements newMap stms (typedAST @ [A.TypedPostElabReturn(exp1)])
           | _ -> ErrorMsg.error None ("return expression didn't typecheck\n");
                  raise ErrorMsg.Error)
             (* there was something else here related to the previous comment that I don't quite understand *)
        
and typecheck prog =
  let environment = Core.Std.String.Map.empty in
  tc_statements environment prog []
  (* else (ErrorMsg.error None "main does not return\n"; raise ErrorMsg.Error) *)
