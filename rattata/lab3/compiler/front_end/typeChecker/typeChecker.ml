(* L3 Compiler
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

let rec argsMatch (arguments : typedPostElabExpr list) (paramTypes : c0type list) =
  match (arguments, paramTypes) with
        ([], []) -> true
      | ([], p :: ps) -> false
      | (arg :: args, []) -> false
      | (arg :: args, p :: ps) ->
          (match (arg, p) with
                 (IntExpr(_), INT) -> argsMatch args ps
               | (BoolExpr(_), BOOL) -> argsMatch args ps
               | _ -> false)


let rec tc_expression env (expression : untypedPostElabExpr) =
  match expression with
    UntypedPostElabConstExpr (constant, typee) -> 
      (match typee with
             INT -> IntExpr(IntConst(constant))
           | BOOL -> BoolExpr(BoolConst(constant)))
  | UntypedPostElabIdentExpr id -> 
      (match M.find env id with (* is it declared? *)
                   Some(typee, isInitialized) -> (if isInitialized (* declared and initialized, all good *)
                                                 then (match typee with
                                                       INT -> IntExpr(IntIdent(id))
                                                     | BOOL -> BoolExpr(BoolIdent(id)))
                                                 else (ErrorMsg.error ("uninitialized variable " ^ id ^ "\n");
                                                       raise ErrorMsg.Error))
                 | None -> (ErrorMsg.error ("undeclared variable " ^ id ^ "\n");
                            raise ErrorMsg.Error))
  | UntypedPostElabBinop (e1, op, e2) -> 
      let tcExpr1 = tc_expression env e1 in
      let tcExpr2 = tc_expression env e2 in
      (match op with
             GT -> (match (tcExpr1, tcExpr2) with
                          (IntExpr(exp1), IntExpr(exp2)) -> BoolExpr(GreaterThan(exp1, exp2))
                        | _ -> ErrorMsg.error ("greater than expression didn't typecheck \n");
                               raise ErrorMsg.Error)
           | LT -> (match (tcExpr1, tcExpr2) with
                          (IntExpr(exp1), IntExpr(exp2)) -> BoolExpr(LessThan(exp1, exp2))
                        | _ -> ErrorMsg.error ("greater than expression didn't typecheck \n");
                               raise ErrorMsg.Error)
           | DOUBLE_EQ -> 
               (match (tcExpr1, tcExpr2) with
                      (IntExpr(exp1), IntExpr(exp2)) -> BoolExpr(IntEquals(exp1, exp2))
                    | (BoolExpr(exp1), BoolExpr(exp2)) -> BoolExpr(BoolEquals(exp1, exp2))
                    | _ -> ErrorMsg.error ("double equals expressions didn't typecheck \n");
                           raise ErrorMsg.Error)
           | LOG_AND -> (match (tcExpr1, tcExpr2) with
                               (BoolExpr(exp1), BoolExpr(exp2)) -> BoolExpr(LogAnd(exp1, exp2))
                             | _ -> ErrorMsg.error ("logical and expressions didn't typecheck \n");
                                    raise ErrorMsg.Error)
           | IntBinop(intOp) -> (match (tcExpr1, tcExpr2) with
                          (IntExpr(exp1), IntExpr(exp2)) -> IntExpr(ASTBinop(exp1, intOp, exp2))
                        | _ -> ErrorMsg.error ("int binop expressions didn't typecheck \n");
                               raise ErrorMsg.Error))
  | UntypedPostElabNot e' -> 
      let tcExpr = tc_expression env e' in
      (match tcExpr with
             BoolExpr(exp1) -> BoolExpr(LogNot(exp1))
           | _ -> ErrorMsg.error ("not expression didn't typecheck \n");
                  raise ErrorMsg.Error)
  | UntypedPostElabTernary(e1, e2, e3) ->
      let tcExpr1 = tc_expression env e1 in
      let tcExpr2 = tc_expression env e2 in
      let tcExpr3 = tc_expression env e3 in
      (match (tcExpr1, tcExpr2, tcExpr3) with
             (BoolExpr(exp1), IntExpr(exp2), IntExpr(exp3)) -> IntExpr(IntTernary(exp1, exp2, exp3))
           | (BoolExpr(exp1), BoolExpr(exp2), BoolExpr(exp3)) -> BoolExpr(BoolTernary(exp1, exp2, exp3))
           | _ -> (ErrorMsg.error ("ternary expression didn't typecheck \n");
                  raise ErrorMsg.Error))
  | UntypedPostElabFunCall(i, argList) -> 
      (match M.find env i with
             Some(funcType, funcParams, _, _) -> 
               (match funcType with
                      INT -> 
                        let typedArgs = List.map tc_expression env argList in
                        if (argsMatch typedArgs funcParams) then IntFunCall(i, typedArgs)
                        else ErrorMsg.error ("parameters don't typecheck \n");
                             raise ErrorMsg.Error
                    | BOOL -> 
                        let typedArgs = List.map tc_expression env argList in
                        if (argsMatch typedArgs funcParams) then BoolFunCall(i, typedArgs)
                        else ErrorMsg.error ("parameters don't typecheck \n");
                             raise ErrorMsg.Error)
                    | VOID -> "????????" (* assign to an unused variable "\\something" just like before *)
           | _ -> ErrorMsg.error ("function doesn't exist \n");
                  raise ErrorMsg.Error)

(* funcName -> (funcType, list of types of funcParams, isDefined, isExternal) *)
let rec tc_header headerEnv (header : untypedPostElabAST) = 
  match header with
        [] -> ()
      | fdecl :: fdecls -> 
          (match fdecl with
                 UntypedPostElabFunDecl(funcType, funcName, funcParams) -> 
                   (let newHeaderEnv = M.add headerEnv funcName (funcType, (List.map (fun (c, i) -> c) funcParams), false, true) in
                    tc_header newHeaderEnv fdecls)
               | _ -> (ErrorMsg.error ("function def'n or typedef in header file \n");
                       raise ErrorMsg.Error))

let rec matchParamListTypes (paramTypes : c0type list) (params : param list) =
  match (paramTypes, params) with
        ([], []) -> true
      | ([], p :: ps) -> false
      | (p :: ps, []) -> false
        (* what is p2??? *)
      | (p :: ps, (typee, _) :: newParams) -> if (p = INT && typee = INT) || (p = BOOL && p2 = BOOL) 
                                  then matchParamListTypes ps newParams else false

let matchFuncTypes funcType1 funcType2 =
  match (funcType1, funcType2) with
        (INT, INT) -> true
      | (BOOL, BOOL) -> true
      | (VOID, VOID) -> true
      | _ -> false

let lowestTypedefType (typedefType : c0type) tbl =
  match typedefType with
        TypedefType(identifier) -> 
          (match M.find tbl identifier with
                 Some(anotherType) -> anotherType
             (* The second case here is never used because all Somes are already matched...what is it for? *)
               | Some _ -> ErrorMsg.error ("not a typedef \n");
                           raise ErrorMsg.Error
               | None -> ErrorMsg.error ("undefined typedef \n");
                         raise ErrorMsg.Error)
      | _ -> typedefType

let rec tc_prog progEnv (prog : untypedPostElabAST) (typedAST : typedPostElabAST) =
  match prog with
        [] -> typedAST
      | gdecl :: gdecls ->
          (match gdecl with
                 UntypedPostElabFunDecl(funcType, funcName, funcParams) -> 
                   (match M.find progEnv funcName with
                          Some (fType, paramTypes, isDefined, isExternal) ->
                            if isExternal
                            then (ErrorMsg.error ("trying to redeclare func that was declared in header \n");
                                 raise ErrorMsg.Error)
                            else
                              if not ((matchFuncTypes fType funcType) && (matchParamListTypes paramTypes funcParams))
                              then (ErrorMsg.error ("trying to redeclare func with wrong func type/param types \n");
                                   raise ErrorMsg.Error)
                              else
                                tc_prog progEnv gdecls typedAST
                        | Some _ -> ErrorMsg.error ("function names can't shadow typedefs \n");
                                    raise ErrorMsg.Error
                        | None -> 
                            let newProgEnv = M.add progEnv funcName 
                            (lowestTypedefType funcType typeEnv, (List.map (fun (c, i) -> c) funcParams), false, false) in
                            tc_prog newProgEnv gdecls typedAST)
               | UntypedPostElabFunDef(funcType, funcName, funcParams, funcBody) -> 
                   (match M.find progEnv funcName with
                          Some (fType, paramTypes, isDefined, isExternal) ->
                            if (isDefined || isExternal)
                            then (ErrorMsg.error ("trying to define already defined OR external function \n");
                                 raise ErrorMsg.Error)
                            else
                              if not ((matchFuncTypes fType funcType) && (matchParamListTypes paramTypes funcParams))
                              then (ErrorMsg.error ("trying to redeclare func with wrong func type/param types \n");
                                   raise ErrorMsg.Error)
                              else
                                let newProgEnv = M.add progEnv funcName 
                                  (fType, paramTypes, true, false) in
                                let funcEnv = Core.Std.String.Map.empty in
                                (* Need to pass in overall environment to tc statments!! BILLY DO THIS *)
                                let typeCheckedBlock = tc_statements funcEnv funcBody fType false [] in
                                  tc_prog newProgEnv gdecls (TypedPostElabFunDef(funcType, funcName, 
                                  funcParams, List.rev typeCheckedBlock)::typedAST)
                         | None -> (if funcName = "main" && 
                                        ((List.length funcParams > 0) || lowestTypedefType(funcType) != INT)
                                    then (ErrorMsg.error ("trying to illegally define main \n");
                                         raise ErrorMsg.Error)
                                    else
                                      (let newProgEnv = M.add progEnv funcName 
                                          (funcType, (List.map (fun (c, i) -> c) funcParams), true, false) in
                                      let funcEnv = Core.Std.String.Map.empty in
                                      let typeCheckedBlock = tc_statements funcEnv funcBody false [] in
                                      tc_prog newProgEnv gdecls (TypedPostElabFunDef(funcType, funcName, 
                                      funcParams, List.rev typeCheckedBlock)::typedAST))))
               | UntypedPostElabTypedef(typeDefType, typeDefName) -> 
                   (match M.find progEnv typeDefName with
                          Some _ -> ErrorMsg.error ("cannot shadow previously declared typedef/func names \n");
                                    raise ErrorMsg.Error
                        | None -> let newProgEnv = M.add progEnv typeDefName (lowestTypedefType(typeDefType)) in
                                  tc_prog newProgEnv gdecls typedAST)  

and tc_statements env (untypedAST : untypedPostElabAST) (fRetType : c0type)
                          (ret : bool) (typedAST : typedPostElabAST) =
  match untypedAST with
    [] -> (ret, env, typedAST)
  | A.UntypedPostElabBlock(blockStmts)::stmts ->
      let (blockRet, blockEnv, blockAst) = tc_statements env blockStmts fRetType ret [] in
      let newRet = ret || blockRet in
      (* We have returned if we return otherwise, or if the block returns *)
      (* Declarations from the block don't count, but initializations do,
         similar to if/else *)
      let newenv = M.mapi env (fun ~key:id -> (fun ~data:value ->
            (match M.find blockEnv id with
                Some (typee, isInit) -> (typee, isInit)
              | None -> assert(false) (* everything in env should be in blockEnv *)))) in
      tc_statements newenv stmts fRetType newRet (blockAst @ typedAST)
  | A.UntypedPostElabDecl(id, typee)::stms ->
      (match M.find env id with
                   Some _ -> (ErrorMsg.error ("redeclared variable " ^ id ^ "\n");
                              raise ErrorMsg.Error)
                 | None -> (let newMap = M.add env id (typee, false) in 
                    tc_statements newMap stms fRetType ret ((TypedPostElabDecl(id, typee)) :: typedAST)))
  | A.UntypedPostElabAssignStmt(id, e)::stms ->
       (let tcExpr = tc_expression env e in
          (match M.find env id with (* it's declared, good *)
                 Some(typee, _) -> 
                   (match (tcExpr, typee) with
                          (IntExpr(_), INT) -> 
                            let newMap = M.add env id (typee, true) in  
                            tc_statements newMap stms fRetType ret ((TypedPostElabAssignStmt(id, tcExpr)) :: typedAST)
                        | (BoolExpr(_), BOOL) -> 
                            let newMap = M.add env id (typee, true) in              
                            tc_statements newMap stms fRetType ret ((TypedPostElabAssignStmt(id, tcExpr)) :: typedAST)
                        | _ -> (ErrorMsg.error ("assignment expression didn't typecheck \n");
                               raise ErrorMsg.Error))
               | None -> (if ((isValidVarDecl id))
                          then (let newMap = M.add env id (INT, true) in              
                                tc_statements newMap stms fRetType ret ((TypedPostElabAssignStmt(id, tcExpr)) :: typedAST))
                          else (ErrorMsg.error ("undeclared test variable " ^ id ^ "\n");
                                raise ErrorMsg.Error))))
  | A.UntypedPostElabIf(e, ast1, ast2)::stms -> 
      let tcExpr = tc_expression env e in
      (match tcExpr with
             BoolExpr(exp1) -> 
               let (ret1, env1, newast1) = tc_statements env ast1 fRetType ret [] in
               let (ret2, env2, newast2) = tc_statements env ast2 fRetType ret [] in
               let newret = if ret then ret else (ret1 && ret2) in
               let newenv = M.mapi env (fun ~key:id -> (fun ~data:value ->
                                             (match (M.find env1 id, M.find env2 id) with
                                                    (Some (typee1, isInit1), Some (typee2, isInit2)) -> 
                                                                (typee1, isInit1 && isInit2)
                                                  | (_, _) -> value))) in
               tc_statements newenv stms fRetType newret 
               ((TypedPostElabIf(exp1, List.rev newast1, List.rev newast2)) :: typedAST)
           | _ -> ErrorMsg.error ("if expression didn't typecheck\n");
                  raise ErrorMsg.Error)
  | A.UntypedPostElabWhile(e, ast1, untypedInitAst)::stms -> 
      let (_, newenv2, newast2) = tc_statements env untypedInitAst fRetType ret [] in
      let tcExpr = tc_expression newenv2 e in
      (match tcExpr with
             BoolExpr(exp1) ->                
               let (_, _, newast1) = tc_statements newenv2 ast1 fRetType ret [] in
               tc_statements env stms fRetType ret 
               ((TypedPostElabWhile(exp1, List.rev newast1)) :: (newast2 @ typedAST))
           | _ -> ErrorMsg.error ("while expression didn't typecheck\n");
                  raise ErrorMsg.Error)
  | A.UntypedPostElabReturn(e)::stms -> 
      let tcExpr = tc_expression env e in
      (* apparently all variables defined before the first return
         get to be treated as initialized...although those declared
         after don't *)
      let newMap = M.map env (fun (typee, _) -> (typee, true)) in
      (match tcExpr with
             IntExpr(exp1) -> 
               (match fRetType with
                      INT -> tc_statements newMap stms fRetType true ((TypedPostElabReturn(exp1)) :: typedAST)
                    | _ -> ErrorMsg.error ("return expression didn't typecheck\n");
                           raise ErrorMsg.Error)
           | BoolExpr(exp1) -> 
               (match fRetType with
                      BOOL -> tc_statements newMap stms fRetType true ((TypedPostElabReturn(exp1)) :: typedAST)
                    | _ -> ErrorMsg.error ("return expression didn't typecheck\n");
                           raise ErrorMsg.Error))
  | A.UntypedPostElabVoidReturn::stms -> 
      (match fRetType with
             VOID -> tc_statements newMap stms fRetType true (TypedPostElabVoidReturn :: typedAST)
           | _ -> ErrorMsg.error ("non-void function must return non-void value \n");
                  raise ErrorMsg.Error)
  | A.UntypedPostElabAssert(e)::stms ->
      let tcExpr = tc_expression env e in
      (match tcExpr with
             BoolExpr(expr1) -> tc_statements newMap stms fRetType ret (Abort :: TypedPostElabAssert(tcExpr) :: typedAST)
           | _ -> ErrorMsg.error ("assert must have boolean expression \n");
                  raise ErrorMsg.Error)
       
and typecheck ((untypedHeaderAST, untypedProgAST) : untypedPostElabOverallAST) =
  let environment = Core.Std.String.Map.empty in
  let typedHeaderAST = tc_prog environment untypedHeaderAST  in
  let typedProgAST = tc_prog environment untypedProgAST  in
  (List.rev (typedProgAST @ typedHeaderAST))

(* Need to create env for typedefs; this is separate from progEnv, which is the function environments. *)
