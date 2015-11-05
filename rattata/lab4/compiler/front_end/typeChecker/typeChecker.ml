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
open PrintDatatypes
open PrintASTs
open TcExprs

(* funcName -> (funcType, list of types of funcParams, isDefined, isExternal) *)
let rec tc_header (header : untypedPostElabAST) (typedAST : typedPostElabAST) = 
  match header with
        [] -> typedAST
      | fdecl :: fdecls -> 
          (match fdecl with
                 UntypedPostElabTypedef(t, i) -> 
                 if (isNestedVoidPtr t) then
                    (ErrorMsg.error ("can't typedef void or nested void ptr\n");
                                raise ErrorMsg.Error)
                 else
                   (match (M.find !typedefMap i, M.find !functionMap i) with
                          (None, None) -> 
                            let () = typedefMap := M.add !typedefMap i 
                                        (lowestTypedefType t) in
                            tc_header fdecls typedAST
                        | _ -> (ErrorMsg.error ("typedef name already used\n");
                                raise ErrorMsg.Error))
               | UntypedPostElabFunDecl(funcType, funcName, funcParams) ->
                   let nameTable = Core.Std.String.Map.empty in
                   if not (uniqueParamNames funcParams nameTable) then 
                      (ErrorMsg.error ("bad param names \n");
                                raise ErrorMsg.Error)
                   else
                   if List.exists (fun (typee, id) -> (isNestedVoidPtr typee)) funcParams then
                      (ErrorMsg.error ("can't have void as param type\n");
                                raise ErrorMsg.Error)
                   else
                   if (areAnyFuncParamsStructs funcParams)
                   then
                      (ErrorMsg.error ("can't have structs as param type \n");
                                raise ErrorMsg.Error)
                   else
                   (match (M.find !typedefMap funcName, 
                           M.find !functionMap funcName) with
                          (Some _, _) -> 
                            (ErrorMsg.error ("function name already used\n");
                             raise ErrorMsg.Error)
                        | (None, Some (fType, paramTypes, isDefined, isExternal)) ->
                            if not ((matchFuncTypes fType funcType) && 
                                    (matchParamListTypes paramTypes funcParams))
                            then (ErrorMsg.error ("redeclared func 
                                                    w/ wrong type/param types \n");
                                  raise ErrorMsg.Error)
                            else
                              tc_header fdecls typedAST
                        | (None, None) -> 
                            (let () = functionMap := M.add !functionMap funcName 
                             (lowestTypedefType funcType, 
                             (List.map (fun (c, i) -> lowestTypedefType c) funcParams), true, true) in
                             tc_header fdecls typedAST))
               | UntypedPostElabStructDecl(structName) ->
                   (match M.find !structMap structName with
                          None -> 
                            let () = structMap := M.add !structMap structName (ref Core.Std.String.Map.empty, false) in
                            tc_header fdecls typedAST
                        | _ -> (ErrorMsg.error ("struct name already used\n");
                                raise ErrorMsg.Error))
               | UntypedPostElabStructDef(structName, fields) ->
                   let nameTable = Core.Std.String.Map.empty in
                   if (not (uniqueFieldNames fields nameTable) || not (areStructFieldsWellDefined fields)) then 
                      (ErrorMsg.error ("bad field names/field is void or void ptr or undefined struct\n");
                                raise ErrorMsg.Error)
                   else
                   (match M.find !structMap structName with
                          Some (fieldMap, false) -> (* declared but undefined struct *)
                            let newFields = List.map(fun (fieldType, fieldName) -> 
                                                    (lowestTypedefType fieldType, fieldName)) fields in
                            let () = List.iter(fun (fieldType, fieldName) -> 
                              fieldMap := M.add !fieldMap fieldName fieldType) newFields in
                            let () = structMap := M.add !structMap structName (fieldMap, true) in
                            tc_header fdecls (TypedPostElabStructDef(structName, newFields)::typedAST) 
                        | Some (_, true) -> (* already defined struct *) 
                            (ErrorMsg.error ("redefining struct " ^ structName ^ "\n");
                             raise ErrorMsg.Error)
                        | None -> 
                            let () = structMap := M.add !structMap structName (ref Core.Std.String.Map.empty, true) in
                            let Some (fieldMap, _) = M.find !structMap structName in
                            let newFields = List.map(fun (fieldType, fieldName) -> 
                                                    (lowestTypedefType fieldType, fieldName)) fields in
                            let () = List.iter(fun (fieldType, fieldName) -> 
                              fieldMap := M.add !fieldMap fieldName fieldType) newFields in
                            let () = structMap := M.add !structMap structName (fieldMap, true) in
                            tc_header fdecls (TypedPostElabStructDef(structName, newFields)::typedAST)) 
               | _ -> (ErrorMsg.error ("func def'n in header file \n");
                       raise ErrorMsg.Error))

let rec init_func_env params = 
  match params with
        [] -> Core.Std.String.Map.empty
      | (typee, id)::ps ->  M.add (init_func_env ps) id (lowestTypedefType typee, true)

let rec tc_prog (prog : untypedPostElabAST) (typedAST : typedPostElabAST) =
  match prog with
        [] -> if not (H.length declaredAndUsedButUndefinedFunctionTable = 0)
              then (ErrorMsg.error ("you got some used but undeclared functions\n");
                    raise ErrorMsg.Error)
              else 
                (match M.find !functionMap "main" with
                       Some(_, _, isDefined, isExternal) -> 
                        if not isDefined then 
                          (ErrorMsg.error ("main undefined\n");
                           raise ErrorMsg.Error)
                        else typedAST
                     | None -> (ErrorMsg.error ("main undeclared\n");
                                raise ErrorMsg.Error))
      | gdecl :: gdecls ->
          (match gdecl with
                 UntypedPostElabFunDecl(funcType, funcName, funcParams) -> 
                   let nameTable = Core.Std.String.Map.empty in
                   if not (uniqueParamNames funcParams nameTable) then 
                      (ErrorMsg.error ("bad param names \n");
                                raise ErrorMsg.Error)
                   else
                   if List.exists (fun (typee, id) -> (isNestedVoidPtr typee)) funcParams then
                      (ErrorMsg.error ("can't have void as param type \n");
                                raise ErrorMsg.Error)
                   else
                   if areAnyFuncParamsStructs funcParams
                   then
                      (ErrorMsg.error ("can't have structs as param type \n");
                                raise ErrorMsg.Error)
                   else
                   (match (M.find !typedefMap funcName, M.find !functionMap funcName) with
                          (Some _, _) -> 
                            (ErrorMsg.error ("trying to shadow used name\n");
                             raise ErrorMsg.Error)
                        | (None, Some (fType, paramTypes, isDefined, isExternal)) ->
                              (if not ((matchFuncTypes fType funcType) && 
                                      (matchParamListTypes paramTypes funcParams))
                              then (ErrorMsg.error ("trying to redeclare func with wrong func type/param types \n");
                                   raise ErrorMsg.Error)
                              else
                                tc_prog gdecls typedAST)
                        | (None, None) -> 
                            let () = functionMap := M.add !functionMap funcName 
                            (lowestTypedefType funcType, 
                            (List.map (fun (c, i) -> lowestTypedefType c) funcParams), 
                            false, false) in
                            tc_prog gdecls typedAST)
               | UntypedPostElabFunDef(funcType, funcName, funcParams, funcBody) ->
                   let nameTable = Core.Std.String.Map.empty in
                   if not (uniqueParamNames funcParams nameTable) then 
                      (ErrorMsg.error ("bad param names \n");
                                raise ErrorMsg.Error)
                   else
                   if List.exists (fun (typee, id) -> typee = VOID) funcParams then
                      (ErrorMsg.error ("can't have void or structs as param types \n");
                                raise ErrorMsg.Error)
                   else
                   if areAnyFuncParamsStructs funcParams
                   then
                      (ErrorMsg.error ("can't have structs as param type \n");
                                raise ErrorMsg.Error)
                   else
                   (match (M.find !typedefMap funcName, M.find !functionMap funcName) with
                          (Some _, _) -> (ErrorMsg.error ("trying to shadow used name \n");
                                          raise ErrorMsg.Error)
                        | (None, Some (fType, paramTypes, isDefined, isExternal)) ->
                            (if (isDefined || isExternal)
                            then (ErrorMsg.error ("trying to define predefined/ external function \n");
                                 raise ErrorMsg.Error)
                            else
                              if not ((matchFuncTypes fType funcType) && 
                                      (matchParamListTypes paramTypes funcParams))
                              then
                                (ErrorMsg.error ("trying to define func with wrong func type/param types \n");
                                   raise ErrorMsg.Error)
                              else
                                let () = H.remove declaredAndUsedButUndefinedFunctionTable funcName in
                                let () = functionMap := M.add !functionMap funcName 
                                  (fType, paramTypes, true, false) in
                                let funcVarMap = init_func_env funcParams in
                                let newFuncParams = List.map(fun (t,i) -> (lowestTypedefType t, i)) funcParams in
                                (* Make sure the function returns! *)
                                let (ret, _, typeCheckedBlock) = 
                                  tc_statements funcVarMap 
                                  funcBody (lowestTypedefType fType) false [] in
                                 if ((not ret) && (not (funcType = VOID))) then
                                  (ErrorMsg.error ("non-void functions must return \n");
                                   raise ErrorMsg.Error) else
                                let newFuncName = "_c0_" ^ funcName in
                                (* We're supposed to call internal functions with the prefix _c0_. I'm doing it
                                   here because we know exactly which are internal/external at this point *)
                                let newFuncType = lowestTypedefType funcType in
                                  tc_prog gdecls (TypedPostElabFunDef(newFuncType, newFuncName, 
                                  newFuncParams, List.rev typeCheckedBlock)::typedAST))
                         | (None, None) -> 
                             (if funcName = "main" && 
                                 ((List.length funcParams > 0) || lowestTypedefType funcType != INT)
                              then (ErrorMsg.error ("trying to illegally define main \n");
                                    raise ErrorMsg.Error)
                              else
                                (let () = functionMap := M.add !functionMap funcName 
                                  (lowestTypedefType funcType, 
                                  (List.map (fun (c, i) -> lowestTypedefType c) funcParams), 
                                  true, false) in
                                 let funcVarMap = init_func_env funcParams in
                                 (* Make sure the function returns! *)
                                 let newFuncParams = List.map(fun (t,i) -> (lowestTypedefType t, i)) funcParams in
                                 let (ret, _, typeCheckedFuncBody) = 
                                   tc_statements funcVarMap 
                                   funcBody (lowestTypedefType funcType) false [] in
                                 if ((not ret) && (not (funcType = VOID))) then
                                  (ErrorMsg.error ("non-void functions must return \n");
                                   raise ErrorMsg.Error)
                                 else
                                  let newFuncName = "_c0_" ^ funcName in
                                  tc_prog gdecls (TypedPostElabFunDef(lowestTypedefType funcType, newFuncName, 
                                  newFuncParams, List.rev typeCheckedFuncBody)::typedAST))))
               | UntypedPostElabTypedef(typeDefType, typeDefName) -> 
                    if (isNestedVoidPtr typeDefType) then
                    (ErrorMsg.error ("can't typedef voids\n");
                     raise ErrorMsg.Error)
                    else
                   (match (M.find !typedefMap typeDefName, M.find !functionMap typeDefName) with
                          (None, None) -> 
                            let () = typedefMap := M.add !typedefMap typeDefName 
                                        (lowestTypedefType typeDefType) in
                            tc_prog gdecls typedAST
                        | _ -> (ErrorMsg.error ("cannot shadow previously declared typedef/func names \n");
                                raise ErrorMsg.Error))
               | UntypedPostElabStructDecl(structName) ->
                   (match M.find !structMap structName with
                          None -> 
                            let () = structMap := M.add !structMap structName (ref Core.Std.String.Map.empty, false) in
                            tc_prog gdecls typedAST
                        | _ -> tc_prog gdecls typedAST) (* (ErrorMsg.error ("redeclaring struct " ^ structName ^ "\n");
                                raise ErrorMsg.Error) *)
               | UntypedPostElabStructDef(structName, fields) ->
                   let nameTable = Core.Std.String.Map.empty in
                   if (not (uniqueFieldNames fields nameTable) || not (areStructFieldsWellDefined fields)) then 
                      (ErrorMsg.error ("bad field names/field is void or a struct that hasn't been defined \n");
                                raise ErrorMsg.Error)
                   else
                   (match M.find !structMap structName with
                          Some (fieldMap, false) -> (* declared but undefined struct *)
                            let newFields = List.map(fun (fieldType, fieldName) -> 
                                                    (lowestTypedefType fieldType, fieldName)) fields in
                            let () = List.iter(fun (fieldType, fieldName) -> 
                              fieldMap := M.add !fieldMap fieldName fieldType) newFields in
                            let () = structMap := M.add !structMap structName (fieldMap, true) in
                            tc_prog gdecls (TypedPostElabStructDef(structName, newFields)::typedAST)
                        | Some (_, true) -> (* already defined struct *) 
                            (ErrorMsg.error ("redefining struct " ^ structName ^ "\n");
                             raise ErrorMsg.Error)
                        | None -> 
                            let () = structMap := M.add !structMap structName (ref Core.Std.String.Map.empty, true) in
                            let Some (fieldMap, _) = M.find !structMap structName in
                            let newFields = List.map(fun (fieldType, fieldName) -> 
                                                    (lowestTypedefType fieldType, fieldName)) fields in
                            let () = List.iter(fun (fieldType, fieldName) -> 
                              fieldMap := M.add !fieldMap fieldName fieldType) newFields in
                            let () = structMap := M.add !structMap structName (fieldMap, true) in
                            tc_prog gdecls (TypedPostElabStructDef(structName, newFields)::typedAST)))

(* varMap is the map of variables within the function body *) 
(* funcRetType is the return type of the function *)         
and tc_statements varMap (untypedBlock : untypedPostElabBlock) (funcRetType : c0type)
                          (ret : bool) (typedBlock : typedPostElabBlock) =
  match untypedBlock with
    [] -> (ret, varMap, typedBlock)
  | A.UntypedPostElabBlock(blockStmts)::stmts ->
      let (blockRet, blockVarMap, blockBlock) = tc_statements varMap blockStmts funcRetType ret [] in
      let newRet = (ret || blockRet) in
      (* We have returned if we return otherwise, or if the block returns *)
      (* Declarations from the block don't count, but initializations do,
         similar to if/else *)
      let newVarMap = M.mapi varMap (fun ~key:id -> (fun ~data:value ->
            (match M.find blockVarMap id with
                Some (typee, isInit) -> (typee, isInit)
              | None -> assert(false) (* everything in varMap should be in blockVarMap *)))) in
      tc_statements newVarMap stmts funcRetType newRet (blockBlock @ typedBlock)
  | A.UntypedPostElabInitDecl(id, typee, e)::stms ->
      (* necessary for statements of the form "int f = f();" *)
       (let (tcExpr, exprType) = tc_expression varMap e in
        let actualDeclType = lowestTypedefType typee in
        if (isNestedVoidPtr actualDeclType) 
        then 
          (ErrorMsg.error ("vars can't be void or arbitrarily nested void ptrs\n");
                       raise ErrorMsg.Error)
        else
        (match actualDeclType with
              Struct(_) -> (ErrorMsg.error ("vars can't be structs\n");
                            raise ErrorMsg.Error)
            | _ -> 
              (match (M.find !typedefMap id, M.find varMap id) with
                 (None, None) -> 
                    let newTypedAST = (TypedPostElabAssignStmt(TypedPostElabVarLVal(id), EQ, tcExpr)
                                    :: TypedPostElabDecl(id, actualDeclType)
                                    :: typedBlock) in
                    if matchTypes exprType actualDeclType then
                      let newVarMap = M.add varMap id (actualDeclType, true) in 
                      tc_statements newVarMap 
                      stms funcRetType ret newTypedAST
                    else (ErrorMsg.error ("\nLHS type: " ^ c0typeToString(actualDeclType) ^"\nRHS type: "
                                    ^ c0typeToString(exprType));
                          raise ErrorMsg.Error)
               | _ -> (ErrorMsg.error ("var names can't shadow func/typedef/declared var names\n");
                       raise ErrorMsg.Error))))
  | A.UntypedPostElabDecl(id, typee)::stms ->
      (match (M.find !typedefMap id, M.find varMap id) with
             (None, None) -> 
               (let actualType = lowestTypedefType typee in
                if (isNestedVoidPtr actualType) 
                then 
                  (ErrorMsg.error ("vars can't be void or arbitrarily nested void ptrs\n");
                       raise ErrorMsg.Error)
                else
                (match actualType with
                      Struct(_) -> (ErrorMsg.error ("vars can't be structs\n");
                                    raise ErrorMsg.Error)
                    | _ -> let newVarMap = M.add varMap id (actualType, false) in 
                 tc_statements newVarMap 
                 stms funcRetType ret ((TypedPostElabDecl(id, actualType)) :: typedBlock)))
           | _ -> (ErrorMsg.error ("var names can't shadow func/typedef/declared var names\n");
                   raise ErrorMsg.Error))
  | A.UntypedPostElabAssignStmt(lval, op, e)::stms ->
       (let (tcExpr, exprType) = tc_expression varMap e in
        let (typedLVal, lvalType) = tc_lval varMap lval in
        if matchTypes exprType (lowestTypedefType lvalType ) && notAStruct exprType
        then
          (match op with
                 EQ -> 
                   (match typedLVal with
                      (* we don't have to add lvals to our varMap unless they are vars *)
                      TypedPostElabVarLVal(id) -> 
                        let newVarMap = M.add varMap id (lvalType, true) in
                        tc_statements newVarMap stms 
                        funcRetType ret ((TypedPostElabAssignStmt(typedLVal, op, tcExpr))::typedBlock)
                    | _ -> 
                       tc_statements varMap stms 
                       funcRetType ret ((TypedPostElabAssignStmt(typedLVal, op, tcExpr))::typedBlock))
               | _ -> 
                  (match typedLVal with
                      TypedPostElabVarLVal(id) -> 
                        (match M.find varMap id with
                               Some(typee, isInit) -> 
                                 if isInit && typee = INT
                                 then tc_statements varMap stms 
                                      funcRetType ret ((TypedPostElabAssignStmt(typedLVal, op, tcExpr))::typedBlock)
                                 else (ErrorMsg.error ("wrong type or lval uninitialized\n");
                                       raise ErrorMsg.Error)
                             | _ -> (ErrorMsg.error ("lval undeclared\n");
                                     raise ErrorMsg.Error))
                    | _ -> 
                        if lvalType = INT
                        then tc_statements varMap stms 
                          funcRetType ret ((TypedPostElabAssignStmt(typedLVal, op, tcExpr))::typedBlock)
                        else (ErrorMsg.error ("can't use int assignOp on non-int expr\n");
                              raise ErrorMsg.Error)))
        else
        (ErrorMsg.error ("\nLHS type: " ^ c0typeToString(lvalType) ^ "\nRHS type: " ^ c0typeToString(exprType) ^ "\n");
                               raise ErrorMsg.Error))
  | A.UntypedPostElabIf(e, block1, block2)::stms -> 
      let (tcExpr, _) = tc_expression varMap e in
      (match tcExpr with
             BoolExpr(exp1) -> 
               let (ret1, varMap1, tcBlock1) = tc_statements varMap block1 funcRetType ret [] in
               let (ret2, varMap2, tcBlock2) = tc_statements varMap block2 funcRetType ret [] in
               let newret = if ret then ret else (ret1 && ret2) in
               let newVarMap = M.mapi varMap (fun ~key:id -> (fun ~data:value ->
                                             (match (M.find varMap1 id, M.find varMap2 id) with
                                                    (Some (typee1, isInit1), Some (typee2, isInit2)) -> 
                                                                (typee1, isInit1 && isInit2)
                                                  | (_, _) -> value))) in
               tc_statements newVarMap stms funcRetType newret 
               ((TypedPostElabIf(exp1, List.rev tcBlock1, List.rev tcBlock2)) :: typedBlock)
           | _ -> ErrorMsg.error ("if expression didn't typecheck\n");
                  raise ErrorMsg.Error)
  | A.UntypedPostElabWhile(e, block1, untypedInitBlock)::stms -> 
      let (_, newVarMap2, tcBlock2) = tc_statements varMap untypedInitBlock funcRetType ret [] in
      let (tcExpr, _) = tc_expression newVarMap2 e in
      (match tcExpr with
             BoolExpr(exp1) ->                
               let (_, _, tcBlock1) = tc_statements newVarMap2 block1 funcRetType ret [] in
               tc_statements varMap stms funcRetType ret 
               ((TypedPostElabWhile(exp1, List.rev tcBlock1)) :: (tcBlock2 @ typedBlock))
           | _ -> ErrorMsg.error ("while expression didn't typecheck\n");
                  raise ErrorMsg.Error)
  | A.UntypedPostElabReturn(e)::stms ->
      let (tcExpr, exprType) = tc_expression varMap e in
      if not (typeNotLarge exprType)
      then
        (ErrorMsg.error ("return expr can't be struct\n");
            raise ErrorMsg.Error)
      else
      (* apparently all variables declared before the first return
         get to be treated as initialized...although those declared
         after don't *)
      let newVarMap = M.map varMap (fun (typee, _) -> (typee, true)) in
      if matchTypes (lowestTypedefType exprType) funcRetType 
      then tc_statements newVarMap 
          stms funcRetType true ((TypedPostElabReturn(tcExpr)) :: typedBlock) 
      else (ErrorMsg.error ("return expr not same as func ret type\n");
            raise ErrorMsg.Error)      
  | A.UntypedPostElabVoidReturn::stms ->
      (* Same as above, variables declared are treated as initalized in
         unreachable code... *)
      let newVarMap = M.map varMap (fun (typee, _) -> (typee, true)) in
      (match funcRetType with
             VOID -> tc_statements newVarMap 
                     stms funcRetType true (TypedPostElabVoidReturn :: typedBlock)
           | _ -> (ErrorMsg.error ("non-void function must return non-void value \n");
                  raise ErrorMsg.Error))
  | A.UntypedPostElabAssert(e)::stms ->
      let (tcExpr, _) = tc_expression varMap e in
      (match tcExpr with
             BoolExpr(expr1) -> tc_statements varMap 
                                stms funcRetType ret (TypedPostElabAssert(expr1) :: typedBlock)
           | _ -> (ErrorMsg.error ("assert must have bool expr\n");
                  raise ErrorMsg.Error))
  | A.UntypedPostElabExprStmt(e)::stms ->
      let (tcExpr, exprType) = tc_expression varMap e in
      (match exprType with
            Struct(_) -> (ErrorMsg.error ("type can't be a struct\n");
                          raise ErrorMsg.Error) 
          | _ ->
      (match tcExpr with
             VoidExpr(stmt) -> tc_statements varMap 
                               stms funcRetType ret (stmt :: typedBlock)
           | _ -> tc_statements varMap 
                  stms funcRetType ret 
                  (TypedPostElabAssignStmt(TypedPostElabVarLVal(GenUnusedID.create()), EQ, tcExpr) :: typedBlock)))
       
and typecheck ((untypedProgAST, untypedHeaderAST) : untypedPostElabOverallAST) =
  
  let typedHeaderAST = tc_header untypedHeaderAST [] in
  (* the main function is considered to always be declared at the top of the main file!
     It still needs to be defined eventually of course *)
  let () = (match M.find !functionMap "main" with
                   Some _ -> (ErrorMsg.error ("main cannot be declared in header \n");
                              raise ErrorMsg.Error)
                 | None -> ()) in
  let () = functionMap := M.add !functionMap "main" (INT, [], false, false) in
  (* type is INT, no params, isDefined = false, isExternal = false *)
  let typedProgAST = tc_prog untypedProgAST [] in
  List.rev (typedProgAST @ typedHeaderAST)

(* Note: please check all of the typing rules for structs, becayse they're
   pretty complicated. Check out the lecture notes on structs, as well
   as the handout. *)
