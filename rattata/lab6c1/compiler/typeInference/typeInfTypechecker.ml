(* L6 Compiler
 * TypeChecker for Type Inference
 * Authors: Ben Plaut, William Tong
 *)

module A = Ast
open TypeInfAst
open Datatypesv1
module M = Core.Std.Map
open String
open PrintDatatypes
open TypeInfPrintASTS
open TypeInfTCExprs

(* funcName -> (funcType, list of types of funcParams, isDefined, isExternal) *)
let rec tc_header (header : A.untypedPostElabAST) (typedAST : typedPostElabAST) = 
  match header with
        [] -> typedAST
      | fdecl :: fdecls -> 
          (match fdecl with
                 A.UntypedPostElabTypedef(t, i) -> 
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
               | A.UntypedPostElabFunDecl(funcType, funcName, funcParams) ->
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
                   if not (funcType = VOID) && (isNestedVoidPtr funcType)
                   then
                     (ErrorMsg.error ("func ret type can't be a void ptr\n");
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
               | A.UntypedPostElabStructDecl(structName) ->
                   (match M.find !structMap structName with
                          None -> 
                            let () = structMap := M.add !structMap structName (ref Core.Std.String.Map.empty, false) in
                            tc_header fdecls typedAST
                        | _ -> (ErrorMsg.error ("struct name already used\n");
                                raise ErrorMsg.Error))
               | A.UntypedPostElabStructDef(structName, fields) ->
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

let rec tc_prog (prog : A.untypedPostElabAST) (typedAST : typedPostElabAST) =
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
                 A.UntypedPostElabFunDecl(funcType, funcName, funcParams) -> 
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
                   if not (funcType = VOID) && (isNestedVoidPtr funcType)
                   then
                     (ErrorMsg.error ("func ret type can't be a void ptr\n");
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
               | A.UntypedPostElabFunDef(funcType, funcName, funcParams, funcBody) ->
                   let nameTable = Core.Std.String.Map.empty in
                   if not (uniqueParamNames funcParams nameTable) then 
                      (ErrorMsg.error ("bad param names \n");
                                raise ErrorMsg.Error)
                   else
                   if List.exists (fun (typee, id) -> typee = VOID) funcParams then
                      (ErrorMsg.error ("can't have void as param types \n");
                                raise ErrorMsg.Error)
                   else
                   if areAnyFuncParamsStructs funcParams
                   then
                      (ErrorMsg.error ("can't have structs as param type \n");
                                raise ErrorMsg.Error)
                   else
                   if not (funcType = VOID) && (isNestedVoidPtr funcType)
                   then
                     (ErrorMsg.error ("func ret type can't be a void ptr\n");
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
                              if (not (matchFuncTypes fType funcType) ||  
                                 not (matchParamListTypes paramTypes funcParams)) && not (isAlpha funcType)
                              then
                                (ErrorMsg.error ("func type in map: " ^ c0typeToString fType ^ 
                                "\nfunc type in def: " ^ c0typeToString funcType ^ "\n");
                                   raise ErrorMsg.Error)
                              else
                                let () = H.remove declaredAndUsedButUndefinedFunctionTable funcName in
                                let () = functionMap := M.add !functionMap funcName 
                                  (fType, paramTypes, true, false) in
                                let funcVarMap = init_func_env funcParams in
                                let newFuncParams = List.map(fun (t,i) -> (lowestTypedefType t, i)) funcParams in
                                (* Make sure the function returns! *)
                                let (ret, _, typeCheckedBlock) = 
                                  tc_statements funcName funcVarMap 
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
                                   tc_statements funcName funcVarMap 
                                   funcBody (lowestTypedefType funcType) false [] in
                                 if ((not ret) && (not (funcType = VOID))) then
                                  (ErrorMsg.error ("non-void functions must return \n");
                                   raise ErrorMsg.Error)
                                 else
                                  let newFuncName = "_c0_" ^ funcName in
                                  tc_prog gdecls (TypedPostElabFunDef(lowestTypedefType funcType, newFuncName, 
                                  newFuncParams, List.rev typeCheckedFuncBody)::typedAST))))
               | A.UntypedPostElabTypedef(typeDefType, typeDefName) -> 
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
               | A.UntypedPostElabFuncTypedef(retType, name, params) ->
                   (match (M.find !typedefMap name, M.find !functionMap name) with
                      (None, None) ->             
                        let paramTypes = List.map (fun (t,i) -> lowestTypedefType t) params in
                        let () = typedefMap := M.add !typedefMap name 
                            (FuncPrototype(Some name, lowestTypedefType retType, paramTypes)) in
                        tc_prog gdecls typedAST
                     | _ -> 
                        (ErrorMsg.error ("cannot shadow previously declared typedef/func names \n");
                          raise ErrorMsg.Error))
               | A.UntypedPostElabStructDecl(structName) ->
                   (match M.find !structMap structName with
                          None -> 
                            let () = structMap := M.add !structMap structName (ref Core.Std.String.Map.empty, false) in
                            tc_prog gdecls typedAST
                        | _ -> tc_prog gdecls typedAST) (* (ErrorMsg.error ("redeclaring struct " ^ structName ^ "\n");
                                raise ErrorMsg.Error) *)
               | A.UntypedPostElabStructDef(structName, fields) ->
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
and tc_statements (fName : ident) varMap (untypedBlock : A.untypedPostElabBlock) (funcRetType : c0type)
                          (ret : bool) (typedBlock : typedPostElabBlock) =
  match untypedBlock with
    [] -> (ret, varMap, typedBlock)
  | A.UntypedPostElabBlock(blockStmts)::stmts ->
      let (blockRet, blockVarMap, blockBlock) = tc_statements fName varMap blockStmts funcRetType ret [] in
      let newRet = (ret || blockRet) in
      (* We have returned if we return otherwise, or if the block returns *)
      (* Declarations from the block don't count, but initializations do,
         similar to if/else *)
      let newVarMap = M.mapi varMap (fun ~key:id -> (fun ~data:value ->
            (match M.find blockVarMap id with
                Some (typee, isInit) -> (typee, isInit)
              | None -> assert(false) (* everything in varMap should be in blockVarMap *)))) in
      tc_statements fName newVarMap stmts funcRetType newRet (blockBlock @ typedBlock)
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
                    let newTypedAST = (TypedPostElabAssignStmt(TypedPostElabVarLVal(id), A.EQ, tcExpr)
                                    :: TypedPostElabDecl(id, actualDeclType)
                                    :: typedBlock) in
                    let newVarMap = M.add varMap id (actualDeclType, true) in
                    if matchTypes actualDeclType exprType then 
                      tc_statements fName newVarMap stms funcRetType ret newTypedAST
                    else if not (isAlpha actualDeclType) && isAlpha exprType
                    then 
                      tc_statements fName newVarMap stms funcRetType ret newTypedAST
                    else                  
                      (ErrorMsg.error ("\nLHS type: " ^ c0typeToString(actualDeclType) ^"\nRHS type: "
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
                 tc_statements fName newVarMap stms funcRetType ret 
                 ((TypedPostElabDecl(id, actualType)) :: typedBlock)))
           | _ -> (ErrorMsg.error ("var names can't shadow func/typedef/declared var names\n");
                   raise ErrorMsg.Error))
  | A.UntypedPostElabAssignStmt(lval, op, e)::stms ->
      let (typedLVal, lvalType) = tc_lval varMap lval in
      let (tcExpr, exprType) = tc_expression varMap e in
      (match typedLVal with
         TypedPostElabVarLVal(id) -> (* LHS is a variable *)
           let newVarMap = M.add varMap id (lvalType, true) in
           if matchTypes (lowestTypedefType lvalType) exprType && notAStruct exprType
           then
             tc_statements fName newVarMap stms funcRetType ret
             ((TypedPostElabAssignStmt(typedLVal, op, tcExpr))::typedBlock)
           else if isAlpha lvalType && not (isAlpha exprType) (* apply type of RHS to LHS *)
           then
             let newerVarMap = M.add varMap id (exprType, true) in 
             tc_statements fName newerVarMap stms funcRetType ret
             ((TypedPostElabAssignStmt(typedLVal, op, tcExpr))::(TypedPostElabDecl(id, exprType))::typedBlock)
           else if not (isAlpha lvalType) && isAlpha exprType (* apply type of LHS to RHS *)
           then
             tc_statements fName newVarMap stms funcRetType ret
             (TypedPostElabAssignStmt(typedLVal, op, applyTypeToAlphaExpr tcExpr lvalType)::typedBlock)
           else if isAlpha lvalType && isAlpha exprType (* both sides have type alpha *)
           then
             tc_statements fName newVarMap stms funcRetType ret
             ((TypedPostElabAssignStmt(typedLVal, op, tcExpr))::typedBlock)
           else (* non-matching types *)
             (ErrorMsg.error ("types don't match\n"); raise ErrorMsg.Error)
       | _ -> (* LHS is a struct field access, array access, or pointer dereference *)
           (match op with
              A.EQ -> 
                if matchTypes (lowestTypedefType lvalType) exprType && notAStruct exprType
                then
                  tc_statements fName varMap stms funcRetType ret 
                  (TypedPostElabAssignStmt(typedLVal, op, tcExpr)::typedBlock)
                else if isAlpha lvalType && not (isAlpha exprType) (* apply type of RHS to LHS *)
                then 
                  tc_statements fName varMap stms funcRetType ret 
                  ((TypedPostElabAssignStmt(typedLVal, op, tcExpr))::typedBlock)
                else if not (isAlpha lvalType) && isAlpha exprType (* apply type of LHS to RHS *)
                then
                  tc_statements fName varMap stms funcRetType ret
                  (TypedPostElabAssignStmt(typedLVal, op, applyTypeToAlphaExpr tcExpr lvalType)::typedBlock)
                else if isAlpha lvalType && isAlpha exprType (* both sides have type alpha *)
                then
                  tc_statements fName varMap stms funcRetType ret 
                  ((TypedPostElabAssignStmt(typedLVal, op, tcExpr))::typedBlock)
                else (* nonmatching types *)
                  (ErrorMsg.error ("types don't match\n"); raise ErrorMsg.Error)
             | _ -> (* other assignOps only work with intExprs, therefore LHS and RHS must become intExprs *)
                 if matchTypes (lowestTypedefType lvalType) exprType && lvalType = INT
                 then 
                   tc_statements fName varMap stms funcRetType ret 
                   (TypedPostElabAssignStmt(typedLVal, op, tcExpr)::typedBlock)
                 else if isAlpha lvalType && not (isAlpha exprType) (* LHS must have type INT now *)
                 then 
                   tc_statements fName varMap stms funcRetType ret 
                   ((TypedPostElabAssignStmt(typedLVal, op, tcExpr))::typedBlock)
                 else if not (isAlpha lvalType) && isAlpha exprType (* RHS must have type INT now *)
                 then 
                   tc_statements fName varMap stms funcRetType ret
                   (TypedPostElabAssignStmt(typedLVal, op, applyTypeToAlphaExpr tcExpr INT)::typedBlock)
                 else if isAlpha lvalType && isAlpha exprType (* Both sides must have type INT now *)
                 then
                   tc_statements fName varMap stms funcRetType ret
                   ((TypedPostElabAssignStmt(typedLVal, op, applyTypeToAlphaExpr tcExpr INT))::typedBlock)
                 else (* nonmatching types *)
                   (ErrorMsg.error ("non-intExpr on either side of op\n"); raise ErrorMsg.Error)))

          
  | A.UntypedPostElabIf(e, block1, block2)::stms -> 
      let (tcExpr, _) = tc_expression varMap e in
      (match tcExpr with
             BoolExpr(exp1) -> 
               let (ret1, varMap1, tcBlock1) = tc_statements fName varMap block1 funcRetType ret [] in
               let (ret2, varMap2, tcBlock2) = tc_statements fName varMap block2 funcRetType ret [] in
               let newret = if ret then ret else (ret1 && ret2) in
               let newVarMap = M.mapi varMap (fun ~key:id -> (fun ~data:value ->
                                             (match (M.find varMap1 id, M.find varMap2 id) with
                                                    (Some (typee1, isInit1), Some (typee2, isInit2)) -> 
                                                                (typee1, isInit1 && isInit2)
                                                  | (_, _) -> value))) in
               tc_statements fName newVarMap stms funcRetType newret 
               ((TypedPostElabIf(exp1, List.rev tcBlock1, List.rev tcBlock2)) :: typedBlock)
           | _ -> ErrorMsg.error ("if expression didn't typecheck\n");
                  raise ErrorMsg.Error)
  | A.UntypedPostElabWhile(e, block1, untypedInitBlock)::stms -> 
      let (_, newVarMap2, tcBlock2) = tc_statements fName varMap untypedInitBlock funcRetType ret [] in
      let (tcExpr, _) = tc_expression newVarMap2 e in
      (match tcExpr with
             BoolExpr(exp1) ->                
               let (_, _, tcBlock1) = tc_statements fName newVarMap2 block1 funcRetType ret [] in
               tc_statements fName varMap stms funcRetType ret 
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
      if matchTypes funcRetType (lowestTypedefType exprType) 
      then 
        tc_statements fName newVarMap stms funcRetType true 
        ((TypedPostElabReturn(tcExpr)) :: typedBlock) 
      else 
        let Some(fType, x, y, z) = M.find !functionMap fName in
        if isAlpha fType && not (isAlpha exprType)
        then
          let () = functionMap := M.add !functionMap fName (exprType,x,y,z) in
          tc_statements fName newVarMap stms funcRetType true 
          ((TypedPostElabReturn(tcExpr)) :: typedBlock)
        else if isAlpha fType && isAlpha exprType 
        then 
          tc_statements fName newVarMap stms funcRetType true 
          ((TypedPostElabReturn(tcExpr)) :: typedBlock)
        else
          (ErrorMsg.error ("return expr type: " ^ c0typeToString exprType ^ 
            ", func ret type: " ^ c0typeToString funcRetType ^ "\n");
            raise ErrorMsg.Error)      
  | A.UntypedPostElabVoidReturn::stms ->
      (* Same as above, variables declared are treated as initalized in
         unreachable code... *)
      let newVarMap = M.map varMap (fun (typee, _) -> (typee, true)) in
      (match funcRetType with
             VOID -> 
               tc_statements fName newVarMap stms funcRetType true 
               (TypedPostElabVoidReturn :: typedBlock)
           | _ -> (ErrorMsg.error ("non-void function must return non-void value \n");
                  raise ErrorMsg.Error))
  | A.UntypedPostElabAssert(e)::stms ->
      let (tcExpr, _) = tc_expression varMap e in
      (match tcExpr with
             BoolExpr(expr1) -> 
               tc_statements fName varMap stms funcRetType ret 
               (TypedPostElabAssert(expr1) :: typedBlock)
           | _ -> (ErrorMsg.error ("assert must have bool expr\n");
                  raise ErrorMsg.Error))
  | A.UntypedPostElabExprStmt(e)::stms ->
      let (tcExpr, exprType) = tc_expression varMap e in
      (match exprType with
         Struct(_) -> (ErrorMsg.error ("type can't be a struct\n");
                          raise ErrorMsg.Error) 
       | _ ->
          (match tcExpr with
             VoidExpr(stmt) -> tc_statements fName varMap stms funcRetType ret (stmt :: typedBlock)
           | _ -> tc_statements fName varMap stms funcRetType ret 
                  (TypedPostElabAssignStmt(TypedPostElabVarLVal(GenUnusedID.create()), A.EQ, tcExpr) :: typedBlock)))
       
and typecheck ((untypedProgAST, untypedHeaderAST) : A.untypedPostElabOverallAST) =
  
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


let rec changeIntInner inner =
  match inner with
    IntConst(c) -> A.IntConst(c)
   |IntSharedExpr(s) -> A.IntSharedExpr(changeSharedExpr s)
   |ASTBinop(i1,op,i2) -> A.ASTBinop(changeIntInner i1, op, changeIntInner i2)
   |BaseCaseShift(i1,op,i2) -> A.BaseCaseShift(changeIntInner i1, op, changeIntInner i2)

and changeBoolInner inner =
  match inner with
    BoolConst(c) -> A.BoolConst(c)
   |BoolSharedExpr(s) -> A.BoolSharedExpr(changeSharedExpr s)
   |IntGreaterThan(i1,i2) -> A.IntGreaterThan(changeIntInner i1, changeIntInner i2)
   |IntLessThan(i1,i2) -> A.IntLessThan(changeIntInner i1, changeIntInner i2)
   |CharGreaterThan(i1,i2) -> A.CharGreaterThan(changeCharInner i1, changeCharInner i2)
   |CharLessThan(i1,i2) -> A.CharLessThan(changeCharInner i1, changeCharInner i2)
   |IntEquals(i1,i2) -> A.IntEquals(changeIntInner i1, changeIntInner i2)
   |BoolEquals(i1,i2) -> A.BoolEquals(changeBoolInner i1, changeBoolInner i2)
   |CharEquals(i1,i2) -> A.CharEquals(changeCharInner i1, changeCharInner i2)
   |PtrEquals(i1,i2) -> A.PtrEquals(changePtrInner i1, changePtrInner i2)
   |LogNot(b) -> A.LogNot(changeBoolInner b)
   |LogAnd(b1, b2) -> A.LogAnd(changeBoolInner b1, changeBoolInner b2)

and changeCharInner inner =
  match inner with
    CharConst(c) -> A.CharConst(c)
   |CharSharedExpr(s) -> A.CharSharedExpr(changeSharedExpr s)

and changeStringInner inner =
  match inner with
    StringConst(c) -> A.StringConst(c)
   |StringSharedExpr(s) -> A.StringSharedExpr(changeSharedExpr s)

and changePtrInner inner =
  match inner with
    Null -> A.Null
   |PtrSharedExpr(s) -> A.PtrSharedExpr(changeSharedExpr s)
   |Alloc(c) -> A.Alloc c
   |AllocArray(c,i) -> A.AllocArray(c,changeIntInner i)
   |AddressOfFunction(i) -> A.AddressOfFunction(i)

and changeAlphaInner (AlphaSharedExpr(s)) = A.AlphaSharedExpr(changeSharedExpr s)

and changeSharedExpr s =
  match s with
    Ternary(b,e1,e2)->A.Ternary(changeBoolInner b, changeExpr e1, changeExpr e2)
   |FunCall(i,exprs)->A.FunCall(i, List.map changeExpr exprs)
   |FuncPointerDeref(p,exprs)->A.FuncPointerDeref(changePtrInner p, List.map changeExpr exprs)
   |FieldAccess(id1,p,id2)->A.FieldAccess(id1, changePtrInner p, id2)
   |ArrayAccess(p,i)->A.ArrayAccess(changePtrInner p, changeIntInner i)
   |Deref(p)->A.Deref(changePtrInner p)
   |Ident(i)->A.Ident(i)

and changeExpr expr =
  match expr with
    IntExpr(inner) -> A.IntExpr(changeIntInner inner)
   |BoolExpr(inner) -> A.BoolExpr(changeBoolInner inner) 
   |CharExpr(inner) -> A.CharExpr(changeCharInner inner) 
   |StringExpr(inner) -> A.StringExpr(changeStringInner inner) 
   |VoidExpr(inner) -> A.VoidExpr(changeStmt inner) 
   |PtrExpr(inner) -> A.PtrExpr(changePtrInner inner) 
   |AlphaExpr(inner) -> A.AlphaExpr(changeAlphaInner inner) 

and changeLVal lval =
  match lval with
    TypedPostElabVarLVal(i) -> A.TypedPostElabVarLVal(i)
   |TypedPostElabFieldLVal(id1, lv, id2) -> A.TypedPostElabFieldLVal(id1, changeLVal lv, id2)
   |TypedPostElabDerefLVal(lv) -> A.TypedPostElabDerefLVal(changeLVal lv)
   |TypedPostElabArrayAccessLVal(lv,i) -> A.TypedPostElabArrayAccessLVal(changeLVal lv, changeIntInner i)

and changeStmt stmt =
  match stmt with
         TypedPostElabDecl(i,t) -> A.TypedPostElabDecl(i,t)
       | TypedPostElabAssignStmt(lval,op,expr)-> 
           A.TypedPostElabAssignStmt(changeLVal lval, op, changeExpr expr)
       | TypedPostElabIf(b,block1,block2) -> 
           A.TypedPostElabIf(changeBoolInner b, List.map changeStmt block1, List.map changeStmt block2) 
       | TypedPostElabWhile(b,blk) -> A.TypedPostElabWhile(changeBoolInner b, List.map changeStmt blk)
       | TypedPostElabReturn(expr) -> A.TypedPostElabReturn(changeExpr expr)
       | TypedPostElabAssert(expr) -> A.TypedPostElabAssert(changeBoolInner expr)
       | TypedPostElabVoidReturn -> A.TypedPostElabVoidReturn
       | VoidFunCall(i,exprs) -> A.VoidFunCall(i,List.map changeExpr exprs)
       | JumpUncond(l) -> A.JumpUncond(l)

let changeDecl decl =
  match decl with
     TypedPostElabStructDef(i,fs) -> A.TypedPostElabStructDef(i,fs)
    |TypedPostElabFunDef(t,i,ps,stmts) -> A.TypedPostElabFunDef(t,i,ps,List.map changeStmt stmts)

let changeAst decls = List.map changeDecl decls

