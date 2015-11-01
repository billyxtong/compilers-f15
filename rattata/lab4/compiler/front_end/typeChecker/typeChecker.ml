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
module H = Hashtbl
open PrintDatatypes
open PrintASTs
open TcExprs

let declaredAndUsedButUndefinedFunctionTable = H.create 5

let functionMap = ref Core.Std.String.Map.empty
let typedefMap = ref Core.Std.String.Map.empty
let structMap = ref Core.Std.String.Map.empty

let isValidVarDecl (identifier : ident) = 
  if sub identifier 0 1 = "\\" 
  then true 
  else false

let lowestTypedefType (typedefType : c0type) =
  match typedefType with
        TypedefType(identifier) -> 
          (match M.find !typedefMap identifier with
                 Some(anotherType) -> anotherType
               | None -> (ErrorMsg.error ("undefined typedef \n");
                         raise ErrorMsg.Error))
      | _ -> typedefType

let rec argsMatch (arguments : typedPostElabExpr list) (paramTypes : c0type list) =
  match (arguments, paramTypes) with
        ([], []) -> true
      | ([], p :: ps) -> false
      | (arg :: args, []) -> false
      | (arg :: args, p :: ps) ->
          (match (arg, lowestTypedefType p) with
                 (IntExpr(_), INT) -> argsMatch args ps
               | (BoolExpr(_), BOOL) -> argsMatch args ps
               | _ -> false)


let rec matchParamListTypes (paramTypes : c0type list) (params : param list) =
  match (paramTypes, params) with
        ([], []) -> true
      | ([], p :: ps) -> false
      | (p :: ps, []) -> false
      | (p :: ps, (typee, _) :: remainingParams) ->
          let pType = lowestTypedefType p in
          let paramType = lowestTypedefType typee in
          if ((pType = INT && paramType = INT) || (pType = BOOL && paramType = BOOL))
          then matchParamListTypes ps remainingParams else false

let matchFuncTypes (funcType1 : c0type) (funcType2 : c0type) =
  if (lowestTypedefType funcType1) = (lowestTypedefType funcType2)
  then true else false

let rec uniqueParamNames (params : param list) nameTable =
  match params with
        [] -> true
      | (datType, datName) :: ps -> 
          (match (M.find nameTable datName, M.find !typedefMap datName) with
                 (None, None) -> uniqueParamNames ps (M.add nameTable datName ())
               | _ -> false)


(* funcName -> (funcType, list of types of funcParams, isDefined, isExternal) *)
let rec tc_header (header : untypedPostElabAST) = 
  match header with
        [] -> ()
      | fdecl :: fdecls -> 
          (match fdecl with
                 UntypedPostElabTypedef(t, i) -> 
                 if t = VOID then
                    (ErrorMsg.error ("apparently you can't typedef voids which is stupid but whatever \n");
                     raise ErrorMsg.Error)
                 else
                   (match (M.find !typedefMap i, M.find !functionMap i) with
                          (None, None) -> 
                            let () = typedefMap := M.add !typedefMap i 
                                        (lowestTypedefType t) in
                            tc_header fdecls
                        | _ -> (ErrorMsg.error ("typedef cannot shadow previously declared typedef/func names \n");
                                raise ErrorMsg.Error))
               | UntypedPostElabFunDecl(funcType, funcName, funcParams) ->
                   let nameTable = Core.Std.String.Map.empty in
                   if not (uniqueParamNames funcParams nameTable) then 
                      (ErrorMsg.error ("bad param names \n");
                                raise ErrorMsg.Error)
                   else
                   if List.exists (fun (typee, id) -> typee = VOID) funcParams then
                      (ErrorMsg.error ("can't have void as param type \n");
                                raise ErrorMsg.Error)
                   else
                   (match (M.find !typedefMap funcName, M.find !functionMap funcName) with
                          (Some _, _) -> 
                            (ErrorMsg.error ("function cannot shadow previously declared typedef/func name\n");
                             raise ErrorMsg.Error)
                        | (None, Some (fType, paramTypes, isDefined, isExternal)) ->
                            if not ((matchFuncTypes fType funcType) && 
                                    (matchParamListTypes paramTypes funcParams))
                            then (ErrorMsg.error ("trying to redeclare func with wrong func type/param types \n");
                                  raise ErrorMsg.Error)
                            else
                              tc_header fdecls
                        | (None, None) -> 
                            (let () = functionMap := M.add !functionMap funcName 
                             (lowestTypedefType funcType, 
                             (List.map (fun (c, i) -> c) funcParams), true, true) in
                             tc_header fdecls))
               | _ -> (ErrorMsg.error ("function def'n in header file \n");
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
                   if List.exists (fun (typee, id) -> typee = VOID) funcParams then
                      (ErrorMsg.error ("can't have void as param type \n");
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
                      (ErrorMsg.error ("can't have void as param type \n");
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
                              then (ErrorMsg.error ("trying to define func with wrong func type/param types \n");
                                   raise ErrorMsg.Error)
                              else
                                let () = H.remove declaredAndUsedButUndefinedFunctionTable funcName in
                                let () = functionMap := M.add !functionMap funcName 
                                  (fType, paramTypes, true, false) in
                                let funcVarMap = init_func_env funcParams in
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
                                  tc_prog gdecls (TypedPostElabFunDef(funcType, newFuncName, 
                                  funcParams, List.rev typeCheckedBlock)::typedAST))
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
                                 let (ret, _, typeCheckedFuncBody) = 
                                   tc_statements funcVarMap 
                                   funcBody (lowestTypedefType funcType) false [] in
                                 if ((not ret) && (not (funcType = VOID))) then
                                  (ErrorMsg.error ("non-void functions must return \n");
                                   raise ErrorMsg.Error)
                                 else
                                  let newFuncName = "_c0_" ^ funcName in
                                  tc_prog gdecls (TypedPostElabFunDef(funcType, newFuncName, 
                                  funcParams, List.rev typeCheckedFuncBody)::typedAST))))
               | UntypedPostElabTypedef(typeDefType, typeDefName) -> 
                    if typeDefType = VOID then
                    (ErrorMsg.error ("apparently you can't typedef voids which is stupid but whatever \n");
                     raise ErrorMsg.Error)
                    else
                   (match (M.find !typedefMap typeDefName, M.find !functionMap typeDefName) with
                          (None, None) -> 
                            let () = typedefMap := M.add !typedefMap typeDefName 
                                        (lowestTypedefType typeDefType) in
                            tc_prog gdecls typedAST
                        | _ -> (ErrorMsg.error ("cannot shadow previously declared typedef/func names \n");
                                raise ErrorMsg.Error)))

(* varMap is the map of variables within the function body *) 
(* funcRetType is the return type of the function *)         
and tc_statements varMap (untypedBlock : untypedPostElabBlock) (funcRetType : c0type)
                          (ret : bool) (typedBlock : typedPostElabBlock) =
  match untypedBlock with
    [] -> (ret, varMap, typedBlock)
  | A.UntypedPostElabBlock(blockStmts)::stmts ->
      (* let () = (blockCounter := !blockCounter + 1) in
      let () = print_string ("block number " ^ string_of_int(!blockCounter) ^ "\n") in *)
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
    (* Added by Ben: see explanation in ast.ml *)
       (let tcExpr = tc_expression varMap e in
        let actualDeclType = lowestTypedefType typee in
      (match (M.find !typedefMap id, M.find varMap id) with
             (None, None) -> 
                if actualDeclType = VOID then 
                  (ErrorMsg.error ("vars can't have type void\n");
                   raise ErrorMsg.Error)
                else
                   let newTypedAST = (TypedPostElabAssignStmt(id, tcExpr)
                                     :: TypedPostElabDecl(id, actualDeclType):: typedBlock) in

                   (match (tcExpr, actualDeclType) with
                          (IntExpr(_), INT) -> 
                            let newVarMap = M.add varMap id (actualDeclType, true) in  
                            tc_statements newVarMap 
                            stms funcRetType ret newTypedAST
                        | (BoolExpr(_), BOOL) -> 
                            let newVarMap = M.add varMap id (actualDeclType, true) in              
                            tc_statements newVarMap 
                            stms funcRetType ret newTypedAST
                        | _ -> (ErrorMsg.error ("assignment expression didn't typecheck" ^ id ^"\n");
                               raise ErrorMsg.Error))
           | _ -> (ErrorMsg.error ("var names can't shadow func/typedef/declared var names\n");
                   raise ErrorMsg.Error)))
  | A.UntypedPostElabDecl(id, typee)::stms ->
      (match (M.find !typedefMap id, M.find varMap id) with
             (None, None) -> 
               (let actualType = lowestTypedefType typee in
                if actualType = VOID then 
                  (ErrorMsg.error ("vars can't have type void\n");
                   raise ErrorMsg.Error)
                else
                 let newVarMap = M.add varMap id (actualType, false) in 
                tc_statements newVarMap 
                stms funcRetType ret ((TypedPostElabDecl(id, actualType)) :: typedBlock))
           | _ -> (ErrorMsg.error ("var names can't shadow func/typedef/declared var names\n");
                   raise ErrorMsg.Error))
  | A.UntypedPostElabAssignStmt(id, e)::stms ->
       (let tcExpr = tc_expression varMap e in
          (match M.find varMap id with (* it's declared, good *)
                 Some(typee, _) -> 
                   (match (tcExpr, typee) with
                          (IntExpr(_), INT) -> 
                            let newVarMap = M.add varMap id (typee, true) in  
                            tc_statements newVarMap 
                            stms funcRetType ret ((TypedPostElabAssignStmt(id, tcExpr)) :: typedBlock)
                        | (BoolExpr(_), BOOL) -> 
                            let newVarMap = M.add varMap id (typee, true) in              
                            tc_statements newVarMap 
                            stms funcRetType ret ((TypedPostElabAssignStmt(id, tcExpr)) :: typedBlock)
                        | _ -> (ErrorMsg.error ("assignment expression didn't typecheck \n");
                               raise ErrorMsg.Error))
               | None -> (if ((isValidVarDecl id))
                          then (let newVarMap = M.add varMap id (INT, true) in              
                                tc_statements newVarMap 
                                stms funcRetType ret ((TypedPostElabAssignStmt(id, tcExpr)) :: typedBlock))
                          else (ErrorMsg.error ("undeclared test variable " ^ id ^ "\n");
                                raise ErrorMsg.Error))))
  | A.UntypedPostElabIf(e, block1, block2)::stms -> 
      let tcExpr = tc_expression varMap e in
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
      let tcExpr = tc_expression newVarMap2 e in
      (match tcExpr with
             BoolExpr(exp1) ->                
               let (_, _, tcBlock1) = tc_statements newVarMap2 block1 funcRetType ret [] in
               tc_statements varMap stms funcRetType ret 
               ((TypedPostElabWhile(exp1, List.rev tcBlock1)) :: (tcBlock2 @ typedBlock))
           | _ -> ErrorMsg.error ("while expression didn't typecheck\n");
                  raise ErrorMsg.Error)
  | A.UntypedPostElabReturn(e)::stms ->
      let tcExpr = tc_expression varMap e in
      (* apparently all variables declared before the first return
         get to be treated as initialized...although those declared
         after don't *)
      let newVarMap = M.map varMap (fun (typee, _) -> (typee, true)) in
      (match tcExpr with
             IntExpr(exp1) -> 
               (match funcRetType with
                      INT -> tc_statements newVarMap 
                             stms funcRetType true ((TypedPostElabReturn(IntExpr exp1)) :: typedBlock)
                    | _ -> (ErrorMsg.error ("int return expression didn't typecheck\n");
                            raise ErrorMsg.Error))
           | BoolExpr(exp1) -> 
               (match funcRetType with
                      BOOL -> tc_statements newVarMap 
                              stms funcRetType true ((TypedPostElabReturn(BoolExpr exp1)) :: typedBlock)
                    | _ -> (ErrorMsg.error ("bool return expression didn't typecheck\n");
                           raise ErrorMsg.Error))
           | VoidExpr(exp1) -> (ErrorMsg.error ("can't return void \n");
                           raise ErrorMsg.Error))
      
  | A.UntypedPostElabVoidReturn::stms ->
      (* Same as above, variables declared are treated as initalized in
         unreachable code... *)
      let newVarMap = M.map varMap (fun (typee, _) -> (typee, true)) in
      (match funcRetType with
             VOID -> tc_statements newVarMap 
                     stms funcRetType true (TypedPostElabVoidReturn :: typedBlock)
           | _ -> ErrorMsg.error ("non-void function must return non-void value \n");
                  raise ErrorMsg.Error)
  | A.UntypedPostElabAssert(e)::stms ->
      let tcExpr = tc_expression varMap e in
      (match tcExpr with
             BoolExpr(expr1) -> tc_statements varMap 
                                stms funcRetType ret (TypedPostElabAssert(expr1) :: typedBlock)
           | _ -> (ErrorMsg.error ("assert must have boolean expression \n");
                  raise ErrorMsg.Error))
  | A.UntypedPostElabExprStmt(e)::stms ->
      let tcExpr = tc_expression varMap e in
      (match tcExpr with
             VoidExpr(stmt) -> tc_statements varMap 
                               stms funcRetType ret (stmt :: typedBlock)
           | _ -> tc_statements varMap 
                  stms funcRetType ret (TypedPostElabAssignStmt(GenUnusedID.create(), tcExpr) :: typedBlock))
       
and typecheck ((untypedProgAST, untypedHeaderAST) : untypedPostElabOverallAST) =
  
  let () = tc_header untypedHeaderAST in
  (* the main function is considered to always be declared at the top of the main file!
     It still needs to be defined eventually of course *)
  let () = (match M.find !functionMap "main" with
                   Some _ -> (ErrorMsg.error ("main cannot be declared in header \n");
                              raise ErrorMsg.Error)
                 | None -> ()) in
  let () = functionMap := M.add !functionMap "main" (INT, [], false, false) in
  (* type is INT, no params, isDefined = false, isExternal = false *)
  let typedProgAST = tc_prog untypedProgAST [] in
  List.rev typedProgAST

(* Note: please check all of the typing rules for structs, becayse they're
   pretty complicated. Check out the lecture notes on structs, as well
   as the handout. *)
