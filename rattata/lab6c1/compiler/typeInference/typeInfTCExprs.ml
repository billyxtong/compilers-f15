open Datatypesv1
open TypeInfAst
module H = Hashtbl
module A = Ast  
module M = Core.Std.Map
open String
open TypeInfPrintASTS
open PrintDatatypes

let alphaCount = ref 0

let declaredAndUsedButUndefinedFunctionTable = H.create 5
type mytype = ((c0type Core.Std.String.Map.t) ref)* bool
let functionMap = ref Core.Std.String.Map.empty
let typedefMap = ref Core.Std.String.Map.empty
let (structMap : (mytype Core.Std.String.Map.t) ref) = 
  ref (Core.Std.String.Map.empty) 

let isAlpha t =
  match t with
    Alpha _ -> true
   |_ -> false

let isValidVarDecl (identifier : ident) =
  if sub identifier 0 1 = "\\" then true else false 

let rec lowestTypedefType (typedefType) =
  match typedefType with
        TypedefType(identifier) ->
          (match M.find !typedefMap identifier with
                 Some(anotherType) -> anotherType
               | None -> (ErrorMsg.error ("undefined typedef \n");
                         raise ErrorMsg.Error))
      | Pointer(c) -> Pointer(lowestTypedefType c)
      | Array(c) -> Array(lowestTypedefType c)
      | _ -> typedefType

let rec addTypeToSharedExpr e = function
    INT -> IntExpr (IntSharedExpr e)
  | BOOL -> BoolExpr (BoolSharedExpr e)
  | CHAR -> CharExpr (CharSharedExpr e)
  | STRING -> StringExpr (StringSharedExpr e)
  | Pointer _ -> PtrExpr (PtrSharedExpr e)
  | VOID -> (ErrorMsg.error ("void ret type"); raise ErrorMsg.Error)
  | Array _ -> PtrExpr (PtrSharedExpr e)
  | FuncPrototype _ -> PtrExpr (PtrSharedExpr e)
  | Poop -> PtrExpr (PtrSharedExpr e)
  | TypedefType t -> addTypeToSharedExpr e (lowestTypedefType (TypedefType t))
  | Struct _ -> PtrExpr (PtrSharedExpr e)
  | Alpha n -> AlphaExpr (AlphaSharedExpr e)

let notAStruct (t : c0type) =
  match lowestTypedefType t with
        Struct(_) -> false
      | _ -> true

let rec matchTypes (t1 : c0type) (t2 : c0type) =
  match (t1, t2) with
        (INT, INT) -> true
      | (BOOL, BOOL) -> true
      | (CHAR, CHAR) -> true
      | (STRING, STRING) -> true
      | (Pointer(c1), Pointer(c2)) -> matchTypes c1 c2
      | (Array(c1), Array(c2)) -> matchTypes c1 c2
      | (Struct(i1), Struct(i2)) -> (i1 = i2)
      | (Poop, Poop) -> true
      | (Pointer(c), Poop) -> true
      | (Poop, Pointer(c)) -> true
      | (FuncPrototype(Some name1, retType1, argTypes1), FuncPrototype(Some name2, retType2, argTypes2)) ->
           name1 == name2 && matchFuncTypes retType1 retType2 && argsMatch argTypes1 argTypes2
      | (FuncPrototype(Some name1, retType1, argTypes1), FuncPrototype(None, retType2, argTypes2)) ->
          matchFuncTypes retType1 retType2 && argsMatch argTypes1 argTypes2 (* binop_fn* f = &foo; *)
      | _ -> false

and argsMatch (argTypes : c0type list) (paramTypes : c0type list) =
  match (argTypes, paramTypes) with
        ([], []) -> true
      | ([], p :: ps) -> false
      | (arg :: args, []) -> false
      | (arg :: args, p :: ps) ->
          if matchTypes arg p
          then argsMatch args ps
          else 
            if isAlpha arg || isAlpha p
            then argsMatch args ps
            else false

and matchFuncTypes (funcType1 : c0type) (funcType2 : c0type) =
  if (lowestTypedefType funcType1) = (lowestTypedefType funcType2)
  then true else false

let rec matchParamListTypes (paramTypes : c0type list) (params : A.param list) =
  match (paramTypes, params) with
        ([], []) -> true
      | ([], p :: ps) -> false
      | (p :: ps, []) -> false
      | (p :: ps, (typee, _) :: remainingParams) ->
          let pType = lowestTypedefType p in
          let paramType = lowestTypedefType typee in
          if matchTypes pType paramType
          then matchParamListTypes ps remainingParams else false


let rec uniqueParamNames (params : A.param list) nameTable =
  match params with
        [] -> true
      | (datType, datName) :: ps ->
          (match (M.find nameTable datName, M.find !typedefMap datName) with
                 (None, None) -> uniqueParamNames ps (M.add nameTable datName ())
               | _ -> false)

let rec uniqueFieldNames (fields : A.field list) nameTable = 
  match fields with
        [] -> true
      | (datType, datName) :: fs ->
          (match M.find nameTable datName with
                 None -> uniqueFieldNames fs (M.add nameTable datName ())
               | _ -> false)

let rec isNestedVoidPtr(t : c0type) =
  match t with
        VOID -> true
      | Pointer(c) -> isNestedVoidPtr c
      | Array(c) -> isNestedVoidPtr c
      | _ -> false

let rec areStructFieldsWellDefined fields =
  match fields with
        [] -> true
      | (typee, _) :: fs -> 
          (match lowestTypedefType typee with
                 Struct(name) -> 
                   (match M.find !structMap name with
                          Some (_, true) -> areStructFieldsWellDefined fs
                        | _ -> false)
               | VOID -> false
               | Pointer(c) -> if (isNestedVoidPtr c) then false else areStructFieldsWellDefined fs
               | _ -> areStructFieldsWellDefined fs)

let typeNotLarge (t : c0type) =
  match lowestTypedefType t with
        Struct(_) -> false
      | _ -> true

let rec areAnyFuncParamsStructs (l : A.param list) =
  match l with
        [] -> false
      | (t, _) :: ps ->
          (match lowestTypedefType t with
                 Struct(_) -> true
               | _ -> areAnyFuncParamsStructs ps)

let rec applyTypeToAlphaSharedExpr (AlphaSharedExpr(expr)) t =
  match expr with
    Ternary(b,e1,e2) -> Ternary(b, applyTypeToAlphaExpr e1 t, applyTypeToAlphaExpr e2 t)
  | _ -> expr

and applyTypeToAlphaExpr (AlphaExpr(inner)) t =
  let typeCheckedSharedExpr = applyTypeToAlphaSharedExpr inner t in
  match t with
    INT -> IntExpr(IntSharedExpr(typeCheckedSharedExpr))
  | BOOL -> BoolExpr(BoolSharedExpr(typeCheckedSharedExpr))
  | CHAR -> CharExpr(CharSharedExpr(typeCheckedSharedExpr))
  | STRING -> StringExpr(StringSharedExpr(typeCheckedSharedExpr))
  | _ -> assert(false)
     (* We should be allowing function pointer calls and derefs, 
                          but our current constructors disallow this and I don't want to 
                          worry about it yet. Field accesses should only have type Alpha _
                          if two structs have the same field name and type and array 
                          accesses if we don't know the type of the array beforehand. 
                          Both of these would only happen if you pass the struct or array
                          as a parameter to a function. *)

let rec tc_expression varEnv (expression : A.untypedPostElabExpr) : typedPostElabExpr * c0type =
  match expression with
    A.UntypedPostElabConstExpr (constant, typee) ->
      (match typee with
             INT -> (IntExpr(IntConst(constant)), INT)
           | BOOL -> (BoolExpr(BoolConst(constant)), BOOL)
           | CHAR -> (CharExpr(CharConst(constant)), CHAR)
           | _ -> (ErrorMsg.error ("constants must be int, bool, or char\n");
                   raise ErrorMsg.Error))
  | A.UntypedPostElabStringConstExpr(str) -> 
      if List.exists(fun c -> c = 0) str then
      (ErrorMsg.error ("null character cannot appear in strings\n");
        raise ErrorMsg.Error)
      else (StringExpr(StringConst(str)), STRING)
  | A.UntypedPostElabNullExpr -> (PtrExpr(Null), Poop)
  | A.UntypedPostElabIdentExpr(i : ident) ->
      (match M.find varEnv i with
             Some (typee, isInit) -> 
               if not isInit
               then (ErrorMsg.error ("ident " ^ i ^ " uninitialized\n");
                   raise ErrorMsg.Error)
               else
                 let actualType = lowestTypedefType typee in
               (match actualType with
                      INT -> (IntExpr(IntSharedExpr(Ident(i))), actualType)
                    | BOOL -> (BoolExpr(BoolSharedExpr(Ident(i))), actualType)
                    | CHAR -> (CharExpr(CharSharedExpr(Ident(i))), actualType)
                    | STRING -> (StringExpr(StringSharedExpr(Ident(i))), actualType)
                    | Pointer(c) -> (PtrExpr(PtrSharedExpr(Ident(i))), actualType)
                    | Array(c) -> (PtrExpr(PtrSharedExpr(Ident(i))), actualType)
                    | Alpha(count) -> (AlphaExpr(AlphaSharedExpr(Ident(i))), actualType) 
                    (* for idents whose types we haven't inferred yet *)
                    | _ -> (ErrorMsg.error ("can't reference a struct without ptr\n");
                            raise ErrorMsg.Error))
           | None -> (ErrorMsg.error ("undeclared variable " ^ i ^ "\n");
                            raise ErrorMsg.Error))
  | A.UntypedPostElabBinop (e1, op, e2) ->
      let (tcExpr1, type1) = tc_expression varEnv e1 in
      let (tcExpr2, type2) = tc_expression varEnv e2 in
      (match op with
             A.GT -> (match (tcExpr1, tcExpr2) with
                          (IntExpr(exp1), IntExpr(exp2)) -> (BoolExpr(IntGreaterThan(exp1, exp2)), BOOL)
                        | (IntExpr(exp1), AlphaExpr(exp2)) -> 
                            (BoolExpr(IntGreaterThan(exp1, IntSharedExpr(applyTypeToAlphaSharedExpr exp2 INT))), BOOL)
                        | (AlphaExpr(exp1), IntExpr(exp2)) -> 
                            (BoolExpr(IntGreaterThan(IntSharedExpr(applyTypeToAlphaSharedExpr exp1 INT), exp2)), BOOL)
                        | (CharExpr(exp1), CharExpr(exp2)) -> (BoolExpr(CharGreaterThan(exp1, exp2)), BOOL)
                        | (CharExpr(exp1), AlphaExpr(exp2)) ->
                            (BoolExpr(CharGreaterThan(exp1, CharSharedExpr(applyTypeToAlphaSharedExpr exp2 CHAR))), BOOL)
                        | (AlphaExpr(exp1), CharExpr(exp2)) -> 
                            (BoolExpr(CharGreaterThan(CharSharedExpr(applyTypeToAlphaSharedExpr exp1 CHAR), exp2)), BOOL)
                        | (AlphaExpr(exp1), AlphaExpr(exp2)) -> assert(false)
                        | _ -> (ErrorMsg.error ("greater than expression didn't typecheck \n");
                               raise ErrorMsg.Error))
           | A.LT -> (match (tcExpr1, tcExpr2) with
                          (IntExpr(exp1), IntExpr(exp2)) -> (BoolExpr(IntLessThan(exp1, exp2)), BOOL)
                        | (IntExpr(exp1), AlphaExpr(exp2)) -> 
                            (BoolExpr(IntLessThan(exp1, IntSharedExpr(applyTypeToAlphaSharedExpr exp2 INT))), BOOL)
                        | (AlphaExpr(exp1), IntExpr(exp2)) -> 
                            (BoolExpr(IntLessThan(IntSharedExpr(applyTypeToAlphaSharedExpr exp1 INT), exp2)), BOOL)
                        | (CharExpr(exp1), CharExpr(exp2)) -> (BoolExpr(CharLessThan(exp1, exp2)), BOOL)
                        | (CharExpr(exp1), AlphaExpr(exp2)) -> 
                            (BoolExpr(CharLessThan(exp1, CharSharedExpr(applyTypeToAlphaSharedExpr exp2 CHAR))), BOOL)
                        | (AlphaExpr(exp1), CharExpr(exp2)) -> 
                            (BoolExpr(CharLessThan(CharSharedExpr(applyTypeToAlphaSharedExpr exp1 CHAR), exp2)), BOOL)
                        | (AlphaExpr(exp1), AlphaExpr(exp2)) -> assert(false)
                        | _ -> (ErrorMsg.error ("less than expression didn't typecheck \n");
                               raise ErrorMsg.Error))
           | A.DOUBLE_EQ ->
               (match (tcExpr1, tcExpr2) with
                  (IntExpr(exp1), IntExpr(exp2)) -> (BoolExpr(IntEquals(exp1, exp2)), BOOL)
                | (IntExpr(exp1), AlphaExpr(exp2)) -> 
                    (BoolExpr(IntEquals(exp1, IntSharedExpr(applyTypeToAlphaSharedExpr exp2 INT))), BOOL)
                | (AlphaExpr(exp1), IntExpr(exp2)) -> 
                    (BoolExpr(IntEquals(IntSharedExpr(applyTypeToAlphaSharedExpr exp1 INT), exp2)), BOOL)
                | (BoolExpr(exp1), BoolExpr(exp2)) -> (BoolExpr(BoolEquals(exp1, exp2)), BOOL)
                | (BoolExpr(exp1), AlphaExpr(exp2)) ->
                    (BoolExpr(BoolEquals(exp1, BoolSharedExpr(applyTypeToAlphaSharedExpr exp2 BOOL))), BOOL)
                | (AlphaExpr(exp1), BoolExpr(exp2)) -> 
                    (BoolExpr(BoolEquals(BoolSharedExpr(applyTypeToAlphaSharedExpr exp1 BOOL), exp2)), BOOL)
                | (CharExpr(exp1), CharExpr(exp2)) -> (BoolExpr(CharEquals(exp1, exp2)), BOOL)
                | (CharExpr(exp1), AlphaExpr(exp2)) -> 
                    (BoolExpr(CharEquals(exp1, CharSharedExpr(applyTypeToAlphaSharedExpr exp2 CHAR))), BOOL)
                | (AlphaExpr(exp1), CharExpr(exp2)) ->
                    (BoolExpr(CharEquals(CharSharedExpr(applyTypeToAlphaSharedExpr exp1 CHAR), exp2)), BOOL)
                | (PtrExpr(exp1), PtrExpr(exp2)) -> 
                    if matchTypes type1 type2 && 
                       notAStruct type1 && notAStruct type2 then
                    (BoolExpr(PtrEquals(exp1, exp2)), BOOL)
                    else (ErrorMsg.error ("different typed ptrExprs\n");
                       raise ErrorMsg.Error)
                | (PtrExpr(exp1), AlphaExpr(exp2)) -> assert(false)
                | (AlphaExpr(exp1), PtrExpr(exp2)) -> assert(false)
                | (AlphaExpr(exp1), AlphaExpr(exp2)) -> assert(false)
                | _ -> 
                  (ErrorMsg.error ("double eq exprs didn't typecheck \n");
                     raise ErrorMsg.Error))
           | A.LOG_AND -> 
               (match (tcExpr1, tcExpr2) with
                  (BoolExpr(exp1), BoolExpr(exp2)) -> (BoolExpr(LogAnd(exp1, exp2)), BOOL)
                | (BoolExpr(exp1), AlphaExpr(exp2)) -> 
                    (BoolExpr(LogAnd(exp1, BoolSharedExpr(applyTypeToAlphaSharedExpr exp2 BOOL))), BOOL)
                | (AlphaExpr(exp1), BoolExpr(exp2)) -> 
                    (BoolExpr(LogAnd(BoolSharedExpr(applyTypeToAlphaSharedExpr exp1 BOOL), exp2)), BOOL)
                | _ -> (ErrorMsg.error ("logical and exprs didn't typecheck \n");
                         raise ErrorMsg.Error))
           | A.IntBinop(intOp) -> 
               (match (tcExpr1, tcExpr2) with
                  (IntExpr(exp1), IntExpr(exp2)) -> (IntExpr(ASTBinop(exp1, intOp, exp2)), INT)
                | (IntExpr(exp1), AlphaExpr(exp2)) -> 
                    (IntExpr(ASTBinop(exp1, intOp, IntSharedExpr(applyTypeToAlphaSharedExpr exp2 INT))), INT)
                | (AlphaExpr(exp1), IntExpr(exp2)) -> 
                    (IntExpr(ASTBinop(IntSharedExpr(applyTypeToAlphaSharedExpr exp1 INT), intOp, exp2)), INT)
                | (AlphaExpr(exp1), AlphaExpr(exp2)) ->
                    (IntExpr(ASTBinop(IntSharedExpr(applyTypeToAlphaSharedExpr exp1 INT), intOp,
                                      IntSharedExpr(applyTypeToAlphaSharedExpr exp2 INT))), INT)
                | _ -> 
                  (ErrorMsg.error ("LHS type: " ^ c0typeToString type1 ^ ", RHS type: " ^ c0typeToString type2 ^ " \n");
                    raise ErrorMsg.Error)))
  | A.UntypedPostElabNot(e' : A.untypedPostElabExpr) ->
      let (tcExpr,t) = tc_expression varEnv e' in
      (match tcExpr with
             BoolExpr(exp1) -> (BoolExpr(LogNot(exp1)), BOOL)
           | AlphaExpr(exp1) -> (BoolExpr(LogNot(BoolSharedExpr(applyTypeToAlphaSharedExpr exp1 BOOL))), BOOL)
           | _ -> (ErrorMsg.error ("not expression didn't typecheck \n");
                  raise ErrorMsg.Error))
  | A.UntypedPostElabTernary(e1, e2, e3) ->
      let (tcExpr1,type1) = tc_expression varEnv e1 in
      let (tcExpr2,type2) = tc_expression varEnv e2 in
      let (tcExpr3,type3) = tc_expression varEnv e3 in
      (match tcExpr1 with
         BoolExpr(exp1) ->
           (match (tcExpr2, tcExpr3) with
              (IntExpr _, IntExpr _) -> 
                (IntExpr(IntSharedExpr(Ternary(exp1, tcExpr2, tcExpr3))), INT)
            | (IntExpr _, AlphaExpr _) -> 
                (IntExpr(IntSharedExpr(Ternary(exp1, tcExpr2, applyTypeToAlphaExpr tcExpr3 INT))), INT)
            | (AlphaExpr _, IntExpr _) -> 
                (IntExpr(IntSharedExpr(Ternary(exp1, applyTypeToAlphaExpr tcExpr2 INT, tcExpr3))), INT)
            | (BoolExpr _, BoolExpr _) -> 
                (BoolExpr(BoolSharedExpr(Ternary(exp1, tcExpr2, tcExpr3))), BOOL)
            | (BoolExpr _, AlphaExpr _) -> assert(false) 
            | (AlphaExpr _, BoolExpr _) -> assert(false) 
            | (CharExpr _, CharExpr _) -> 
                (CharExpr(CharSharedExpr(Ternary(exp1, tcExpr2, tcExpr3))), CHAR)
            | (CharExpr _, AlphaExpr _) -> assert(false)
            | (AlphaExpr _, CharExpr _) -> assert(false)
            | (StringExpr _, StringExpr _) -> 
                (StringExpr(StringSharedExpr(Ternary(exp1, tcExpr2, tcExpr3))), STRING)
            | (StringExpr _, AlphaExpr _) -> assert(false)
            | (AlphaExpr _, StringExpr _) -> assert(false)
            | (PtrExpr _, PtrExpr _) ->
                if matchTypes type2 type3 && typeNotLarge type2 && typeNotLarge type3
                then
                  if not (type2 = Poop) (* if types match and first isn't NULL, use first *)
                  then 
                    (PtrExpr(PtrSharedExpr(Ternary(exp1, tcExpr2, tcExpr3))), type2)
                  else if not (type3 = Poop) (* if first is NULL and second isn't, use second *)
                  then (PtrExpr(PtrSharedExpr(Ternary(exp1, tcExpr2, tcExpr3))), type3)
                  else (* whole exp is NULL *)
                    (PtrExpr(PtrSharedExpr(Ternary(exp1, tcExpr2, tcExpr3))), Poop)
                else
                  (ErrorMsg.error ("pointer exprs in ternary have diff. types\n");
                  raise ErrorMsg.Error)
            | (PtrExpr(_), AlphaExpr(exp3)) -> assert(false)
            | (AlphaExpr(exp2), PtrExpr(_)) -> assert(false)
            | (AlphaExpr(exp2), AlphaExpr(exp3)) -> assert(false)
            | _ -> (ErrorMsg.error ("exprs in ternary have diff. types\n");
                    raise ErrorMsg.Error))
       | _ -> (ErrorMsg.error ("ternary expression didn't typecheck \n");
                   raise ErrorMsg.Error))
  | A.UntypedPostElabFunCall(i, argList) -> 
      (match (M.find varEnv i, M.find !functionMap i) with
             (Some _, _) -> (ErrorMsg.error ("cannot call this function while var with same name is in scope \n");
                             raise ErrorMsg.Error)
           | (None, Some(funcType, funcParams, isDefined, isExternal)) ->
               let () = (if not isDefined
                         then H.replace declaredAndUsedButUndefinedFunctionTable i ()
                         else ()) in
               let typedArgList = List.map (fun arg -> tc_expression varEnv arg) argList in
               let argTypes = List.map (fun (_, argType) -> lowestTypedefType argType) typedArgList in
               let typedArgs = List.map (fun (typedArg, _) -> typedArg) typedArgList in
               let newFuncName = if isExternal then i else "_c0_" ^ i in
               (* internal functions must be called with prefix _c0_ *)
               if (argsMatch funcParams argTypes) then
               (match funcType with
                      INT -> (IntExpr(IntSharedExpr(FunCall(newFuncName, typedArgs))), funcType)
                    | BOOL -> (BoolExpr(BoolSharedExpr(FunCall(newFuncName, typedArgs))), funcType)
                    | CHAR -> (CharExpr(CharSharedExpr(FunCall(newFuncName, typedArgs))), funcType)
                    | STRING -> (StringExpr(StringSharedExpr(FunCall(newFuncName, typedArgs))), funcType)
                    | VOID -> (VoidExpr(VoidFunCall(newFuncName, typedArgs)), funcType)
                    | FuncPrototype(name, t, ts) -> 
                      (ErrorMsg.error ("functions can't return functions, only function pointers \n");
                         raise ErrorMsg.Error)
                    | Pointer(c) -> (PtrExpr(PtrSharedExpr(FunCall(newFuncName, typedArgs))), funcType)
                    | Array(c) -> (PtrExpr(PtrSharedExpr(FunCall(newFuncName, typedArgs))), funcType)
                    | Alpha(count) -> (AlphaExpr(AlphaSharedExpr(FunCall(newFuncName, typedArgs))), funcType)
                    | _ -> (ErrorMsg.error ("functions can't return structs \n");
                         raise ErrorMsg.Error))
               else
                 (ErrorMsg.error ("parameters don't typecheck: " ^
                     (concat ", " (List.map PrintDatatypes.c0typeToString funcParams)) ^
                    " but got " ^ concat ", "
                    (List.map PrintDatatypes.c0typeToString argTypes) ^ "\n");
                     raise ErrorMsg.Error)
           | _ -> (ErrorMsg.error ("function " ^ i ^ " doesn't exist \n");
                   raise ErrorMsg.Error)) 
  | A.UntypedPostElabFunPtrCall(expr, argList) -> 
      let (typedExpr, exprType) = tc_expression varEnv expr in
      let typedArgList = List.map (fun arg -> tc_expression varEnv arg) argList in
      let argTypes = List.map (fun (expression, expressionType) -> expressionType) typedArgList in
      let typedArgs = List.map (fun (typedArg, _) -> typedArg) typedArgList in
      (match (typedExpr, exprType) with
         (PtrExpr fPtr, (Pointer (FuncPrototype(name,retType,paramTypes)))) -> 
           if argsMatch paramTypes argTypes
           then
             (addTypeToSharedExpr (FuncPointerDeref(fPtr,typedArgs)) retType, retType)
           else (ErrorMsg.error ("ret or arg types don't match for func ptr\n");
                   raise ErrorMsg.Error)
        | _  -> (ErrorMsg.error ("not right func ptr type: " ^
                                 PrintDatatypes.c0typeToString exprType ^ "\n");
                   raise ErrorMsg.Error))
  | A.UntypedPostElabAddressOfFunction(fName) -> 
      (match M.find !functionMap fName with
         None -> (ErrorMsg.error ("undeclared/undefined function\n");
                     raise ErrorMsg.Error)
       | Some(funcType, funcParams, isDefined, isExternal) -> 
           let fType = FuncPrototype(None, funcType, funcParams) in
           let newFName = (if isExternal then fName else "_c0_" ^ fName) in
           (PtrExpr (AddressOfFunction newFName), Pointer(fType)))
  | A.UntypedPostElabFieldAccessExpr(untypedExpr, fieldName) -> (* dots ONLY *)
      let (typedExp,typee) = tc_expression varEnv untypedExpr in
      let actualType = lowestTypedefType typee in
      (match (typedExp, actualType) with
         (PtrExpr(PtrSharedExpr(inner)), Struct(structName)) ->
            (match inner with
               Ident(_) -> (ErrorMsg.error ("bad expr on LHS\n"); raise ErrorMsg.Error)
             | _ -> 
                (match M.find !structMap structName with
                  Some(fieldMap, true) -> 
                    (match M.find !fieldMap fieldName with
                       Some fieldType -> 
                         (match fieldType with
                           INT -> (IntExpr(IntSharedExpr(FieldAccess(structName, PtrSharedExpr(inner), fieldName))), INT)
                         | BOOL -> (BoolExpr(BoolSharedExpr(FieldAccess(structName, PtrSharedExpr(inner), fieldName))), BOOL)
                         | CHAR -> (CharExpr(CharSharedExpr(FieldAccess(structName, PtrSharedExpr(inner), fieldName))), CHAR)
                         | STRING -> (StringExpr(StringSharedExpr(FieldAccess(structName, PtrSharedExpr(inner), fieldName))),
                                                STRING)
                         | _ -> (PtrExpr(PtrSharedExpr(FieldAccess(structName, PtrSharedExpr(inner), fieldName))), fieldType))
                     | None -> 
                         (ErrorMsg.error ("struct " ^ structName ^
                              " has no A.field with name "
                              ^ fieldName ^ "\n"); raise ErrorMsg.Error))
                | None -> (ErrorMsg.error ("no defined struct with name " ^ structName ^ "\n");
                           raise ErrorMsg.Error)))
        | _ -> (ErrorMsg.error ("bad expr on LHS, or LHS not a struct\n"); raise ErrorMsg.Error))
  | A.UntypedPostElabAlloc(t : c0type) ->
      (match t with
        Alpha _ -> (ErrorMsg.error ("can't alloc alpha\n"); raise ErrorMsg.Error)
      | _ ->
          let baseType = lowestTypedefType t in
          if (isNestedVoidPtr baseType) then
            (ErrorMsg.error ("can't alloc void\n");
            raise ErrorMsg.Error)
          else
          (match baseType with
             Struct(structName) ->
               (match M.find !structMap structName with
                      Some (_, true) -> (PtrExpr(Alloc(baseType)), Pointer(baseType))
                    | _ -> (ErrorMsg.error ("undefined struct\n");
                            raise ErrorMsg.Error))
           | _ -> (PtrExpr(Alloc(baseType)), Pointer(baseType))))
  | A.UntypedPostElabDerefExpr(e : A.untypedPostElabExpr) ->
      let (typedExp, typee) = tc_expression varEnv e in
      let actualType = lowestTypedefType typee in
      (match (typedExp, actualType) with
             (_, Poop) -> (ErrorMsg.error ("can't dereference NULL\n");
                   raise ErrorMsg.Error)
           | (PtrExpr(p), Pointer(c)) -> 
               (match c with
                      INT -> (IntExpr(IntSharedExpr(Deref(p))), c)
                    | BOOL -> (BoolExpr(BoolSharedExpr(Deref(p))), c)
                    | CHAR -> (CharExpr(CharSharedExpr(Deref(p))), c)
                    | STRING -> (StringExpr(StringSharedExpr(Deref(p))), c)
                    | _ -> (PtrExpr(PtrSharedExpr(Deref(p))), c))
           | (AlphaExpr(AlphaSharedExpr(s)), Alpha(count)) -> 
               (AlphaExpr(AlphaSharedExpr(Deref(PtrSharedExpr(s)))), Alpha(count+1))
           | _ -> (ErrorMsg.error ("trying to dereference non-pointer expr\n");
                   raise ErrorMsg.Error))
  | A.UntypedPostElabArrayAlloc(typee, e) ->
      let baseType = lowestTypedefType typee in
      if (isNestedVoidPtr baseType) then
      (ErrorMsg.error ("can't alloc void\n");
        raise ErrorMsg.Error)
      else
      (match lowestTypedefType typee with
             Struct(structName) -> 
              (match M.find !structMap structName with
                     Some(_, true) -> 
                       let (typedExpr, _) = tc_expression varEnv e in
                       (match typedExpr with
                              IntExpr(i) -> (PtrExpr(AllocArray(baseType, i)), Array(baseType))
                            | _ -> (ErrorMsg.error ("can't allocate an array without an intexpr\n");
                                    raise ErrorMsg.Error))
                   | _ -> (ErrorMsg.error ("undeclared/undefined struct\n");
                                    raise ErrorMsg.Error))
           | _ -> let (typedExpr, _) = tc_expression varEnv e in
                       (match typedExpr with
                              IntExpr(i) -> (PtrExpr(AllocArray(baseType, i)), Array(baseType))
                            | _ -> (ErrorMsg.error ("can't allocate an array without an intexpr\n");
                                    raise ErrorMsg.Error)))
  | A.UntypedPostElabArrayAccessExpr(e1, e2) ->
      let (typedExp1, type1) = tc_expression varEnv e1 in
      (match type1 with
             Array(arrayType) ->
               let (typedExp2, type2) = tc_expression varEnv e2 in
               (match (typedExp1, typedExp2) with
                      (PtrExpr(p), IntExpr(i)) ->
                         (match p with
                                Null -> (ErrorMsg.error ("can't have a null array\n");
                                         raise ErrorMsg.Error)
                              | Alloc(_) -> (ErrorMsg.error ("can't access ptrs like arrays\n");
                                             raise ErrorMsg.Error)
                              | _ -> 
                                  (match arrayType with
                                         INT -> (IntExpr(IntSharedExpr(ArrayAccess(p, i))), arrayType)
                                       | BOOL -> (BoolExpr(BoolSharedExpr(ArrayAccess(p, i))), arrayType)
                                       | CHAR -> (CharExpr(CharSharedExpr(ArrayAccess(p, i))), arrayType)
                                       | STRING -> (StringExpr(StringSharedExpr(ArrayAccess(p, i))), arrayType)
                                       | _ -> (PtrExpr(PtrSharedExpr(ArrayAccess(p, i))), arrayType)))
                    | _ -> (ErrorMsg.error ("first expr not an array\n");
                            raise ErrorMsg.Error)))

let rec tc_lval_helper varEnv isNested (lval : A.untypedPostElabLVal) =
  match lval with
        A.UntypedPostElabVarLVal(id) ->
          (match M.find varEnv id with (* is it declared? *)
             Some(typee, isInitialized) ->
               (if not isNested (* if it's not nested we do stuff *)
                then
                (match typee with
                   Struct _ -> 
                     (ErrorMsg.error ("bad type for " ^ id ^ "; can't put struct in local var\n");
                      raise ErrorMsg.Error)
                 | _ -> (TypedPostElabVarLVal(id), typee))
                else
                  if not isInitialized (* if it's nested, it has to be initialized *)
                  then 
                    (ErrorMsg.error ("uninitialized variable " ^ id ^ "\n");
                     raise ErrorMsg.Error)
                  else (TypedPostElabVarLVal(id), typee))
           | None -> (* L6: with type inference, a variable doesn't need to be declared to be used *)
               if not isNested (* standalone variable on the LHS, such as x = _____ *)
               then
                 let () = alphaCount := !alphaCount + 1 in
                 (TypedPostElabVarLVal(id), Alpha(!alphaCount))
               else 
               (* the var lval is part of a struct field access, array access, or pointer dereference,
                * but then it should've been initialized before use, so this is bad *)
                 (ErrorMsg.error ("uninitialized variable " ^ id ^ "\n");
                  raise ErrorMsg.Error))
      | A.UntypedPostElabFieldLVal(untypedLVal,fieldName) ->
          let (typedLVal, lvalType) = tc_lval_helper varEnv isNested untypedLVal in
          (match lvalType with
              Struct(structName) -> 
                (match M.find !structMap structName with
                    Some (fieldMap, true) ->
                      (match M.find !fieldMap fieldName with
                          Some fieldType ->            
                            (match typedLVal with
                               TypedPostElabVarLVal(id) -> 
                                 (ErrorMsg.error ("can't store structs in local vars\n");
                                  raise ErrorMsg.Error)
                             | _ -> (TypedPostElabFieldLVal(structName, typedLVal, fieldName), fieldType))
                        | None -> (ErrorMsg.error ("struct " ^ structName ^
                                                            " has no field with name "
                                                            ^ fieldName ^ "\n");
                                            raise ErrorMsg.Error))
                  | _ -> (ErrorMsg.error ("no defined struct with name " ^ structName ^ "\n");
                          raise ErrorMsg.Error))
            | _ -> (ErrorMsg.error ("LHS lval isn't a struct\n");
                     raise ErrorMsg.Error))
      | A.UntypedPostElabDerefLVal(untypedLVal) ->
          (match tc_lval_helper varEnv isNested untypedLVal with
                 (typedLVal, Pointer(c)) ->
                   if c = Poop
                   then (ErrorMsg.error ("dereferencing a null pointer\n");
                         raise ErrorMsg.Error)
                   else (TypedPostElabDerefLVal(typedLVal), c)
               | _ -> (ErrorMsg.error ("dereferencing a non-pointer\n");
                       raise ErrorMsg.Error))
      | A.UntypedPostElabArrayAccessLVal(untypedLVal,exp) ->
          (match (tc_lval_helper varEnv isNested untypedLVal, tc_expression varEnv exp) with
                 ((typedLVal, Array(c)), (IntExpr(i), INT)) -> (TypedPostElabArrayAccessLVal(typedLVal, i), c)
               | _ -> (ErrorMsg.error ("array access lval didn't typecheck\n");
                       raise ErrorMsg.Error))

let tc_lval varEnv lval =
  match lval with
        A.UntypedPostElabVarLVal(id) -> tc_lval_helper varEnv false lval
      | _ -> tc_lval_helper varEnv true lval
