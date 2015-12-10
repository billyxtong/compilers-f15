open Datatypesv1
open Ast
module H = Hashtbl
module M = Core.Std.Map
open String
open PrintASTs
open PrintDatatypes

let declaredAndUsedButUndefinedFunctionTable = H.create 5
type mytype = ((c0type Core.Std.String.Map.t) ref)* bool
let functionMap = ref Core.Std.String.Map.empty
let typedefMap = ref Core.Std.String.Map.empty 
(* maps typedef'd function name types to their return type and arg types *)
let (structMap : (mytype Core.Std.String.Map.t) ref) = 
  ref (Core.Std.String.Map.empty)

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

let rec matchTypes (t1 : c0type) (t2 : c0type) =
  match (t1, t2) with
        (INT, INT) -> true
      | (BOOL, BOOL) -> true
      | (CHAR, CHAR) -> true
      | (STRING, STRING) -> true
      | (Pointer(c1), Pointer(c2)) -> matchTypes c1 c2
      | (Array(c1), Array(c2)) -> matchTypes c1 c2
      | (Struct(i1), Struct(i2)) -> (i1 = i2)
      | (FuncID(i1), FuncID(i2)) -> (i1 = i2)
      | (Poop, Poop) -> true
      | (Pointer(c), Poop) -> true
      | (Poop, Pointer(c)) -> true
      | _ -> false

let notAStruct (t : c0type) =
  match lowestTypedefType t with
        Struct(_) -> false
      | _ -> true

let rec argsMatch (argTypes : c0type list) (paramTypes : c0type list) =
  match (argTypes, paramTypes) with
        ([], []) -> true
      | ([], p :: ps) -> false
      | (arg :: args, []) -> false
      | (arg :: args, p :: ps) ->
          if matchTypes arg p
          then argsMatch args ps
          else false

let rec matchParamListTypes (paramTypes : c0type list) (params : param list) =
  match (paramTypes, params) with
        ([], []) -> true
      | ([], p :: ps) -> false
      | (p :: ps, []) -> false
      | (p :: ps, (typee, _) :: remainingParams) ->
          let pType = lowestTypedefType p in
          let paramType = lowestTypedefType typee in
          if matchTypes pType paramType
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

let rec uniqueFieldNames (fields : field list) nameTable = 
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

let rec areAnyFuncParamsStructs (l : param list) =
  match l with
        [] -> false
      | (t, _) :: ps ->
          (match lowestTypedefType t with
                 Struct(_) -> true
               | _ -> areAnyFuncParamsStructs ps)

let rec tc_expression varEnv (expression : untypedPostElabExpr) : typedPostElabExpr * c0type =
  match expression with
    UntypedPostElabConstExpr (constant, typee) ->
      (match typee with
             INT -> (IntExpr(IntConst(constant)), INT)
           | BOOL -> (BoolExpr(BoolConst(constant)), BOOL)
           | CHAR -> (CharExpr(CharConst(constant)), CHAR)
           | _ -> (ErrorMsg.error ("constants must be int, bool, or char\n");
                   raise ErrorMsg.Error))
  | UntypedPostElabStringConstExpr(str) -> 
      if List.exists(fun c -> c = 0) str then
      (ErrorMsg.error ("null character cannot appear in strings\n");
        raise ErrorMsg.Error)
      else (StringExpr(StringConst(str)), STRING)
  | UntypedPostElabNullExpr -> (PtrExpr(Null), Poop)
  | UntypedPostElabIdentExpr(i : ident) ->
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
                    | FuncID(i) -> 
                        (ErrorMsg.error ("can't reference a function without ptr\n");
                            raise ErrorMsg.Error)
                    | Pointer(c) -> (PtrExpr(PtrSharedExpr(Ident(i))), actualType)
                    | Array(c) -> (PtrExpr(PtrSharedExpr(Ident(i))), actualType)
                    | _ -> (ErrorMsg.error ("can't reference a struct without ptr\n");
                            raise ErrorMsg.Error))
           | None -> (ErrorMsg.error ("undeclared variable " ^ i ^ "\n");
                            raise ErrorMsg.Error))
  | UntypedPostElabBinop (e1, op, e2) ->
      let (tcExpr1, type1) = tc_expression varEnv e1 in
      let (tcExpr2, type2) = tc_expression varEnv e2 in
      (match op with
             GT -> (match (tcExpr1, tcExpr2) with
                          (IntExpr(exp1), IntExpr(exp2)) -> (BoolExpr(IntGreaterThan(exp1, exp2)), BOOL)
                        | (CharExpr(exp1), CharExpr(exp2)) -> (BoolExpr(CharGreaterThan(exp1, exp2)), BOOL)
                        | _ -> (ErrorMsg.error ("greater than expression didn't typecheck \n");
                               raise ErrorMsg.Error))
           | LT -> (match (tcExpr1, tcExpr2) with
                          (IntExpr(exp1), IntExpr(exp2)) -> (BoolExpr(IntLessThan(exp1, exp2)), BOOL)
                        | (CharExpr(exp1), CharExpr(exp2)) -> (BoolExpr(CharLessThan(exp1, exp2)), BOOL)
                        | _ -> (ErrorMsg.error ("less than expression didn't typecheck \n");
                               raise ErrorMsg.Error))
           | DOUBLE_EQ ->
               (match (tcExpr1, tcExpr2) with
                      (IntExpr(exp1), IntExpr(exp2)) -> (BoolExpr(IntEquals(exp1, exp2)), BOOL)
                    | (BoolExpr(exp1), BoolExpr(exp2)) -> (BoolExpr(BoolEquals(exp1, exp2)), BOOL)
                    | (CharExpr(exp1), CharExpr(exp2)) -> (BoolExpr(CharEquals(exp1, exp2)), BOOL)
                    | (PtrExpr(exp1), PtrExpr(exp2)) -> 
                        if matchTypes type1 type2 && notAStruct type1 && notAStruct type2 then
                        (BoolExpr(PtrEquals(exp1, exp2)), BOOL)
                        else (ErrorMsg.error ("different typed ptrExprs\n");
                           raise ErrorMsg.Error)
                    | _ -> (ErrorMsg.error ("double equals expressions didn't typecheck \n");
                           raise ErrorMsg.Error))
           | LOG_AND -> (match (tcExpr1, tcExpr2) with
                               (BoolExpr(exp1), BoolExpr(exp2)) -> (BoolExpr(LogAnd(exp1, exp2)), BOOL)
                             | _ -> (ErrorMsg.error ("logical and expressions didn't typecheck \n");
                                    raise ErrorMsg.Error))
           | IntBinop(intOp) -> (match (tcExpr1, tcExpr2) with
                          (IntExpr(exp1), IntExpr(exp2)) -> (IntExpr(ASTBinop(exp1, intOp, exp2)), INT)
                        | _ -> (ErrorMsg.error ("int binop expressions didn't typecheck \n");
                               raise ErrorMsg.Error)))
  | UntypedPostElabNot(e' : untypedPostElabExpr) ->
      let (tcExpr,t) = tc_expression varEnv e' in
      (match tcExpr with
             BoolExpr(exp1) -> (BoolExpr(LogNot(exp1)), BOOL)
           | _ -> (ErrorMsg.error ("not expression didn't typecheck \n");
                  raise ErrorMsg.Error))
  | UntypedPostElabTernary(e1, e2, e3) ->
      let (tcExpr1,type1) = tc_expression varEnv e1 in
      let (tcExpr2,type2) = tc_expression varEnv e2 in
      let (tcExpr3,type3) = tc_expression varEnv e3 in
      (match tcExpr1 with
             BoolExpr(exp1) ->
               (match (tcExpr2, tcExpr3) with
                      (IntExpr(_), IntExpr(_)) -> (IntExpr(IntSharedExpr(Ternary(exp1, tcExpr2, tcExpr3))), INT)
                    | (BoolExpr(_), BoolExpr(_)) -> (BoolExpr(BoolSharedExpr(Ternary(exp1, tcExpr2, tcExpr3))), BOOL)
                    | (CharExpr(_), CharExpr(_)) -> (CharExpr(CharSharedExpr(Ternary(exp1, tcExpr2, tcExpr3))), CHAR)
                    | (StringExpr(_), StringExpr(_)) -> (StringExpr(StringSharedExpr(Ternary(exp1, tcExpr2, tcExpr3))), STRING)
                    | (PtrExpr(_), PtrExpr(_)) ->
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
                    | _ -> (ErrorMsg.error ("exprs in ternary have diff. types\n");
                            raise ErrorMsg.Error))
           | _ -> (ErrorMsg.error ("ternary expression didn't typecheck \n");
                   raise ErrorMsg.Error))
  | UntypedPostElabFunCall(i, argList) -> 
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
               if (argsMatch argTypes funcParams) then
               (match funcType with
                      INT -> (IntExpr(IntSharedExpr(FunCall(newFuncName, typedArgs))), funcType)
                    | BOOL -> (BoolExpr(BoolSharedExpr(FunCall(newFuncName, typedArgs))), funcType)
                    | CHAR -> (CharExpr(CharSharedExpr(FunCall(newFuncName, typedArgs))), funcType)
                    | STRING -> (StringExpr(StringSharedExpr(FunCall(newFuncName, typedArgs))), funcType)
                    | VOID -> (VoidExpr(VoidFunCall(newFuncName, typedArgs)), funcType)
                    | FuncID(ident) -> 
                      (ErrorMsg.error ("functions can't return functions, only function pointers \n");
                         raise ErrorMsg.Error)
                    | Pointer(c) -> (PtrExpr(PtrSharedExpr(FunCall(newFuncName, typedArgs))), funcType)
                    | Array(c) -> (PtrExpr(PtrSharedExpr(FunCall(newFuncName, typedArgs))), funcType)
                    | _ -> (ErrorMsg.error ("functions can't return structs \n");
                         raise ErrorMsg.Error))
               else
                 (ErrorMsg.error ("parameters don't typecheck \n");
                     raise ErrorMsg.Error)
           | _ -> (ErrorMsg.error ("function " ^ i ^ " doesn't exist \n");
                   raise ErrorMsg.Error)) (*
  | UntypedPostElabFunPtrCall(expr, argList) -> 
      let (typedExpr, exprType) = tc_expression varEnv expr in
      let typedArgList = List.map (fun arg -> tc_expression varEng arg) argList in
      (match exprType with
         Some()


)      


*)
  | UntypedPostElabAddressOfFunction(ident) -> 
      (match M.find !functionMap ident with
         None -> (ErrorMsg.error ("undeclared/undefined function\n");
                     raise ErrorMsg.Error)
       | Some(funcType, funcParams, isDefined, isExternal) -> 
           )
  | UntypedPostElabFieldAccessExpr(untypedExpr, fieldName) -> (* dots ONLY *)
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
                              " has no field with name "
                              ^ fieldName ^ "\n"); raise ErrorMsg.Error))
                | None -> (ErrorMsg.error ("no defined struct with name " ^ structName ^ "\n");
                           raise ErrorMsg.Error)))
        | _ -> (ErrorMsg.error ("bad expr on LHS, or LHS not a struct\n"); raise ErrorMsg.Error))
  | UntypedPostElabAlloc(t : c0type) ->
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
           | _ -> (PtrExpr(Alloc(baseType)), Pointer(baseType)))
  | UntypedPostElabDerefExpr(e : untypedPostElabExpr) ->
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
           | _ -> (ErrorMsg.error ("trying to dereference non-pointer expr\n");
                   raise ErrorMsg.Error))
  | UntypedPostElabArrayAlloc(typee, e) ->
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
  | UntypedPostElabArrayAccessExpr(e1, e2) ->
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

let rec tc_lval_helper varEnv isNested (lval : untypedPostElabLVal) =
  match lval with
        UntypedPostElabVarLVal(id) ->
          (match M.find varEnv id with (* is it declared? *)
                   Some(typee, isInitialized) ->
                     (if not isNested (* if it's not nested we do stuff *)
                      then
                        (match typee with
                               Struct _ -> (ErrorMsg.error ("bad type for " ^ id ^ "; can't put struct in local var\n");
                                            raise ErrorMsg.Error)
                             | _ ->
                                 (TypedPostElabVarLVal(id), typee))
                      else
                        if not isInitialized (* if it's nested, it has to be initialized *)
                        then (ErrorMsg.error ("uninitialized variable " ^ id ^ "\n");
                              raise ErrorMsg.Error)
                        else
                          (TypedPostElabVarLVal(id), typee))
                 | None -> (ErrorMsg.error ("undeclared variable " ^ id ^ "\n");
                            raise ErrorMsg.Error))
      | UntypedPostElabFieldLVal(untypedLVal,fieldName) ->
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
      | UntypedPostElabDerefLVal(untypedLVal) ->
          (match tc_lval_helper varEnv isNested untypedLVal with
                 (typedLVal, Pointer(c)) ->
                   if c = Poop
                   then (ErrorMsg.error ("dereferencing a null pointer\n");
                         raise ErrorMsg.Error)
                   else (TypedPostElabDerefLVal(typedLVal), c)
               | _ -> (ErrorMsg.error ("dereferencing a non-pointer\n");
                       raise ErrorMsg.Error))
      | UntypedPostElabArrayAccessLVal(untypedLVal,exp) ->
          (match (tc_lval_helper varEnv isNested untypedLVal, tc_expression varEnv exp) with
                 ((typedLVal, Array(c)), (IntExpr(i), INT)) -> (TypedPostElabArrayAccessLVal(typedLVal, i), c)
               | _ -> (ErrorMsg.error ("array access lval didn't typecheck\n");
                       raise ErrorMsg.Error))

let tc_lval varEnv lval =
  match lval with
        UntypedPostElabVarLVal(id) -> tc_lval_helper varEnv false lval
      | _ -> tc_lval_helper varEnv true lval
