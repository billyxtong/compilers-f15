open Typechecker

let rec matchTypes (t1 : c0type) (t2 : c0type) =
  match (t1, t2) with
        (INT, INT) -> true
      | (BOOL, BOOL) -> true
      | (Pointer(c1), Pointer(c2)) -> matchTypes c1 c2
      | (Array(c1), Array(c2)) -> matchTypes c1 c2
      | (Struct(i1), Struct(i2)) -> (i1 = i2)
      | (Poop, Poop) -> true
      | _ -> false

let rec tc_lval varEnv (lval : untypedPostElabLVal) =
  match lval with
        UntypedPostElabVarLVal(id) -> 
          (match M.find varEnv id with (* is it declared? *)
                   Some(typee, isInitialized) -> 
                     (if isInitialized (* declared and initialized, all good *)
                      then 
                        (match typee with
                               INT -> TypedPostElabVarLVal(id)
                             | BOOL -> TypedPostElabVarLVal(id)
                             | Pointer(p) -> TypedPostElabVarLVal(id)
                             | Array(a) -> TypedPostElabVarLVal(id)
                             | _ -> (ErrorMsg.error ("bad type for " ^ id ^ "\n");
                                     raise ErrorMsg.Error))
                      else 
                        (ErrorMsg.error ("uninitialized variable " ^ id ^ "\n");
                         raise ErrorMsg.Error))
                 | None -> (ErrorMsg.error ("undeclared variable " ^ id ^ "\n");
                            raise ErrorMsg.Error)) 
      | UntypedPostElabFieldLVal(untypedLVal,id) -> 
          let typedLVal = tc_lval varEnv untypedLVal in
          (match typedLVal with
                 TypedPostElabVarLVal(i) -> ""
               | TypedPostElabFieldLVal(t,i) -> ""
               | 
          TypedPostElabFieldLVal(tc_lval varEnv untypedLVal, id)
      | UntypedPostElabDerefLVal(untypedLVal) -> 
          TypedPostElabDerefLVal(tc_lval varEnv untypedLVal)
      | UntypedPostElabArrayAccessLVal(untypedLVal,exp) -> 

let rec tc_expression varEnv (expression : untypedPostElabExpr) =
  match expression with
    UntypedPostElabConstExpr (constant, typee) -> 
      (match typee with
             INT -> (IntExpr(IntConst(constant)), INT)
           | BOOL -> (BoolExpr(BoolConst(constant)), BOOL)
           | _ -> (ErrorMsg.error ("constants must be int or bool\n");
                   raise ErrorMsg.Error))
  | UntypedPostElabNullExpr -> (PtrExpr(Null), Poop)
  | UntypedPostElabIdentExpr i -> 
      (match M.find varEnv i with
             Some (typee, isInit) -> 
               (match typee with
                      INT -> (IntExpr(IntSharedExpr(Ident(i))), typee)
                    | BOOL -> (BoolExpr(BoolSharedExpr(Ident(i))), typee)
                    | Pointer(c) -> (PtrExpr(PtrSharedExpr(Ident(i))), typee)
                    | Array(c) -> (PtrExpr(PtrSharedExpr(Ident(i))), typee)
                    | _ -> (ErrorMsg.error ("can't reference a struct without ptr\n");
                            raise ErrorMsg.Error))
           | None -> (ErrorMsg.error ("undeclared variable " ^ id ^ "\n");
                            raise ErrorMsg.Error))
  | UntypedPostElabBinop (e1, op, e2) -> 
      let (tcExpr1, type1) = tc_expression varEnv e1 in
      let (tcExpr2, type2) = tc_expression varEnv e2 in
      (match op with
             GT -> (match (tcExpr1, tcExpr2) with
                          (IntExpr(exp1), IntExpr(exp2)) -> (BoolExpr(GreaterThan(exp1, exp2)), BOOL)
                        | _ -> (ErrorMsg.error ("greater than expression didn't typecheck \n");
                               raise ErrorMsg.Error))
           | LT -> (match (tcExpr1, tcExpr2) with
                          (IntExpr(exp1), IntExpr(exp2)) -> (BoolExpr(LessThan(exp1, exp2)), BOOL)
                        | _ -> (ErrorMsg.error ("less than expression didn't typecheck \n");
                               raise ErrorMsg.Error))
           | DOUBLE_EQ -> 
               (match (tcExpr1, tcExpr2) with
                      (IntExpr(exp1), IntExpr(exp2)) -> (BoolExpr(IntEquals(exp1, exp2)), BOOL)
                    | (BoolExpr(exp1), BoolExpr(exp2)) -> (BoolExpr(BoolEquals(exp1, exp2)), BOOL)
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
  | UntypedPostElabNot e' -> 
      let (tcExpr,t) = tc_expression varEnv e' in
      (match tcExpr with
             BoolExpr(exp1) -> (BoolExpr(LogNot(exp1)), BOOL)
           | _ -> ErrorMsg.error ("not expression didn't typecheck \n");
                  raise ErrorMsg.Error)
  | UntypedPostElabTernary(e1, e2, e3) ->
      let (tcExpr1,type1) = tc_expression varEnv e1 in
      let (tcExpr2,type2) = tc_expression varEnv e2 in
      let (tcExpr3,type3) = tc_expression varEnv e3 in
      (match tcExpr1 with
             BoolExpr(exp1) -> 
               (match (tcExpr2, tcExpr3) with
                      (IntExpr(_), IntExpr(_)) -> (IntExpr(IntSharedExpr(Ternary(exp1, tcExpr2, tcExpr3))), INT)
                    | (BoolExpr(_), BoolExpr(_)) -> (BoolExpr(BoolSharedExpr(Ternary(exp1, tcExpr2, tcExpr3))), BOOL)
                    | (PtrExpr(_), PtrExpr(_)) -> 
                        if matchTypes type2 type3
                        then                      
                          (PtrExpr(PtrSharedExpr(Ternary(exp1, tcExpr2, tcExpr3))), type2)
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
               let typedArgs = List.map (#1 (tc_expression varEnv)) argList in
               let newFuncName = if isExternal then i else "_c0_" ^ i in
               (* internal functions must be called with prefix _c0_ *)
               if (argsMatch typedArgs funcParams) then 
               (match funcType with
                      INT -> (IntExpr(IntSharedExpr(FunCall(newFuncName, typedArgs))), funcType)
                    | BOOL -> (BoolExpr(BoolSharedExpr(FunCall(newFuncName, typedArgs))), funcType)
                    | VOID -> (VoidExpr(VoidFunCall(newFuncName, typedArgs)), funcType)
                    | Pointer(c) -> (PtrExpr(PtrSharedExpr(FunCall(newFuncName, typedArgs))), funcType)
                    | Array(c) -> (PtrExpr(PtrSharedExpr(FunCall(newFuncName, typedArgs))), funcType)
                    | _ -> (ErrorMsg.error ("functions can't return structs \n");
                         raise ErrorMsg.Error))
               else (ErrorMsg.error ("parameters don't typecheck \n");
                     raise ErrorMsg.Error)
           | _ -> (ErrorMsg.error ("function doesn't exist \n");
                   raise ErrorMsg.Error))
  | UntypedPostElabFieldAccessExpr(e,fieldName) ->
      let (typedExp,typee) = tc_expression varEnv e in
      match (typedExp, typee) with
            (PtrExpr(PtrSharedExpr(Deref(p))), Struct(structName)) -> 
              (match M.find structMap structName with
                     Some fieldMap -> 
                       (match M.find fieldMap fieldName with
                              Some _ -> (PtrExpr(PtrSharedExpr(FieldAccess())))
                              None -> (ErrorMsg.error ("struct " ^ structName ^ 
                                                    " has no field with name " 
                                                    ^ fieldName ^ "\n");
                                       raise ErrorMsg.Error))
                   | None -> (ErrorMsg.error ("no struct with name " ^ structName ^ "\n");
                              raise ErrorMsg.Error))
  | UntypedPostElabAlloc(t) ->
  | UntypedPostElabDerefExpr(e)  ->
      let (typedExp, typee) = tc_expression varEnv e in
      (match (typedExp, typee) with
             (_, Poop) -> (ErrorMsg.error ("trying to dereference null pointer\n");
                           raise ErrorMsg.Error)
           | (PtrExpr(p), Pointer(c)) -> (PtrExpr(PtrSharedExpr(Deref(p))), c)
           | _ -> (ErrorMsg.error ("trying to dereference non-pointer expr\n");
                   raise ErrorMsg.Error))
  | UntypedPostElabArrayAlloc(c,e)  ->
      let (typedExp, _) = tc_expression varEnv e in
      (match typedExp with
             IntExpr(i) -> PtrExpr(AllocArray(c,i))
           | _ -> (ErrorMsg.error ("can't allocate an array without an intexpr\n");
                  raise ErrorMsg.Error))
  | UntypedPostElabArrayAccessExpr(e1,e2)  -> 
      let (typedExp1, type1) = tc_expression varEnv e1 in
      let (typedExp2, type2) = tc_expression varEnv e2 in
      (match (typedExp1, typedExp2) with
             (PtrExpr(p), IntExpr(i)) -> 
               (match p with
                      Null -> (ErrorMsg.error ("can't have a null array\n");
                               raise ErrorMsg.Error)
                    | Alloc(_) -> (ErrorMsg.error ("can't access ptrs like arrays\n");
                                   raise ErrorMsg.Error)
                    | _ -> PtrExpr(PtrSharedExpr(ArrayAccess(p, i))))

