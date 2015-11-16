open Datatypesv1
module H = Hashtbl
(* For an array of size n, we allocate n + 8 bytes, store the
   length of the array in the address (a-8), where a is the
   pointer that we use for this array in the rest of the program *)

(* This maps each struct type name to (m, size), where size is the total
   size of the struct and m is
   another map: one that maps each field name to the offset
   from the base pointer to the struct *)
let structDefsMap = H.create 100 

let smallFieldSize = 4
let ptrFieldSize = 8
let ptrAlignment = 8  

let rec makeStructInnerMap fields offset map =
      match fields with
         [] -> offset
       | (fieldType, fieldName)::rest ->
           let fieldSize = (match fieldType with
                               INT -> smallFieldSize
                             | BOOL -> smallFieldSize
                             | Pointer _ -> ptrFieldSize
                             | Array _ -> ptrFieldSize
                             | Struct structName -> (
                                 let (fields, structSize) =
                                   H.find structDefsMap structName in structSize)
                             | _ -> assert(false)) in                               
           let currFieldOffset =
        (* If it's an int or a bool, it's always fine, since
           all addresses with be mod 4. If it's a pointer, buffer it
           so that it is correctly aligned *)
              (if fieldSize = smallFieldSize then offset
              else if offset mod ptrAlignment = 0 then offset
              else offset + (ptrAlignment - (offset mod ptrAlignment))) in
           let nextFieldOffset = currFieldOffset + fieldSize in
           let () = H.add map fieldName currFieldOffset in
           makeStructInnerMap rest nextFieldOffset map
                
let updateStructDefsMap structTypeName fields =
   (* First create the inner map. Assuming the base pointer of the
      struct is 8-byte aligned, this ensures that ints/bools are
      4-byte aligned, and pointers are 8-byte aligned *)
      let initialOffset = 0 in
      let innerMap = H.create (List.length fields) in
      let structTotalSize = makeStructInnerMap fields initialOffset innerMap in
      H.add structDefsMap structTypeName (innerMap, structTotalSize)

let getTypeFromExpr = function
     TmpIntExpr _ -> INT
   | TmpBoolExpr _ -> BOOL
   | TmpPtrExpr _ -> Pointer Poop (* Again, doesn't matter what type of ptr *)

let get32vs64ForType = function
    INT -> BIT32
  | BOOL -> BIT32
  | Pointer _ -> BIT64
  | Array _ -> BIT64
  | _ -> assert(false)

let getSizeForType = function
     INT -> smallFieldSize
   | BOOL -> smallFieldSize
   | Pointer _ -> ptrFieldSize
   | Array _ -> ptrFieldSize
   | Struct structTypeName ->
        (try let (_, structSize) = H.find structDefsMap structTypeName in
            structSize
        with Not_found -> (let () = print_string("struct " ^ structTypeName
                             ^ "not defined before alloc\n") in
                           assert(false)))
   | VOID -> assert(false)
   | TypedefType _ -> assert(false)
   | Poop -> assert(false)

let tmpToTypedExpr t exprType = match exprType with
                         INT -> TmpIntExpr (TmpIntArg (TmpLoc t))
                       | BOOL -> TmpBoolExpr (TmpBoolArg (TmpLoc t))
                       | Pointer _ -> TmpPtrExpr (TmpPtrArg (TmpLoc t))
                       | _ -> assert(false)

let sharedExprToTypedExpr exprType sharedExpr =
     match exprType with
         INT -> TmpIntExpr (TmpIntSharedExpr sharedExpr)
       | BOOL -> TmpBoolExpr (TmpBoolSharedExpr sharedExpr)
       | Pointer _ -> TmpPtrExpr (TmpPtrSharedExpr sharedExpr)
       | _ -> assert(false)

let rec lvalToExpr typee = function
    TmpVarLVal t -> tmpToTypedExpr (TmpVar t) typee
  | TmpDerefLVal lval -> 
           (* Again, type of pointer doesn't matter *)
        let TmpPtrExpr ptrLValExpr = lvalToExpr (Pointer Poop) lval in
        sharedExprToTypedExpr typee (TmpInfAddrDeref ptrLValExpr)
  | TmpFieldAccessLVal (structName, structPtr, fieldName) ->
        let TmpPtrExpr structPtrExpr = lvalToExpr (Pointer Poop) structPtr in
        sharedExprToTypedExpr typee (TmpInfAddrFieldAccess(structName,
                                             structPtrExpr, fieldName))
  | TmpArrayAccessLVal (arrayLVal, idxExpr) ->
        let TmpPtrExpr arrayExpr = lvalToExpr (Pointer Poop) arrayLVal in
        sharedExprToTypedExpr typee (TmpInfAddrArrayAccess(arrayExpr, idxExpr))

(* throws an error if unable to convert to a lval. At this point should
   only have tmps and derefs, I believe *)
let rec exprToLVal = function
    TmpBoolExpr (TmpBoolArg (TmpLoc (TmpVar t))) -> TmpVarLVal t
  | TmpBoolExpr (TmpBoolArg (TmpLoc (TmpDeref t))) -> TmpDerefLVal (TmpVarLVal t)
  | TmpBoolExpr (TmpBoolSharedExpr (TmpInfAddrDeref p)) ->
           TmpDerefLVal (exprToLVal (TmpPtrExpr p))
  | TmpIntExpr (TmpIntArg (TmpLoc (TmpVar t))) -> TmpVarLVal t
  | TmpIntExpr (TmpIntArg (TmpLoc (TmpDeref t))) -> TmpDerefLVal (TmpVarLVal t)
  | TmpIntExpr (TmpIntSharedExpr (TmpInfAddrDeref p)) ->
           TmpDerefLVal (exprToLVal (TmpPtrExpr p))
  | TmpPtrExpr (TmpPtrArg (TmpLoc (TmpVar t))) -> TmpVarLVal t
  | TmpPtrExpr (TmpPtrArg (TmpLoc (TmpDeref t))) -> TmpDerefLVal (TmpVarLVal t)
  | TmpPtrExpr (TmpPtrSharedExpr (TmpInfAddrDeref p)) ->
           TmpDerefLVal (exprToLVal (TmpPtrExpr p))
  | _ -> assert(false)                           

let makeAccessInstr elemType resultTmp accessPtrExpr = match elemType with
                    BOOL -> TmpInfAddrMov(BIT32, TmpBoolExpr (TmpBoolSharedExpr
                               (TmpInfAddrDeref accessPtrExpr)), resultTmp)
                  | INT -> TmpInfAddrMov(BIT32, TmpIntExpr (TmpIntSharedExpr
                               (TmpInfAddrDeref accessPtrExpr)), resultTmp)
                  | Pointer _ -> TmpInfAddrMov(BIT64, TmpPtrExpr (TmpPtrSharedExpr
                               (TmpInfAddrDeref accessPtrExpr)), resultTmp)
                  | _ -> assert(false)

(* All args to fun calls should be temps at this point, because that's
   what happens in generalToInfAddr *)
let rec allArgsAreTmps = function
    [] -> true
  | TmpIntExpr (TmpIntArg (TmpLoc t))::rest -> allArgsAreTmps rest
  | _ -> false                                         

(* we need the type in order to calculate array offsets *)
let rec handleSharedExpr exprType = function
     TmpInfAddrFunCall (fName, args) ->
        let () = assert(allArgsAreTmps args) in
        (* move this into a new tmp first to make sure we only evaluate it once. *)
        let t = Tmp (Temp.create()) in
        (tmpToTypedExpr (TmpVar t) exprType, TmpInfAddrMov (get32vs64ForType exprType,
               sharedExprToTypedExpr exprType (TmpInfAddrFunCall(fName, args)),
               TmpVarLVal t)::[])
   | TmpInfAddrDeref (ptrExp) ->
       let (TmpPtrExpr e_result, instrs) = handleMemForExpr (TmpPtrExpr ptrExp) in
       let nullCheckInstrs = handleNullPointerCheck e_result in
       
       (sharedExprToTypedExpr exprType (TmpInfAddrDeref e_result),
        instrs @ nullCheckInstrs)
   | TmpInfAddrFieldAccess(structTypeName, structPtr, fieldName) ->
       let (fieldPtr, instrs) = getStructAccessPtr structTypeName
                                structPtr fieldName in
        let resultTmp = Tmp (Temp.create()) in
        let accessInstr = makeAccessInstr exprType (TmpVarLVal resultTmp) fieldPtr in
        (tmpToTypedExpr (TmpVar resultTmp) exprType, instrs @ accessInstr::[])
   | TmpInfAddrArrayAccess (ptrExp, indexExpr) ->
       let (accessPtrExpr, instrs) = getArrayAccessPtr exprType ptrExp indexExpr in
       let resultTmp = Tmp (Temp.create()) in
       let doTheAccess = makeAccessInstr exprType (TmpVarLVal resultTmp)
                         accessPtrExpr in
       let resultTmpExpr = tmpToTypedExpr (TmpVar resultTmp) exprType in
       (resultTmpExpr, instrs @ doTheAccess :: [])
   | TmpLValExpr lval ->
       let (handledLVal, instrsBeforeRHS, instrsAfterRHS) =
           handleMemForLVal exprType lval in
       let lvalAsExpr = lvalToExpr exprType handledLVal in
       (* In this case, there is no RHS so we can put them wherever. This just matters
          for things like A[*p] = 1/0 where p is NULL *)
       (lvalAsExpr, instrsBeforeRHS @ instrsAfterRHS)

(* returns (e, instrs) pair *)
and handleMemForExpr = function
      TmpBoolExpr (TmpBoolArg arg) -> (TmpBoolExpr (TmpBoolArg arg), [])
    | TmpPtrExpr (TmpPtrArg arg) -> (TmpPtrExpr (TmpPtrArg arg), [])
    | TmpIntExpr (TmpIntArg arg) -> (TmpIntExpr (TmpIntArg arg), [])
    | TmpIntExpr (TmpInfAddrBinopExpr (op, e1, e2)) ->
        let (TmpIntExpr e1_result, instrs1) = handleMemForExpr (TmpIntExpr e1) in
        let (TmpIntExpr e2_result, instrs2) = handleMemForExpr (TmpIntExpr e2) in
        (* We have to make sure we're evaluating e1 before e2. So if
           there were any instructions created that involved evaluating
           e2, we need to make sure e1 happens before. We do this
           by moving e1 to a new tmp beforehand. *)
        let (final_e1, final_instrs1) =
             (match instrs2 with
                 [] -> (e1_result, instrs1)
               | _ -> (let t = Tmp (Temp.create()) in (TmpIntArg (TmpLoc (TmpVar t)),
                       instrs1 @ TmpInfAddrMov(BIT32, TmpIntExpr e1_result,
                                               (TmpVarLVal t))::[])))
            in
        (TmpIntExpr (TmpInfAddrBinopExpr(op, final_e1, e2_result)),
         final_instrs1 @ instrs2)
    | TmpPtrExpr (TmpAlloc typee) -> 
           (* alloc becomes a function call to calloc. We're going to just
              calloc 1 element of the appropriate size. Using calloc
              because it zero-initalizes memory. *)
             (TmpPtrExpr (TmpPtrSharedExpr (TmpInfAddrFunCall ("calloc",
              TmpIntExpr (TmpIntArg (TmpConst 1))::
              TmpIntExpr (TmpIntArg (TmpConst (getSizeForType typee)))::[]))),
              [])
    | TmpPtrExpr (TmpAllocArray (elemType, numElems)) ->
       (* Remember! We allocate an extra 8 bytes and store the length
          in the address p - 8, where p is the address we return here. *)
       let spaceForLength = 8 in
       let extraElemsForLength = (* how many extra elems do we need
                      to alloc to get 8 bytes for the length? *)
         (if getSizeForType elemType = spaceForLength then 1 else 2) in
       let (TmpIntExpr numElemsExpr, instrsForNumElems) =
          handleMemForExpr (TmpIntExpr numElems) in
      (* check that numElems is nonnegative. *)
       let doAllocLabel = GenLabel.create () in
       let throwError = TmpInfAddrVoidFunCall("raise",
                                TmpIntExpr (TmpIntArg (TmpConst 12))::[]) in
       let checkNumElemsNonnegative = (if !OptimizeFlags.safeMode then
           TmpInfAddrBoolInstr
           (TmpInfAddrCmp(BIT32, TmpIntExpr(TmpIntArg(TmpConst 0)),
                          TmpIntExpr numElemsExpr))
           ::TmpInfAddrJump(JGE, doAllocLabel)::throwError
           ::TmpInfAddrLabel doAllocLabel::[] else []) in
       let numElemsToAlloc = TmpIntExpr (TmpInfAddrBinopExpr(ADD, numElemsExpr,
                              TmpIntArg (TmpConst extraElemsForLength))) in
       let funCallExpr = TmpPtrSharedExpr (TmpInfAddrFunCall ("calloc",
             numElemsToAlloc::
             TmpIntExpr (TmpIntArg (TmpConst (getSizeForType elemType)))
                                                      ::[])) in
       let funCallResult = Tmp (Temp.create()) in
       let storeFunCall = TmpInfAddrMov(BIT64, TmpPtrExpr funCallExpr,
                                        TmpVarLVal funCallResult) in
       let finalExpr = TmpPtrExpr (TmpInfAddrPtrBinop (PTR_ADD,
                                   TmpPtrArg (TmpLoc (TmpVar funCallResult)),
                                   TmpIntArg (TmpConst spaceForLength))) in
       (* Actually store the length! *)
       let lengthLoc = TmpVarLVal funCallResult in
       let storeLengthInstr = TmpInfAddrMov(BIT32, TmpIntExpr numElemsExpr,
                                            TmpDerefLVal lengthLoc) in
       (finalExpr, instrsForNumElems @ checkNumElemsNonnegative
                   @ storeFunCall::storeLengthInstr::[])
    | TmpBoolExpr (TmpBoolSharedExpr e) -> handleSharedExpr BOOL e
    | TmpIntExpr (TmpIntSharedExpr e) -> handleSharedExpr INT e
    | TmpPtrExpr (TmpPtrSharedExpr e) -> handleSharedExpr (Pointer VOID) e
                      (* Note: we just need to know that it's a pointer,
                         we don't care what type. I'm putting VOID in
                         because if for some reason we do use the
                         pointer time, this will probably throw an error
                         and let us know that something weird is afoot. *)
    | TmpPtrExpr (TmpInfAddrPtrBinop(op, ptrExp, intExp)) ->
        (* Again, make sure we evaluate ptrExp before intExp, see above *)
        let (TmpPtrExpr ptr_result, instrs1) =
          handleMemForExpr (TmpPtrExpr ptrExp) in
        let (TmpIntExpr int_result, instrs2) =
          handleMemForExpr (TmpIntExpr intExp) in
        let (final_ptr_expr, final_instrs1) = 
             (match instrs2 with
                 [] -> (ptr_result, instrs1)
               | _ -> (let t = Tmp (Temp.create()) in (TmpPtrArg (TmpLoc (TmpVar t)),
                       instrs1 @
                       TmpInfAddrMov(BIT64, TmpPtrExpr ptr_result,
                                     (TmpVarLVal t))::[])))
            in
        (TmpPtrExpr (TmpInfAddrPtrBinop(op, final_ptr_expr, int_result)),
         final_instrs1 @ instrs2)

and getStructAccessPtr structTypeName structPtr fieldName =
    try
        let (fieldOffsets, _) = H.find structDefsMap structTypeName in
        let (structPtrFinal, structPtrInstrs) =
            (match structPtr with
                TmpPtrSharedExpr (TmpInfAddrDeref p) ->
                   let (TmpPtrExpr pFinal, pInstrs) = handleMemForExpr (TmpPtrExpr p) in
                   (pFinal, pInstrs)
              | TmpPtrSharedExpr (TmpInfAddrArrayAccess (arrayExpr, idxExpr)) ->
                 getArrayAccessPtr (Struct structTypeName) arrayExpr idxExpr
              | TmpPtrSharedExpr (TmpInfAddrFieldAccess (innerStructTypeName,
                                              innerStructPtr, innerFieldName)) ->
                getStructAccessPtr innerStructTypeName innerStructPtr innerFieldName
              | _ -> assert(false)) in
        let structPtrTmp = Tmp (Temp.create()) in
        let storeStructPtr = TmpInfAddrMov(BIT64, TmpPtrExpr structPtrFinal,
                                           TmpVarLVal structPtrTmp) in
        let accessOffset = H.find fieldOffsets fieldName in
        let fieldPtrExpr = TmpInfAddrPtrBinop (PTR_ADD,
                             TmpPtrArg (TmpLoc (TmpVar structPtrTmp)),
                             TmpIntArg (TmpConst accessOffset)) in
        let fieldPtrFinal = Tmp (Temp.create()) in
        let storeFieldPtr = TmpInfAddrMov(BIT64, TmpPtrExpr fieldPtrExpr,
                                          TmpVarLVal fieldPtrFinal) in
        let nullCheckInstrs =
            handleNullPointerCheck (TmpPtrArg (TmpLoc (TmpVar structPtrTmp))) in
        (TmpPtrArg (TmpLoc (TmpVar fieldPtrFinal)),
         structPtrInstrs @ storeStructPtr :: storeFieldPtr :: [] @ nullCheckInstrs)
    with Not_found -> let () = print_string("struct " ^ structTypeName
                           ^ "not defined before alloc\n") in assert(false)

and handleNullPointerCheck ptr =
     if not !OptimizeFlags.safeMode then [] else
     let doAccessLabel = GenLabel.create () in
     let throwError = TmpInfAddrVoidFunCall("raise",
                                TmpIntExpr (TmpIntArg (TmpConst 12))::[]) in
     let nullCheck = TmpInfAddrBoolInstr (TmpInfAddrCmp(BIT64,
           TmpPtrExpr ptr, TmpPtrExpr (TmpPtrArg (TmpConst 0))))::
           TmpInfAddrJump(JNE, doAccessLabel)::throwError::
           TmpInfAddrLabel(doAccessLabel)::[]
     in nullCheck                               

and getArrayAccessPtr elemType ptrExp indexExpr =
       let (TmpPtrExpr ptr_final, ptr_instrs) =
           handleMemForExpr (TmpPtrExpr ptrExp) in
       let (TmpIntExpr index_final, index_instrs) =
           handleMemForExpr (TmpIntExpr indexExpr) in
       let arrayPtrTmp = Tmp (Temp.create()) in
       let storeArrayPtr = TmpInfAddrMov(BIT64, TmpPtrExpr ptr_final,
                                         TmpVarLVal arrayPtrTmp) in
        let nullCheckInstrs =
            handleNullPointerCheck (TmpPtrArg (TmpLoc (TmpVar arrayPtrTmp))) in
       (* The number of elems is stored at the address ptr_final - 8 *)
       let numElemsPtr = TmpInfAddrPtrBinop(PTR_SUB,
                                     TmpPtrArg (TmpLoc (TmpVar arrayPtrTmp)),
                                            TmpIntArg (TmpConst 8)) in
       let numElemsExpr = TmpIntSharedExpr (TmpInfAddrDeref numElemsPtr) in
       let errorLabel = GenLabel.create() in
       let doTheAccessLabel = GenLabel.create() in
       (* Note that the operand order for cmp has already been reversed!
          So cmp (a,b) followed by jg will jump is b > a *)
       let indexLowerCheck = TmpInfAddrBoolInstr (TmpInfAddrCmp(BIT32,
           TmpIntExpr (TmpIntArg (TmpConst 0)), TmpIntExpr index_final))::
           TmpInfAddrJump(JL, errorLabel)::[] in
       let indexUpperCheck = TmpInfAddrBoolInstr (TmpInfAddrCmp(BIT32,
           TmpIntExpr numElemsExpr, TmpIntExpr index_final))::
                             TmpInfAddrJump(JGE, errorLabel)::
                             TmpInfAddrJump(JL, doTheAccessLabel)::[] in
       let throwError = TmpInfAddrVoidFunCall("raise",
                 (* We're supposed to raise signal 12 (sigusr2) on
                    memor errors *)
                              TmpIntExpr (TmpIntArg (TmpConst 12))::[]) in
       let accessOffsetTmp = Tmp (Temp.create()) in
       let accessOffsetExpr = TmpInfAddrBinopExpr(MUL,
                          TmpIntArg (TmpConst (getSizeForType elemType)),
                                      index_final) in
       let storeAccessOffset = TmpInfAddrMov(BIT32, TmpIntExpr accessOffsetExpr,
                                             TmpVarLVal accessOffsetTmp) in
       let maskOffsetExpr = TmpInfAddrMaskUpper accessOffsetTmp in
       let accessPtrExpr = TmpInfAddrPtrBinop(PTR_ADD,
                                    TmpPtrArg (TmpLoc (TmpVar arrayPtrTmp)),
                                    TmpIntArg (TmpLoc (TmpVar accessOffsetTmp))) in
       let accessPtrFinal = Tmp (Temp.create()) in
       let storeAccessPtr = TmpInfAddrMov(BIT64, TmpPtrExpr accessPtrExpr,
                                          TmpVarLVal accessPtrFinal) in
       (* Does where I put the storeAccessPtr in this order matter? *)
      (* Ok now actually put all of the instructions together *)
      (* The array pointer is evaluated first, I checked *)
      let allInstrs = (if !OptimizeFlags.safeMode then
             ptr_instrs @ index_instrs @ storeArrayPtr::nullCheckInstrs @
             indexLowerCheck
             @ indexUpperCheck @ TmpInfAddrLabel(errorLabel)::throwError
             ::TmpInfAddrLabel(doTheAccessLabel)
             :: storeAccessOffset::maskOffsetExpr::storeAccessPtr::[]
             else
             ptr_instrs @ index_instrs @ storeArrayPtr::nullCheckInstrs @
             indexLowerCheck
             @ indexUpperCheck @ TmpInfAddrLabel(errorLabel)::throwError
             ::TmpInfAddrLabel(doTheAccessLabel)
             :: storeAccessOffset::maskOffsetExpr::storeAccessPtr::[]) in
      (TmpPtrArg (TmpLoc (TmpVar accessPtrFinal)), allInstrs)

(* Ok I realized I don't have to redo everything I did for the exprs,
   that makes me feel better *)
(* So derefs and vars are fine. The functions for FieldAccess and ArrayAccess
   should both return tmps, so we can just use those too! Yay *)
(* now returns (lval, instrsBeforeRHS, instrsAfterRHS) *)      
and handleMemForLVal typee = function
    TmpVarLVal t -> (TmpVarLVal t, [], [])
  | TmpDerefLVal ptr ->
    let (ptrFinal, ptrInstrsBeforeRHS, ptrInstrsAfterRHS) =
      (* recursive call: all instrs we use for a nested thing need to happen
         before RHS *)
        handleMemForLVal (Pointer Poop) ptr in
    let TmpPtrExpr ptrFinalAsExpr = lvalToExpr (Pointer Poop) ptrFinal in
    let nullCheckInstrs = handleNullPointerCheck ptrFinalAsExpr in
    (* We know that the inner element is a pointer, what type doesn't matter *)
            (TmpDerefLVal(ptrFinal), ptrInstrsBeforeRHS @ ptrInstrsAfterRHS,
             nullCheckInstrs)
  | TmpFieldAccessLVal (structName, structptr, fieldName) ->
      (* We know that structptr is a pointer of some kind; doesn't matter what *)
      let TmpPtrExpr structPtrExpr = lvalToExpr (Pointer Poop) structptr in
      let (fieldAccessPtr, instrs) = getStructAccessPtr structName
          structPtrExpr fieldName in
      let fieldAccessPtrLVal = TmpDerefLVal (exprToLVal (TmpPtrExpr fieldAccessPtr))
      (* everything comes before RHS I think? *)
      in (fieldAccessPtrLVal, instrs, [])
  | TmpArrayAccessLVal (arrayLVal, idxExpr) ->
      let TmpPtrExpr arrayExpr = lvalToExpr (Pointer Poop) arrayLVal in
      let (TmpIntExpr idxExprFinal, idxInstrs) =
           handleMemForExpr (TmpIntExpr idxExpr) in
      let (arrayAccessPtr, arrayInstrs) =
           getArrayAccessPtr typee arrayExpr idxExprFinal in
      let arrayAccessPtrLVal = TmpDerefLVal (exprToLVal (TmpPtrExpr arrayAccessPtr)) in
      (* Apparently the whole array (including idx) gets evaluated before RHS?
         I thought there was a case where that wasn't true *)
      (arrayAccessPtrLVal, idxInstrs @ arrayInstrs, [])

let handleMemForInstr = function
      TmpInfAddrJump j -> TmpInfAddrJump j::[]
    | TmpInfAddrLabel lbl -> TmpInfAddrLabel lbl::[]
    | TmpInfAddrMov (opSize, src, dest) ->
      (* Note: dest is evaluated first! *)
        let typee = (if opSize = BIT64 then (Pointer Poop) else getTypeFromExpr src) in
        (* We need to know the size of the type to do array accesses and such, but
           LVal doesn't have the typed constructors (IntExpr, etc), so
           we have to get it from the rhs beforehand *)
        let (destFinal, instrsBeforeSrc, instrsAfterSrc) =
            handleMemForLVal typee dest in
        let (srcFinal, instrsForSrc) = handleMemForExpr src in
        (match srcFinal with
             TmpIntExpr (TmpInfAddrBinopExpr _) -> let srcFinalTmp = Tmp (Temp.create()) in
                            let storeSrc = TmpInfAddrMov(opSize, srcFinal,
                                                         TmpVarLVal srcFinalTmp) in
                            instrsBeforeSrc @ instrsForSrc @ [storeSrc] @ instrsAfterSrc
                            @ TmpInfAddrMov(opSize, TmpIntExpr (TmpIntArg (
                                    TmpLoc (TmpVar srcFinalTmp))), destFinal)::[]
           | _ -> instrsBeforeSrc @ instrsForSrc @ instrsAfterSrc @
                  TmpInfAddrMov (opSize, srcFinal, destFinal)::[])
                                                                                    
    | TmpInfAddrReturn (retSize, arg) ->
        let (argFinal, instrsForArg) = handleMemForExpr arg in
        instrsForArg @ TmpInfAddrReturn (retSize, argFinal)::[]
    | TmpInfAddrVoidFunCall (fName, args) ->
        let () = assert(allArgsAreTmps args) in
        TmpInfAddrVoidFunCall (fName, args)::[]
    | TmpInfAddrBoolInstr (TmpInfAddrTest (e1, e2)) ->
        let (TmpBoolExpr e1_final, instrs1) = handleMemForExpr (TmpBoolExpr e1) in
        let (TmpBoolExpr e2_final, instrs2) = handleMemForExpr (TmpBoolExpr e2) in
        instrs1 @ instrs2 @
        TmpInfAddrBoolInstr (TmpInfAddrTest (e1_final, e2_final))::[]
    | TmpInfAddrBoolInstr (TmpInfAddrCmp (opSize, e1, e2)) ->
        let (e1_final, instrs1) = handleMemForExpr e1 in
        let (e2_final, instrs2) = handleMemForExpr e2 in
        instrs1 @ instrs2 @
        TmpInfAddrBoolInstr (TmpInfAddrCmp (opSize, e1_final, e2_final))::[]
    | TmpInfAddrMaskUpper _ -> assert(false)

let handleMemForFunDef (fName, tmpParams, instrs) =
    TmpInfAddrFunDef(fName, tmpParams,
         List.flatten (List.map handleMemForInstr instrs))
      

let rec handleMemStuff (prog: tmpInfAddrGlobalDecl list) =
    match prog with
       [] -> []
     | TmpStructDef(structTypeName, fields)::rest ->
         let () = updateStructDefsMap structTypeName fields in handleMemStuff rest
     | TmpInfAddrFunDef(fName, tmpParams, instrs)::rest ->
         handleMemForFunDef(fName, tmpParams, instrs)::handleMemStuff rest
