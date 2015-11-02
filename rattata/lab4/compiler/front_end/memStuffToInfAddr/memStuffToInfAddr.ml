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
        (sharedExprToTypedExpr exprType (TmpInfAddrFunCall(fName, args)), [])
   | TmpInfAddrDeref (ptrExp) ->
       let (TmpPtrExpr e_result, instrs) = handleMemForExpr (TmpPtrExpr ptrExp) in
       (sharedExprToTypedExpr exprType (TmpInfAddrDeref e_result), instrs)
   | TmpInfAddrFieldAccess(structTypeName, structPtr, fieldName) ->
       handleStructAccess exprType structTypeName structPtr fieldName
   | TmpInfAddrArrayAccess (ptrExp, indexExpr) ->
       handleArrayAccess exprType ptrExp indexExpr

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
               | _ -> (let t = Tmp (Temp.create()) in (TmpIntArg (TmpLoc t),
                       instrs1 @ TmpInfAddrMov(BIT32, TmpIntExpr e1_result, t)::[])))
            in
        (TmpIntExpr (TmpInfAddrBinopExpr(op, final_e1, e2_result)),
         final_instrs1 @ instrs2)
    | TmpPtrExpr (TmpAlloc typee) -> 
           (* alloc becomes a function call to malloc *) 
             (TmpPtrExpr (TmpPtrSharedExpr (TmpInfAddrFunCall ("malloc",
              TmpIntExpr (TmpIntArg (TmpConst (getSizeForType typee)))::[]))),
              [])
    | TmpPtrExpr (TmpAllocArray (elemType, numElems)) ->
       (* Remember! We allocate an extra 8 bytes and store the length
          in the address p - 8, where p is the address we return here. *)
      (* Not sure if we need to deal with initializing memory here? *)
       let spaceForLength = 8 in
       let sizeForMalloc = getSizeForType elemType + spaceForLength in 
       let funCallExpr = TmpPtrSharedExpr (TmpInfAddrFunCall ("malloc",
             [TmpIntExpr (TmpIntArg (TmpConst sizeForMalloc))])) in
       let finalExpr = TmpPtrExpr (TmpInfAddrPtrBinop (PTR_ADD, funCallExpr,
                                   TmpIntArg (TmpConst spaceForLength))) in
       (finalExpr, [])
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
               | _ -> (let t = Tmp (Temp.create()) in (TmpPtrArg (TmpLoc t),
                       instrs1 @
                       TmpInfAddrMov(BIT64, TmpPtrExpr ptr_result, t)::[])))
            in
        (TmpPtrExpr (TmpInfAddrPtrBinop(op, final_ptr_expr, int_result)),
         final_instrs1 @ instrs2)

and handleStructAccess exprType structTypeName structPtr fieldName =
    try
        let (fieldOffsets, _) = H.find structDefsMap structTypeName in
        let (TmpPtrExpr structPtrFinal, structPtrInstrs) =
             handleMemForExpr (TmpPtrExpr structPtr) in
        let accessOffset = H.find fieldOffsets fieldName in
        let resultTmp = Tmp (Temp.create()) in
        let fieldPtrExpr = TmpInfAddrPtrBinop (PTR_ADD, structPtrFinal,
                             TmpIntArg (TmpConst accessOffset)) in
        let accessInstr = makeAccessInstr exprType resultTmp fieldPtrExpr in
        (tmpToTypedExpr resultTmp exprType, structPtrInstrs @ accessInstr::[])
    with Not_found -> let () = print_string("struct " ^ structTypeName
                           ^ "not defined before alloc\n") in assert(false)

and handleArrayAccess elemType ptrExp indexExpr =
       let (TmpPtrExpr ptr_final, ptr_instrs) =
           handleMemForExpr (TmpPtrExpr ptrExp) in
       let (TmpIntExpr index_final, index_instrs) =
           handleMemForExpr (TmpIntExpr indexExpr) in
       (* The number of elems is stored at the address ptr_final - 8 *)
       let numElemsPtr = TmpInfAddrPtrBinop(PTR_SUB, ptr_final,
                                            TmpIntArg (TmpConst 8)) in
       let numElemsExpr = TmpIntSharedExpr (TmpInfAddrDeref numElemsPtr) in
       let resultTmp = Tmp (Temp.create()) in
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
       let accessOffsetExpr = TmpInfAddrBinopExpr(MUL,
                          TmpIntArg (TmpConst (getSizeForType elemType)),
                                      index_final) in
       let accessPtrExpr = TmpInfAddrPtrBinop(PTR_ADD, ptr_final,
                                              accessOffsetExpr) in
       let doTheAccess = makeAccessInstr elemType resultTmp accessPtrExpr in
      (* Ok now actually put all of the instructions together *)
      (* The array pointer is evaluated first, I checked *)
      let allInstrs = ptr_instrs @ index_instrs @ indexLowerCheck
             @ indexUpperCheck @ TmpInfAddrLabel(errorLabel)::throwError
             ::TmpInfAddrLabel(doTheAccessLabel)::doTheAccess::[] in
      let resultTmpExpr = tmpToTypedExpr resultTmp elemType in
      (resultTmpExpr, allInstrs)

let rec lvalToExpr typee = function
    TmpVarLVal t -> tmpToTypedExpr t typee
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

(* After this, the only lvals should be tmps *)
(* Ok I realized I don't have to redo everything I did for the exprs,
   that makes me feel better *)
(* So derefs and vars are fine. The functions for FieldAccess and ArrayAccess
   should both return tmps, so we can just use those too! Yay *)
let rec handleMemForLVal = function
    TmpVarLVal t -> (TmpVarLVal t, [])
  | TmpDerefLVal ptr -> let (ptrFinal, instrs) = handleMemForLVal ptr in
                        (TmpDerefLVal(ptrFinal), instrs)                        

let handleMemForInstr = function
      TmpInfAddrJump j -> TmpInfAddrJump j::[]
    | TmpInfAddrLabel lbl -> TmpInfAddrLabel lbl::[]
    | TmpInfAddrMov (opSize, src, dest) ->
        let (srcFinal, instrsForSrc) = handleMemForExpr src in
        instrsForSrc @ TmpInfAddrMov(opSize, srcFinal, dest)::[]
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
