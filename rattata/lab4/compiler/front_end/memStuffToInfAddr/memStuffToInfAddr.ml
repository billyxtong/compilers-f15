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
        try let (_, structSize) = H.find structDefsMap structTypeName in
            structSize
        with Not_found -> (let () = print_string("struct " ^ structTypeName
                             ^ "not defined before alloc\n") in
                           assert(false))

(* we need the type in order to calculate array offsets *)
let rec handleSharedExpr typee = function
     TmpInfAddrFunCall (fName, args) -> (TmpInfAddrFunCall(fName, args), [])
   | TmpInfAddrDeref (ptrExp) ->
       let (e_result, instrs) = handleMemForExpr ptrExp in
       (TmpInfAddrDeref e_result, instrs)
   (* | TmpInfAddrFieldAccess(structPtr, fieldName) -> *)
   (*     try let (fieldOffsets, _) = H.find structDefsMap structTypeName in *)
   (*     with Not_found -> (let () = print_string("struct " ^ structTypeName *)
   (*                                            ^ "not defined before alloc\n") in *)
   (*                      assert(false)) *)
   | TmpInfAddrArrayAccess (ptrExp, indexExpr) ->
       let (ptr_final, ptr_instrs) = handleMemForExpr ptrExp in
       let (index_final, index_instrs) = handleMemForExpr indexExpr in
       let elemSize = getSizeForType typee in
       (* The number of elems is stored at the address ptr_final - 8 *)
       let numElemsPtr = TmpInfAddrSub64(ptr_final, TmpIntArg (TmpConst 8)) in
       let numElemsExpr = TmpIntSharedExpr (TmpInfAddDeref numElemsPtr) in
       let resultTmp = Tmp (Temp.create()) in
       let errorLabel = GenLabel.create() in
       let doTheAccessLabel = GenLabel.create() in
       (* Note that the operand order for cmp has already been reversed!
          So cmp (a,b) followed by jg will jump is b > a *)
       let indexLowerCheck = TmpInfAddrCmp32(
           TmpIntExpr (TmpIntArg (TmpConst 0)), TmpIntExpr index_final)::
                             TmpInfAddrJump(JL, errorLabel)::[] in
       let indexUpperCheck = TmpInfAddrCmp32(
           TmpIntExpr (numElemsExpr, TmpIntExpr index_final))::
                             TmpInfAddrJump(JGE, errorLabel)::
                             TmpInfAddrJump(JL, doTheAccessLabel)::[] in
       let throwError = TmpInfAddrVoidFunCall("raise",
                              TmpIntExpr (TmpIntArg (TmpConst 12))) in
       let accessOffsetExpr = TmpInfAddrBinop(MUL, getSizeForType typee,
                                              numElemsExpr) in
       let accessPtrExpr = TmpInfAddrAdd64(ptr_final, accessOffsetExpr) in
       let doTheAccess = TmpInfAddrMov(TmpInfAddrDeref accessPtrExpr,
                                       resultTmp) in
      (* Ok now actually put all of the instructions together *)
      (* The array pointer is evaluated first, I checked *)
      let allInstrs = ptr_instrs @ index_instrs @ indexLowerCheck
             @ indexUpperCheck @ TmpInfAddrLabel(errorLabel)::throwError
             ::TmpInfAddrLabel(doTheAccessLabel)::doTheAccess::[] in
      (resultTmp, allInstrs)

                             

       

(* returns (e, instrs) pair *)
and handleMemForExpr = function
      TmpBoolExpr (TmpBoolArg arg) -> (TmpBoolExpr (TmpBoolArg arg), [])
    | TmpPtrExpr (TmpPtrArg arg) -> (TmpPtrExpr (TmpPtrArg arg), [])
    | TmpIntExpr (TmpIntArg arg) -> (TmpIntExpr (TmpIntArg arg), [])
    | TmpIntExpr (TmpInfAddrBinopExpr (op, e1, e2)) ->
        let (e1_result, instrs1) = handleMemForExpr e1 in
        let (e2_result, instrs2) = handleMemForExpr e2 in
        (* We have to make sure we're evaluating e1 before e2. So if
           there were any instructions created that involved evaluating
           e2, we need to make sure e1 happens before. We do this
           by moving e1 to a new tmp beforehand. *)
        let (final_e1, final_instrs1) =
             (match instrs2 with
                 [] -> (e1_result, instrs1)
               | _ -> (let t = Tmp (Temp.create()) in
                       (t, instrs1 @ TmpInfAddrMov(e1_result, t)::[]))) in
        (TmpInfAddrBinop(op, final_e1, e2_result), final_instrs1 @ instrs2)
    | TmpPtrExpr (TmpAlloc typee) -> TmpPtrExpr (TmpPtrSharedExpr
           (* alloc becomes a function call to malloc *) 
             (TmpInfAddrFunCall ("malloc", getSizeForType typee)))
    | TmpBoolExpr (TmpBoolSharedExpr e) -> handleSharedExpr e
    | TmpIntExpr (TmpIntSharedExpr e) -> handleSharedExpr e
    | TmpPtrExpr (TmpPtrSharedExpr e) -> handleSharedExpr e
                                    

let handleMemForInstr = function
      TmpInfAddrJump j -> TmpInfAddrJump j::[]
    | TmpInfAddrLabel lbl -> TmpInfAddrLabel lbl::[]
    | TmpInfAddrMov32 (src, dest) ->
      

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
