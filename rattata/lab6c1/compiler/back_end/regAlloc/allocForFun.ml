module H = Hashtbl
open Datatypesv1
open PrintDatatypes
module A = Array
open Graph  

(* We need two now, for things like movl (%rbx), (-8(%rbp)) *)
let firstSpillReg = Reg R15
let secondSpillReg = Reg R14

let bytesForArg = 8

let listToString i a = String.concat "" (List.map
            (fun x -> string_of_int(i)^": " ^string_of_int(x) ^ ", ") a @["\n"])

let assemLocForColor regArray offsetIncr colorNum =
    if (colorNum < A.length regArray) then Reg(A.get regArray colorNum)
    (* If there are 8 registers but this color is 10 (indexed from 0),
       that means we need there are at least 11 colors, which means we need
       at least 3 * (size of one tmp) bytes of stack memory. Since the
       first stack spot is offsetIncr(rsp), we add one *)
    else MemAddr(RBP, - ((colorNum - (A.length regArray) + 1) * offsetIncr))

let rec makeColorToAssemLocMapFromParams tmpToColorMap paramTmps paramRegs resultMap
    nextArgOffsetAboveRBP =
    match (paramTmps, paramRegs) with
         (paramT :: params, reg :: regs) ->
               let paramColor = (try H.find tmpToColorMap paramT
                                 with Not_found -> assert(false)) in
               let () = H.add resultMap paramColor (Reg reg) in
               makeColorToAssemLocMapFromParams tmpToColorMap params regs resultMap
                 nextArgOffsetAboveRBP
      | ([], _) -> (resultMap, paramRegs) (* we need to know if there were any
                                             paramRegs left over! *)
      | (paramT :: params, []) -> (* what if we ran out of regs? *)
               let currLoc = MemAddr(RBP, nextArgOffsetAboveRBP) in
               let paramColor = (try H.find tmpToColorMap paramT
                                 with Not_found -> assert(false)) in
               let () = H.add resultMap paramColor currLoc in
               makeColorToAssemLocMapFromParams tmpToColorMap params [] resultMap
                 (nextArgOffsetAboveRBP + bytesForArg)

let rec addColorToAssemLocMappingsForProgTmps tmpToColorMap progTmps allocableRegs
    colorToAssemLocMap nextOffsetBelowRBP =
    match progTmps with
         [] -> ()
       | t :: temps ->
               let tColor = (try H.find tmpToColorMap t
                                 with Not_found -> assert(false)) in
               (try let _ = H.find colorToAssemLocMap tColor in
                  (* we already have an assemLoc for this color, move on *)
                  addColorToAssemLocMappingsForProgTmps tmpToColorMap temps
                    allocableRegs (* note: all allocableRegs *) colorToAssemLocMap
                    nextOffsetBelowRBP
                with Not_found ->
           (match allocableRegs with
               reg :: regs ->
                    let () = H.add colorToAssemLocMap tColor (Reg reg) in
                    addColorToAssemLocMappingsForProgTmps tmpToColorMap temps
                    (* don't include curr reg *)
                    regs colorToAssemLocMap nextOffsetBelowRBP
             | [] -> let currLoc = MemAddr(RBP, -nextOffsetBelowRBP) in
                     let () = H.add colorToAssemLocMap tColor currLoc in
                     addColorToAssemLocMappingsForProgTmps tmpToColorMap temps
                     [] colorToAssemLocMap (nextOffsetBelowRBP + bytesForArg)))

let rec makeTmpToAssemLocMap tmpToColorMap tmpList colorToAssemLocMap resultMap =
    let getAssemLoc t = (try let color = H.find tmpToColorMap t in
                             H.find colorToAssemLocMap color
                         with Not_found -> assert(false)) in
    let () = List.iter (fun t -> H.add resultMap (Tmp t) (getAssemLoc t)) tmpList in
    resultMap

let translateTmpArg tbl = function
    TmpConst c -> Const c
  | TmpLoc (TmpVar t) -> (try AssemLoc (H.find tbl t)
                 (* The "with" means there's a tmp that is used, but we
                    never assigned an AssemLoc to it because we never
                    wrote to it. This can happen if the variable was never initialized,
                    but it was ok because the line was unreachable *)
                 with Not_found -> AssemLoc (MemAddr(RSP, -666)))
  | TmpLoc (TmpDeref t) -> try AssemLoc (match (H.find tbl t) with
                                     MemAddr m -> MemAddrDeref m
                                   | Reg r -> RegDeref r
                                   | _ -> assert(false))
                           with Not_found -> failwith "I don't think this can happen"

let translateTmpLoc tbl = function
    TmpVar t -> H.find tbl t
  | TmpDeref t -> (match H.find tbl t with
                       Reg r -> RegDeref r
                     | MemAddr mem -> MemAddrDeref mem
                     | _ -> assert(false))                                        
                  (* we get rid of MemAddrDeref later *)

let getArgDest paramRegArray i =
    (* First 6 args go into paramRegArray. 7th arg goes 8 above RSP,
       8th arg goes 16 above RSP, etc *)
    if i < Array.length paramRegArray then Reg paramRegArray.(i) else
    (* This will the correct offset AFTER WE DECREASE RSP *)
    (* We put all the args above RSP *)
    MemAddr(RSP, bytesForArg * (i - Array.length paramRegArray))
      (* this puts the 7th arg at 0(rsp) BEFORE the call. Which should be 8(rsp) after *)

let getPushPopsAndRSPshiftForFunCall fName allocdRegs spaceForArgs finalOffset
  numPushesAtTop =
    let (pushRegsInstrs, popRegsInstrs) =
          (List.map (fun r -> PUSH r) allocdRegs,
           List.map (fun r -> POP r) (List.rev allocdRegs)) in
    (* Now make sure we align RSP to 16 bytes. Note that we know that RSP
       is 8-byte-but-not-16-bye aligned at the moment. *)
    (* mod 16 = 0 because we always push RBP which takes 8 bytes, and every
    other time we push registers before a function call, we push twice:
    once at the top of this function, and once before the call. Because we're a
    little silly. *)
    let priorRSPShift = finalOffset + spaceForArgs +
            (bytesForArg * numPushesAtTop) +
            (bytesForArg * (List.length pushRegsInstrs)) in
    let () = assert(priorRSPShift mod 8 = 0) in
    (* Note that RSP is 8-byte-but-not-16-bye aligned at the moment. *)
    let is16ByteAligned =  priorRSPShift mod 16 = 8 in
    let rspShiftAmount = (if is16ByteAligned then spaceForArgs
                          else spaceForArgs + 8) in
    (pushRegsInstrs, popRegsInstrs, rspShiftAmount)

let translateFunCall tbl allocdRegs finalOffset paramRegArray funcToParamSizeMap
      opSize fName args dest regsPushedAtTop =
    (* 1. Figure out how many args we'll need to put on the stack.
       2. Figure out how much we'll need to decrease RSP by, both to make
       room for the stack args, and to make sure it's 16-byte aligned.
       3. Decrease RSP by the corresponding amount.
       4. Move args into expected regs/memAddr
       5. Call function
       6. Restore RSP
       7. Move result from EAX to wherever we want (if there is a result)
    *)
    let numPushesAtTop = List.length regsPushedAtTop in
    let numStackArgs = max 0 (List.length args - (Array.length paramRegArray)) in
                 (* numStackArgs can't be less than 0, even if the right term is *)
    let spaceForArgs = bytesForArg * numStackArgs in
    let (pushRegsInstrs, popRegsInstrs, rspShiftAmount) =
         getPushPopsAndRSPshiftForFunCall fName allocdRegs spaceForArgs finalOffset
             numPushesAtTop in
    let moveInstrs = List.mapi (fun i -> fun arg ->
        MOV((try (H.find funcToParamSizeMap fName).(i)
             with Not_found -> BIT64),
          translateTmpArg tbl arg, getArgDest paramRegArray i)) args in
    let rspSUBInstr = if rspShiftAmount = 0 && !OptimizeFlags.removeSubAddZeroToRsp
                      then []
                      else PTR_BINOP(PTR_SUB, Const rspShiftAmount, Reg RSP)::[] in
    let rspADDInstr = if rspShiftAmount = 0 && !OptimizeFlags.removeSubAddZeroToRsp
                      then []
                      else PTR_BINOP(PTR_ADD, Const rspShiftAmount, Reg RSP)::[] in
    (* See if we need to move EAX to a certain result tmp (we wouldn't have to
       for void function calls *)
    (* See which registers we need to save (only the ones we're using *)
    let resultInstr = (match dest with
                          None -> []
                        | Some t -> MOV(opSize, AssemLoc (Reg EAX),
                                       translateTmpLoc tbl t)::[]) in
    pushRegsInstrs @ rspSUBInstr @ moveInstrs @ [CALL fName]
    @ rspADDInstr @ popRegsInstrs @ resultInstr 

let translate tbl allocdRegs finalOffset paramRegArray
    funcToParamSizeMap regsPushedAtTop (instr : tmp2AddrInstr) : assemInstr list =
   match instr with
        Tmp2AddrMov(opSize, arg, dest) -> MOV(opSize, translateTmpArg tbl arg,
                                              translateTmpLoc tbl dest)::[]
      | Tmp2AddrBinop(binop, arg, dest) ->
              INT_BINOP(binop, translateTmpArg tbl arg, translateTmpLoc tbl dest)::[]
      | Tmp2AddrPtrBinop(binop, arg, dest) ->
              PTR_BINOP(binop, translateTmpArg tbl arg, translateTmpLoc tbl dest)::[]
      | Tmp2AddrReturn(opSize, arg) ->
        (* Need to pop in opposite order as pushed *)
        let rspADDInstr = if finalOffset = 0 && !OptimizeFlags.removeSubAddZeroToRsp
          then []
          else PTR_BINOP (PTR_ADD, Const finalOffset, Reg RSP)::[] in
        let popInstrs = List.map (fun r -> POP r) (List.rev regsPushedAtTop) in
                               MOV(opSize, translateTmpArg tbl arg, Reg EAX)::
                               rspADDInstr
                               @ (popInstrs)@ RETURN::[]
      | Tmp2AddrJump j -> JUMP j::[]
      | Tmp2AddrLabel jumpLabel -> LABEL jumpLabel::[]
      | Tmp2AddrBoolInstr TmpTest(arg, t) ->
              BOOL_INSTR (TEST (translateTmpArg tbl arg, translateTmpLoc tbl t))::[]
      | Tmp2AddrBoolInstr TmpCmp(opSize, arg, t) ->
              BOOL_INSTR (CMP (opSize, translateTmpArg tbl arg, translateTmpLoc tbl t))::[]
      | Tmp2AddrFunCall(opSize, fName, args, dest) ->
        translateFunCall tbl allocdRegs finalOffset paramRegArray funcToParamSizeMap
          opSize fName args dest regsPushedAtTop
      | Tmp2AddrMaskUpper t -> MASK_UPPER (translateTmpLoc tbl (TmpVar t))::[]
                                                 

(* We want to find the amount of memory allocated for this function's local vars:
   the largest distance below RBP on the stack *)
let maxDistBelowRBPOnStack t loc currMax =
    match loc with
      | MemAddr(RBP, offset) -> max (-offset) currMax
      | _ -> currMax

let rec getTempSet instrList tempSet =
    match instrList
        with [] -> tempSet
      | instr::instrs -> (match instr with
           (* Only count things that we write to: this matters so that we don't
              re-allocate for the params (which we never write to, since we move them
              all to new temps immediately *)
                   Tmp2AddrMov(opSize, src, TmpVar (Tmp dest)) ->
                       (let () = H.replace tempSet dest () in getTempSet instrs tempSet)
                 | Tmp2AddrBinop(op, src, TmpVar (Tmp dest)) ->
                       (let () = H.replace tempSet dest () in getTempSet instrs tempSet)
                 | Tmp2AddrPtrBinop(op, src, TmpVar (Tmp dest)) ->
                      (let () = H.replace tempSet dest () in getTempSet instrs tempSet)
                 | Tmp2AddrFunCall(retSize, fName, args, Some (TmpVar (Tmp dest))) ->
                                              (let () = H.replace tempSet dest () in
                                                             getTempSet instrs tempSet)
                 | Tmp2AddrMov(opSize, src, TmpDeref (Tmp dest)) ->
                       (let () = H.replace tempSet dest () in getTempSet instrs tempSet)
                 | Tmp2AddrBinop(op, src, TmpDeref (Tmp dest)) ->
                       (let () = H.replace tempSet dest () in getTempSet instrs tempSet)
                 | Tmp2AddrFunCall(retSize, fName, args, Some (TmpDeref (Tmp dest))) ->
                                              (let () = H.replace tempSet dest () in
                                                             getTempSet instrs tempSet)
                 | _ -> getTempSet instrs tempSet)

let getTempList instrList =
  let tempSet = getTempSet instrList (H.create (List.length instrList)) in
  let addTempToList t () acc = t::acc in
  H.fold addTempToList tempSet []
                              
let getColoring instrs tempList paramTmps =
  if !OptimizeFlags.doRegAlloc then
     let interferenceGraph =
          LivenessAnalysis.analyzeLiveness instrs tempList paramTmps in
     let tieBreakFunc = TieBreak.getTieBreakFunc instrs in
     let vertexOrdering = maxCardSearch interferenceGraph tieBreakFunc in
     (* let () = print_string("vertices: " ^ (String.concat ", " *)
     (*                        (List.map string_of_int vertexOrdering))) in *)
     let tmpToColorMap = greedilyColor interferenceGraph vertexOrdering in
     tmpToColorMap
  else
     let tmpToColorMap = H.create (List.length tempList) in
     let () = List.iter (fun t -> H.add tmpToColorMap t t) (tempList @ paramTmps) in
     tmpToColorMap

let progHasDivs instrs = List.exists (function Tmp2AddrBinop(FAKEDIV, _, _) -> true
                                        | Tmp2AddrBinop(FAKEMOD, _, _) -> true
                                        | _ -> false) instrs

let progHasShifts instrs = List.exists (function Tmp2AddrBinop(LSHIFT, _, _) -> true
                                        | Tmp2AddrBinop(RSHIFT, _, _) -> true
                                        | _ -> false) instrs

let getAllocableRegList instrs =
    let base = [EBX; R10; R11; R12; R13] in
    let baseWithIdx = List.mapi (fun i -> fun r -> (i,r)) base in
    let upToMax = List.filter (fun (i,r) -> i < !OptimizeFlags.numNonParamRegsToAlloc)
          baseWithIdx in
    let result = List.map (fun (i,r) -> r) upToMax in
    result

let rec getUsedRegsList regSet tmpToAssemLocMap tmps =
    match tmps with
        [] -> (* for now, we never actually use ECX and EDX so we don't
                 have to push/pop them (which is what this affects) *)
              let () = H.remove regSet ECX in
              let () = H.remove regSet EDX in
              let addRegToList r () acc = r::acc in H.fold addRegToList regSet []
      | t :: rest ->
           (try (match H.find tmpToAssemLocMap (Tmp t) with
                 | MemAddr _ -> getUsedRegsList regSet tmpToAssemLocMap rest
                 | MemAddrDeref _  -> getUsedRegsList regSet tmpToAssemLocMap rest
                 | Reg r -> let () = H.replace regSet r () in
                           getUsedRegsList regSet tmpToAssemLocMap rest
                 | RegDeref r -> let () = H.replace regSet r () in
                           getUsedRegsList regSet tmpToAssemLocMap rest)
          with Not_found -> assert(false))

let updateAssemLocsWithActualArgRBPOffset tmpToAssemLocMap firstArgOffsetAboveRbp =
    let newMap = H.create (H.length tmpToAssemLocMap) in
    let () = H.iter (function t ->
         function Reg r -> H.replace newMap t (Reg r)
                | RegDeref r -> H.replace newMap t (RegDeref r)
                | MemAddr(RBP, offset) ->
                  if offset < 0 (* local var, below RBP. ignore *)
                  then H.replace newMap t (MemAddr (RBP, offset))
                  else (* arg above RBP: add the start offset that we calculated.
                          Note: when the offset is 0, we add it, because the first
                          local var below RBP is at -8(rbp) *)
                  H.replace newMap t (MemAddr (RBP, offset + firstArgOffsetAboveRbp))
                | MemAddrDeref(RBP, offset) ->
                  if offset < 0 (* local var, below RBP. ignore *)
                  then H.replace newMap t (MemAddrDeref (RBP, offset))
                  else (* should not get here: no args are derefs *)
                    assert(false)
                | MemAddr (r, offset) -> (* ignore *)
                       H.replace newMap t (MemAddr (r, offset))
                | MemAddrDeref (r, offset) -> H.replace newMap t
                                             (MemAddrDeref(r, offset)))
               tmpToAssemLocMap
    in newMap

let rec padParamsto6Tmps paramTmps =
    if List.length paramTmps >= 6 then paramTmps
    else padParamsto6Tmps (paramTmps @ [Temp.create()])

let allocForFun (Tmp2AddrFunDef(fName, params, instrs) : tmp2AddrFunDef)
  funcToParamSizeMap : assemFunDef =
  let paramRegList = [EDI; ESI; EDX; ECX; R8; R9] in
  (* DO NOT ALLOCATE THE SPILLAGE REGISTER HERE!!! OR REGISTERS USED FOR WONKY *)
  let paramRegArray = Array.of_list paramRegList in
  let progTmps = getTempList instrs in
  let paramTmps = List.map (fun (Tmp t, pSize) -> t) params in
  let paddedParamTmps = padParamsto6Tmps paramTmps in
  let tmpToColorMap = getColoring instrs progTmps paddedParamTmps in
  let generalPurposeRegs = getAllocableRegList instrs in
  (* here's the deal: we need to know how many registers we're pushing, in order
     to know what the start offset above rbp should be for the args. But, we
     don't know how many registers we're using until we make the assemLoc
     map. So we're going to first do the assemLoc map with the first offset
     being 0, and then go back and add the actual offset afterwards *)
  let paramTmps = List.map (fun (Tmp t, pSize) -> t) params in
  let firstArgOffsetAboveRbpFAKE = 0 in
  let (colorToAssemLocMap, leftoverParamRegs) =
      makeColorToAssemLocMapFromParams tmpToColorMap paddedParamTmps paramRegList
        (H.create 1000) firstArgOffsetAboveRbpFAKE in
  let () = assert (List.length leftoverParamRegs = 0) in
  (* let usableParamRegs = List.filter (fun r -> (not (r = EDX) && not (r = ECX))) *)
  (*     leftoverParamRegs in *)
  (* we make the 3rd and 4th params interfere with everything in livenessAnalysis.ml,
     but what about functions that take fewer than 3 params? then we have to block
     the use of EDX/ECX in this way *)
  let allocableRegList = generalPurposeRegs @ leftoverParamRegs in
  let firstOffsetBelowRbp = bytesForArg in
  let () = addColorToAssemLocMappingsForProgTmps tmpToColorMap progTmps
       allocableRegList colorToAssemLocMap firstOffsetBelowRbp in
  (* paramTmps or paddedParamTmps? *)
  let tmpToAssemLocMap = makeTmpToAssemLocMap tmpToColorMap (progTmps @ paramTmps)
       colorToAssemLocMap (H.create 1000) in
  let allocdRegs = getUsedRegsList (H.create 10) tmpToAssemLocMap
         (paramTmps @ progTmps) in
  let regsToPushAtTop = (if fName = "_c0_main" || not !OptimizeFlags.onlyPushRegsOnce
                         then RBP :: allocdRegs
                         else RBP::[]) in
  let pushInstrs = List.map (fun r -> PUSH r) regsToPushAtTop in
  let firstArgOffsetAboveRbp = bytesForArg * ((List.length pushInstrs) + 1)
                                    (* +1 for return address *) in
  let finalTmpToAssemLocMap = updateAssemLocsWithActualArgRBPOffset
          tmpToAssemLocMap firstArgOffsetAboveRbp in
  (* let () = print_string("num push: " ^ string_of_int numPushInstrs ^ ", num allocd: " *)
  (*                       ^ string_of_int (List.length allocdRegs)) in *)
  let finalOffset = H.fold maxDistBelowRBPOnStack finalTmpToAssemLocMap 0 in
  (* Move RSP into RBP BEFORE we change RSP! We need to use RBP to refer to the
     args on the stack above it *)
  let rspSUBInstr = if finalOffset = 0 && !OptimizeFlags.removeSubAddZeroToRsp
                    then []
                    else PTR_BINOP (PTR_SUB, Const finalOffset, Reg RSP)::[] in
  let finalInstrs = pushInstrs @
  MOV(BIT64, AssemLoc (Reg RSP), Reg RBP)::[] @ rspSUBInstr @
  (List.concat (List.map (translate finalTmpToAssemLocMap allocdRegs finalOffset
                            paramRegArray funcToParamSizeMap regsToPushAtTop
                         ) instrs)) in
  AssemFunDef(fName, finalInstrs)

(* WE'RE USING EAX ALSO FOR MASKING OUT UPPER BITS. *)
