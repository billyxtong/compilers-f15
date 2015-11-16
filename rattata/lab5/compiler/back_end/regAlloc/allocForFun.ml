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

(* colorList consists of tuples (t, colorForTmp) where t is the temp number
   and color is the color of t. We need t in order to add it to the
   map properly *)
let rec makeTmpToAssemLocMap tmpToColorMap tmpList offsetIncr regArray resultMap =
    match tmpList with
        [] -> resultMap
      | t::tmps ->
           let color = H.find tmpToColorMap t in
           let currAssemLoc = assemLocForColor regArray offsetIncr color in
           let () = H.add resultMap (Tmp t) currAssemLoc in
           makeTmpToAssemLocMap tmpToColorMap tmps offsetIncr regArray resultMap

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

let translateFunCall tbl allocdRegs finalOffset paramRegArray funcToParamSizeMap
      opSize fName args dest =
    (* 1. Figure out how many args we'll need to put on the stack.
       2. Figure out how much we'll need to decrease RSP by, both to make
       room for the stack args, and to make sure it's 16-byte aligned.
       3. Decrease RSP by the corresponding amount.
       4. Move args into expected regs/memAddr
       5. Call function
       6. Restore RSP
       7. Move result from EAX to wherever we want (if there is a result)
    *)
    let numStackArgs = max 0 (List.length args - (Array.length paramRegArray)) in
                 (* numStackArgs can't be less than 0, even if the right term is *)
    let spaceForArgs = bytesForArg * numStackArgs in
    let is16ByteAligned = (finalOffset + spaceForArgs) mod 16 = 0 in
    (* mod 16 = 0 because we always push RBP which takes 8 bytes, and every
    other time we push registers before a function call, we push twice:
    once at the top of this function, and once before the call. Because we're a
    little silly. *)
    let rspShiftAmount = (if is16ByteAligned then spaceForArgs
                          else spaceForArgs + 8) in
            (* Adding 8 makes it 16-byte-aligned if it isn't *)
    let moveInstrs = List.mapi (fun i -> fun arg ->
        MOV((try (H.find funcToParamSizeMap fName).(i)
             with Not_found -> BIT64),
          translateTmpArg tbl arg, getArgDest paramRegArray i)) args in
    (* See if we need to move EAX to a certain result tmp (we wouldn't have to
       for void function calls *)
    (* See which registers we need to save (only the ones we're using *)
    let pushRegsInstrs = List.map (fun r -> PUSH r) allocdRegs in
    let popRegsInstrs = List.map (fun r -> POP r) (List.rev allocdRegs) in
    let resultInstr = (match dest with
                          None -> []
                        | Some t -> MOV(opSize, AssemLoc (Reg EAX),
                                       translateTmpLoc tbl t)::[]) in
    (* Now make sure we align RSP to 16 bytes. Note that we know that RSP
       is 8-byte-but-not-16-bye aligned at the moment. *)
    pushRegsInstrs @
    PTR_BINOP(PTR_SUB, Const rspShiftAmount, Reg RSP):: moveInstrs @ [CALL fName]
    @ [PTR_BINOP(PTR_ADD, Const rspShiftAmount, Reg RSP)] @ popRegsInstrs @ resultInstr 

let translate tbl allocdRegs finalOffset paramRegArray
    funcToParamSizeMap (instr : tmp2AddrInstr) : assemInstr list =
   match instr with
        Tmp2AddrMov(opSize, arg, dest) -> MOV(opSize, translateTmpArg tbl arg,
                                              translateTmpLoc tbl dest)::[]
      | Tmp2AddrBinop(binop, arg, dest) ->
              INT_BINOP(binop, translateTmpArg tbl arg, translateTmpLoc tbl dest)::[]
      | Tmp2AddrPtrBinop(binop, arg, dest) ->
              PTR_BINOP(binop, translateTmpArg tbl arg, translateTmpLoc tbl dest)::[]
      | Tmp2AddrReturn(opSize, arg) ->
        (* Need to pop in opposite order as pushed *)
        let popInstrs = List.map (fun r -> POP r) (List.rev allocdRegs) @ [POP RBP] in
                               MOV(opSize, translateTmpArg tbl arg, Reg EAX)::
                               PTR_BINOP(PTR_ADD, Const finalOffset, Reg RSP)::[]
                               @ (popInstrs)@ RETURN::[]
      | Tmp2AddrJump j -> JUMP j::[]
      | Tmp2AddrLabel jumpLabel -> LABEL jumpLabel::[]
      | Tmp2AddrBoolInstr TmpTest(arg, t) ->
              BOOL_INSTR (TEST (translateTmpArg tbl arg, translateTmpLoc tbl t))::[]
      | Tmp2AddrBoolInstr TmpCmp(opSize, arg, t) ->
              BOOL_INSTR (CMP (opSize, translateTmpArg tbl arg, translateTmpLoc tbl t))::[]
      | Tmp2AddrFunCall(opSize, fName, args, dest) ->
        translateFunCall tbl allocdRegs finalOffset paramRegArray funcToParamSizeMap
          opSize fName args dest
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

let getTempList instrList = let tempSet = getTempSet instrList
                                (H.create (List.length instrList)) in
                            let addTempToList t () acc = t::acc in
                            H.fold addTempToList tempSet []
let mappingForParam argOffsetAboveRbp paramRegArray i =
    (* We know that the first 6 params will be mapped to registers, and the
       rest will be in offsets of 8 bytes above what was RSP on
       entrance to the function. HOWEVER since the  *)
    if i < Array.length paramRegArray then Reg paramRegArray.(i) else
    let offsetFromArgStart = bytesForArg * (i - (Array.length paramRegArray)) in
           (* there's no +1  because if i == length, it should be 0 *)
    MemAddr(RBP, argOffsetAboveRbp + offsetFromArgStart)

let addParamMappings tmpToAssemLocMap argOffsetAboveRbp paramRegArray params =
    let paramTmps = List.map (fun (t, paramSize) -> t) params in
    List.iteri (fun i -> fun p ->
        H.add tmpToAssemLocMap p (mappingForParam argOffsetAboveRbp
                                    paramRegArray i)) paramTmps

let combineForMaxColor t color currMax = max color currMax

let getUsedRegs maxColor allocableRegList =
    let paramListIndices = List.mapi (fun i p -> (i,p)) allocableRegList in
    (* The order of regs in allocableRegList corrrespond to the colors *)
    let filtered = List.filter (fun (i,p) -> i <= maxColor) paramListIndices in
    (* have to convert it back to list without indices *)
    List.map (fun (i,p) -> p) filtered

let getColoring instrs tempList =
  if !OptimizeFlags.doRegAlloc then
     let interferenceGraph = LivenessAnalysis.analyzeLiveness instrs tempList in
     let tieBreakFunc = TieBreak.getTieBreakFunc instrs in
     let vertexOrdering = maxCardSearch interferenceGraph tieBreakFunc in
     let tmpToColorMap = greedilyColor interferenceGraph vertexOrdering in
     tmpToColorMap
  else
     let tmpToColorMap = H.create (List.length tempList) in
     let () = List.iter (fun t -> H.add tmpToColorMap t t) tempList in
     tmpToColorMap

let progHasDivs instrs = List.exists (function Tmp2AddrBinop(FAKEDIV, _, _) -> true
                                        | Tmp2AddrBinop(FAKEMOD, _, _) -> true
                                        | _ -> false) instrs

let progHasShifts instrs = List.exists (function Tmp2AddrBinop(LSHIFT, _, _) -> true
                                        | Tmp2AddrBinop(RSHIFT, _, _) -> true
                                        | _ -> false) instrs

let getAllocableRegList instrs =
    let base = [EBX; R10; R11; R12; R13] in
    if not !OptimizeFlags.checkNoShiftsDivs then base else
      let withDivs = (if progHasDivs instrs then base else EDX::base) in
      let withShifts = (if progHasShifts instrs then withDivs else ECX::withDivs) in
      withShifts

let allocForFun (Tmp2AddrFunDef(fName, params, instrs) : tmp2AddrFunDef)
  funcToParamSizeMap : assemFunDef =
  let paramRegArray = Array.of_list [EDI; ESI; EDX; ECX; R8; R9] in
  let allocableRegList = getAllocableRegList instrs in
  (* DO NOT ALLOCATE THE SPILLAGE REGISTER HERE!!! OR REGISTERS USED FOR WONKY *)
  let regArray = Array.of_list allocableRegList in
  let tempList = getTempList instrs in
  let tmpToColorMap = getColoring instrs tempList in
  let maxColor = H.fold combineForMaxColor tmpToColorMap (-1) in
  (* -1 because if no colors are used, maxColor should not be 0 (that means one is used) *)
  let allocdRegs = getUsedRegs maxColor allocableRegList in
  let pushInstrs = PUSH RBP :: List.map (fun r -> PUSH r) allocdRegs in  
  let offsetIncr = 8 in
  let tmpToAssemLocMap = makeTmpToAssemLocMap tmpToColorMap tempList
      offsetIncr regArray (H.create 100) in
  let argSize = 8 in
  let argOffsetAboveRbp = argSize * (List.length pushInstrs + 1)
                                    (* +1 for return address *) in
  let () = addParamMappings tmpToAssemLocMap argOffsetAboveRbp paramRegArray params in
  let finalOffset = H.fold maxDistBelowRBPOnStack tmpToAssemLocMap 0 in
  (* Move RSP into RBP BEFORE we change RSP! We need to use RBP to refer to the
     args on the stack above it *)
  let finalInstrs = pushInstrs @
  MOV(BIT64, AssemLoc (Reg RSP), Reg RBP)
  :: PTR_BINOP (PTR_SUB, Const finalOffset, Reg RSP):: [] @
  (List.concat (List.map (translate tmpToAssemLocMap allocdRegs finalOffset
                            paramRegArray funcToParamSizeMap) instrs)) in
  AssemFunDef(fName, finalInstrs)

(* WE'RE USING ECX ALSO FOR MASKING OUT UPPER BITS. *)
