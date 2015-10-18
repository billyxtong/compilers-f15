module H = Hashtbl
open Datatypesv1
open PrintDatatypes
module A = Array
open Graph  

let spillReg = Reg EDI

let bytesForArg = 8

let calleeSavedList = [RBP; EBX; R12; R13; R14; R15]
let callerSavedList = [ECX; EDI; ESI; R8; R9; R10; R11]

let listToString i a = String.concat "" (List.map
            (fun x -> string_of_int(i)^": " ^string_of_int(x) ^ ", ") a @["\n"])

let assemLocForColor regArray offsetIncr colorNum =
    if (colorNum < A.length regArray) then Reg(A.get regArray colorNum)
    (* If there are 8 registers but this color is 10 (indexed from 0),
       that means we need there are at least 11 colors, which means we need
       at least 3 * (size of one tmp) bytes of stack memory. Since the
       first stack spot is 4(rsp), we add one *)
    else MemAddr(RBP, (colorNum - (A.length regArray) + 1) * offsetIncr)

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
  | TmpLoc t -> (try AssemLoc (H.find tbl t)
                 (* The "with" means there's a tmp that is used, but we
                    never assigned an AssemLoc to it because we never
                    wrote to it. This can happen if the variable was never initialized,
                    but it was ok because the line was unreachable *)
                 with Not_found -> AssemLoc (MemAddr(RSP, -666)))

let getArgDest paramRegArray i =
    (* First 6 args go into paramRegArray. 7th arg goes 8 above RSP,
       8th arg goes 16 above RSP, etc *)
    if i < Array.length paramRegArray then Reg paramRegArray.(i) else
    (* This will the correct offset AFTER WE DECREASE RSP *)
    MemAddr(RBP, bytesForArg * (i - Array.length paramRegArray + 1))

let translateFunCall tbl allocdRegs finalOffset paramRegArray fName args dest =
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
    (* mod 16 = 0 because we always push RBP which takes 8 bytes *)
    (* CHANGEEEEEEEEEEEEEEEEEEEEEEEEEEEEE *)
    let rspShiftAmount = (if is16ByteAligned then spaceForArgs
                          else spaceForArgs + 8) in
            (* Adding 8 makes it 16-byte-aligned if it isn't *)
    let moveInstrs = List.mapi (fun i -> fun arg ->
        MOV(translateTmpArg tbl arg, getArgDest paramRegArray i)) args in
    (* See if we need to move EAX to a certain result tmp (we wouldn't have to
       for void function calls *)
    (* See which registers we need to save (only the ones we're using *)
    let pushRegsInstrs = List.map (fun r -> PUSH r) allocdRegs in
    let popRegsInstrs = List.map (fun r -> POP r) (List.rev allocdRegs) in
    let resultInstr = (match dest with
                          None -> []
                        | Some t -> MOV(AssemLoc (Reg EAX), H.find tbl t)::[]) in
    (* Now make sure we align RSP to 16 bytes. Note that we know that RSP
       is 8-byte-but-not-16-bye aligned at the moment. *)
    pushRegsInstrs @
    SUBQ(Const rspShiftAmount, Reg RSP):: moveInstrs @ [CALL fName]
    @ [ADDQ(Const rspShiftAmount, Reg RSP)] @ popRegsInstrs @ resultInstr 

let translate tbl allocdRegs finalOffset paramRegArray (instr : tmp2AddrInstr) : assemInstr list =
   match instr with
        Tmp2AddrMov(arg, dest) -> MOV(translateTmpArg tbl arg, H.find tbl dest)::[]
      | Tmp2AddrBinop(binop, arg, dest) ->
              INT_BINOP(binop, translateTmpArg tbl arg, H.find tbl dest)::[]
      | Tmp2AddrReturn(arg) ->
        (* Need to pop in opposite order as pushed *)
        let popInstrs = List.map (fun r -> POP r) (List.rev allocdRegs) @ [POP RBP] in
                               MOV(translateTmpArg tbl arg, Reg EAX)::
                               ADDQ(Const finalOffset, Reg RSP)::[]
                               @ (popInstrs)@ RETURN::[]
      | Tmp2AddrJump j -> JUMP j::[]
      | Tmp2AddrLabel jumpLabel -> LABEL jumpLabel::[]
      | Tmp2AddrBoolInstr TmpTest(arg, t) ->
              BOOL_INSTR (TEST (translateTmpArg tbl arg, H.find tbl t))::[]
      | Tmp2AddrBoolInstr TmpCmp(arg, t) ->
              BOOL_INSTR (CMP (translateTmpArg tbl arg, H.find tbl t))::[]
      | Tmp2AddrFunCall(fName, args, dest) ->
        translateFunCall tbl allocdRegs finalOffset paramRegArray fName args dest
                                                 

let combineForMaxOffset t loc currMax =
    match loc with
        Reg _ -> currMax
      | MemAddr(_, offset) -> max offset currMax

let rec getTempSet instrList tempSet =
    match instrList
        with [] -> tempSet
      | instr::instrs -> (match instr with
           (* Only count things that we write to: this matters so that we don't
              re-allocate for the params (which we never write to, since we move them
              all to new temps immediately *)
                   Tmp2AddrMov(src, Tmp dest) -> (let () = H.replace tempSet dest () in
                                                             getTempSet instrs tempSet)
                 | Tmp2AddrBinop(op, src, Tmp dest) -> (let () = H.replace tempSet dest () in
                                                             getTempSet instrs tempSet)
                 | Tmp2AddrFunCall(fName, args, Some (Tmp dest)) ->
                                              (let () = H.replace tempSet dest () in
                                                             getTempSet instrs tempSet)
                 | _ -> getTempSet instrs tempSet)

let getTempList instrList = let tempSet = getTempSet instrList
                                (H.create (List.length instrList)) in
                            let addTempToList t () acc = t::acc in
                            H.fold addTempToList tempSet []
let mappingForParam paramRegArray i =
    (* We know that the first 6 params will be mapped to registers, and the
       rest will be in offsets of 8 bytes above RBP (what was RSP on
       entrance to the function) *)
    if i < Array.length paramRegArray then Reg paramRegArray.(i) else
    MemAddr(RBP, bytesForArg * (i - (Array.length paramRegArray) + 1))

let addParamMappings tmpToAssemLocMap paramRegArray params =
    List.iteri (fun i -> fun p ->
        H.add tmpToAssemLocMap p (mappingForParam paramRegArray i)) params

let combineForMaxColor t color currMax = max color currMax

let getUsedRegs maxColor allocableRegList =
    let paramListIndices = List.mapi (fun i p -> (i,p)) allocableRegList in
    (* The order of regs in allocableRegList corrrespond to the colors *)
    let filtered = List.filter (fun (i,p) -> i <= maxColor) paramListIndices in
    (* have to convert it back to list without indices *)
    List.map (fun (i,p) -> p) filtered

let allocForFun (Tmp2AddrFunDef(fName, params, instrs) : tmp2AddrFunDef) : assemFunDef =
  let paramRegArray = Array.of_list [EDI; ESI; EDX; ECX; R8; R9] in
  let allocableRegList = [EBX; R10; R11; R12; R13; R14; R15] in
  (* DO NOT ALLOCATE THE SPILLAGE REGISTER HERE!!! OR REGISTERS USED FOR WONKY *)
  let regArray = Array.of_list allocableRegList in
  let tempList = getTempList instrs in
  let interferenceGraph = LivenessAnalysis.analyzeLiveness instrs tempList in
  let startVertex = 0 in (* where cardSearch starts from; arbitrary for now *)
  let vertexOrdering = maxCardSearch interferenceGraph startVertex in
  let tmpToColorMap = greedilyColor interferenceGraph vertexOrdering in
  let offsetIncr = 4 in
  (* we need a list of tmps to go through; just use vertexOrdering *)
  let tmpToAssemLocMap = makeTmpToAssemLocMap tmpToColorMap vertexOrdering
      offsetIncr regArray (H.create 100) in
  let () = addParamMappings tmpToAssemLocMap paramRegArray params in
  let finalOffset = H.fold combineForMaxOffset tmpToAssemLocMap 0 in
  let maxColor = H.fold combineForMaxColor tmpToColorMap (-1) in
  (* -1 because if no colors are used, maxColor should not be 0 (that means one is used) *)
  let allocdRegs = getUsedRegs maxColor allocableRegList in
  let pushInstrs = PUSH RBP :: List.map (fun r -> PUSH r) allocdRegs in  
  (* Move RSP into RBP AFTER we change RSP! since we're doing positive offsets from RBP *)
  let finalInstrs = pushInstrs @
  SUBQ(Const finalOffset, Reg RSP)::MOVQ(AssemLoc (Reg RSP), Reg RBP) :: [] @
  (List.concat (List.map (translate tmpToAssemLocMap allocdRegs finalOffset paramRegArray) instrs)) in
  AssemFunDef(fName, finalInstrs)
