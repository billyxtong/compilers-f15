module H = Hashtbl
open Datatypesv1
open PrintDatatypes
module A = Array
open Graph  

let spillReg = Reg EDI

let pushPopList = [EBX; ESI; R8; R9; R10; R11; R12; R13; R14; R15; ECX; EDI]

let listToString i a = String.concat "" (List.map (fun x -> string_of_int(i)^": " ^string_of_int(x) ^ ", ") a @["\n"])

let pushInstrs = List.map (fun r -> PUSH r) pushPopList

let popInstrs = List.map (fun r -> POP r) (List.rev pushPopList)

let assemLocForColor regArray offsetIncr colorNum =
    if (colorNum < A.length regArray) then Reg(A.get regArray colorNum)
    (* If there are 8 registers but this color is 10 (indexed from 0),
       that means we need there are at least 11 colors, which means we need
       at least 3 * (size of one tmp) bytes of stack memory. Since the
       first stack spot is 4(rsp), we add one *)
    else MemAddr(RSP, (colorNum - (A.length regArray) + 1) * offsetIncr)

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
                     
let translate tbl finalOffset (instr : tmp2AddrInstr) : assemInstr list =
   match instr with
        Tmp2AddrMov(arg, dest) -> MOV(translateTmpArg tbl arg, H.find tbl dest)::[]
      | Tmp2AddrBinop(binop, arg, dest) ->
              INT_BINOP(binop, translateTmpArg tbl arg, H.find tbl dest)::[]
      | Tmp2AddrReturn(arg) -> MOV(translateTmpArg tbl arg, Reg EAX)::
                               ADDQ(Const finalOffset, Reg RSP)::[] @ (popInstrs)@ RETURN::[]
      | Tmp2AddrJump j -> JUMP j::[]
      | Tmp2AddrLabel jumpLabel -> LABEL jumpLabel::[]
      | Tmp2AddrBoolInstr TmpTest(arg, t) ->
              BOOL_INSTR (TEST (translateTmpArg tbl arg, H.find tbl t))::[]
      | Tmp2AddrBoolInstr TmpCmp(arg, t) ->
              BOOL_INSTR (CMP (translateTmpArg tbl arg, H.find tbl t))::[]

let combineForMaxOffset t loc currMax =
    match loc with
        Reg _ -> currMax
      | MemAddr(_, offset) -> max offset currMax

let rec getTempSet instrList tempSet =
    match instrList
        with [] -> tempSet
      | instr::instrs -> (match instr with
                              Tmp2AddrMov(src, Tmp dest) -> (let () = H.replace tempSet dest () in
                                                             getTempSet instrs tempSet)
                            | Tmp2AddrBinop(op, src, Tmp dest) -> (let () = H.replace tempSet dest () in
                                                             getTempSet instrs tempSet)
                            | _ -> getTempSet instrs tempSet)

let getTempList instrList = let tempSet = getTempSet instrList
                                (H.create (List.length instrList)) in
                            let addTempToList t () acc = t::acc in
                            H.fold addTempToList tempSet []


let regAlloc (instrList : tmp2AddrProg) =
  let regList = [EBX; ESI; R8; R9; R10; R11; R12; R13; R14; R15] in
  (* DO NOT ALLOCATE THE SPILLAGE REGISTER HERE!!! *)
  let regArray = Array.of_list regList in
  let tempList = getTempList instrList in
  let interferenceGraph = LivenessAnalysis.analyzeLiveness instrList tempList in
  let startVertex = 0 in (* where cardSearch starts from; arbitrary for now *)
  let vertexOrdering = maxCardSearch interferenceGraph startVertex in
  let tmpToColorMap = greedilyColor interferenceGraph vertexOrdering in
  let offsetIncr = 4 in
  (* we need a list of tmps to go through; just use vertexOrdering *)
  let tmpToAssemLocMap = makeTmpToAssemLocMap tmpToColorMap vertexOrdering
      offsetIncr regArray (H.create 100) in
  let finalOffset = H.fold combineForMaxOffset tmpToAssemLocMap 0 in
  (pushInstrs) @ [SUBQ(Const finalOffset, Reg RSP)] @
                (List.concat (List.map (translate tmpToAssemLocMap finalOffset) instrList));
                (* [POP(EBX)]; [POP(RSP)]; [POP(ESI)]; [POP(EDI)]; 
                [POP(R12)]; [POP(R13)]; [POP(R14)]; [POP(R15)]; *)
