open Hashtbl
open Datatypesv1
open PrintDatatypes
(* open LivenessAnalysis *)

let spillReg = Reg EDI

let rec putInHashTable (instrList : tmp2AddrProg) (tbl : (tmp, assemLoc) Hashtbl.t) 
                       (regList : reg list) (offset : int) =
  match instrList with
        [] -> offset
      | Tmp2AddrJump _::prog -> putInHashTable prog tbl [] offset
      | Tmp2AddrLabel _::prog -> putInHashTable prog tbl [] offset                                   
      | instr :: prog ->
        (match regList with
                                [] -> (match instr with
                                             Tmp2AddrMov(arg,dest) -> 
                                               (try (let _ = find tbl dest in
                                                     putInHashTable prog tbl regList offset)
                                                with
                                                    Not_found -> (let () = (add tbl dest (MemAddr(RSP, offset))) in
                                                                  putInHashTable prog tbl regList (offset + 4)))
                                           | Tmp2AddrBinop(binop,arg,dest) -> 
                                               (try (let _ = find tbl dest in
                                                    putInHashTable prog tbl regList offset)
                                                with
                                                    Not_found -> (let () = (add tbl dest (MemAddr(RSP, offset))) in
                                                                  putInHashTable prog tbl regList (offset + 4)))
                                           | Tmp2AddrReturn(arg) -> putInHashTable prog tbl regList offset
                                           (* We only allocate when we write to a temp; test and cmp don't
                                              write so we can ignore them *)
                                           | Tmp2AddrBoolInstr _ -> putInHashTable prog tbl regList offset)
                              | r :: newRegList -> (match instr with
                                             Tmp2AddrMov(arg,dest) -> 
                                               (try (let _ = find tbl dest
                                                     in putInHashTable prog tbl newRegList offset)
                                                with
                                                    Not_found -> let () = add tbl dest (Reg r) in
                                                                 putInHashTable prog tbl newRegList offset)
                                           | Tmp2AddrBinop(binop,arg,dest) -> 
                                               (try (let _ = find tbl dest
                                                     in putInHashTable prog tbl newRegList offset) with
                                                    Not_found -> let () = add tbl dest (Reg(r)) in
                                                                 putInHashTable prog tbl newRegList offset)
                                           | Tmp2AddrReturn(arg) -> putInHashTable prog tbl regList offset
                                           (* We only allocate when we write to a temp; test and cmp don't
                                              write so we can ignore them *)
                                           | Tmp2AddrBoolInstr _ -> putInHashTable prog tbl regList offset)
        )
                                 
let translateTmpArg tbl = function
     TmpLoc t -> AssemLoc(find tbl t)
   | TmpConst c -> Const c
                     
let translate tbl finalOffset (instr : tmp2AddrInstr) : assemInstr list =
   match instr with
        Tmp2AddrMov(arg, dest) -> MOV(translateTmpArg tbl arg, find tbl dest)::[]
      | Tmp2AddrBinop(binop, arg, dest) ->
              INT_BINOP(binop, translateTmpArg tbl arg, find tbl dest)::[]
      | Tmp2AddrReturn(arg) -> MOV(translateTmpArg tbl arg, Reg EAX)::
                               ADDQ(Const finalOffset, Reg RSP)::RETURN::[]
      | Tmp2AddrJump j -> JUMP j::[]
      | Tmp2AddrLabel jumpLabel -> LABEL jumpLabel::[]
      | Tmp2AddrBoolInstr TmpTest(arg, t) ->
              BOOL_INSTR (TEST (translateTmpArg tbl arg, find tbl t))::[]
      | Tmp2AddrBoolInstr TmpCmp(arg, t) ->
              BOOL_INSTR (CMP (translateTmpArg tbl arg, find tbl t))::[]

let regAlloc (instrList : tmp2AddrProg) =
  (* let regList = [EBX; ECX; ESI; R8; R9; R10; R11; R12; R13; R14; R15] in *)
  let regList = [] in
  (* DO NOT ALLOCATE THE SPILLAGE REGISTER HERE!!! *)
  let tmpToAssemLocTable = create 100 in
  let finalOffset = putInHashTable instrList tmpToAssemLocTable regList (4) in
  List.concat [ (* [PUSH(EBX)]; [PUSH(RSP)]; [PUSH(ESI)]; [PUSH(EDI)]; 
                [PUSH(R12)]; [PUSH(R13)]; [PUSH(R14)]; [PUSH(R15)]; *)
                [SUBQ(Const finalOffset, Reg RSP)];
                (List.concat (List.map (translate tmpToAssemLocTable finalOffset) instrList));
                (* [POP(EBX)]; [POP(RSP)]; [POP(ESI)]; [POP(EDI)]; 
                [POP(R12)]; [POP(R13)]; [POP(R14)]; [POP(R15)]; *) ]
