open Hashtbl
open Datatypesv1
open PrintDatatypes
open LivenessAnalysis

let spillReg = Reg EDI

let rec putInHashTable (instrList : tmp2AddrProg) (tbl : (tmp, assemLoc) Hashtbl.t) 
                       (regList : reg list) (offset : int) =
  match instrList with
        [] -> ()
      | instr :: prog ->
        (match regList with
                                [] -> (match instr with
                                             Tmp2AddrMov(arg,dest) -> 
                                               (try (let _ = find tbl dest in
                                                     putInHashTable prog tbl [] offset)
                                                with
                                                    Not_found -> (let () = (add tbl dest (MemAddr(RSP, offset))) in
                                                                  putInHashTable prog tbl [] (offset - 4)))
                                           | Tmp2AddrBinop(binop,arg,dest) -> 
                                               (try (let _ = find tbl dest in
                                                    putInHashTable prog tbl [] offset)
                                                with
                                                    Not_found -> (let () = (add tbl dest (MemAddr(RSP, offset))) in
                                                                  putInHashTable prog tbl [] (offset - 4)))
                                           | Tmp2AddrReturn(arg) -> ())
                              | r :: newRegList -> (match instr with
                                             Tmp2AddrMov(arg,dest) -> 
                                               (try (let _ = find tbl dest
                                                     in putInHashTable prog tbl newRegList offset) with
                                                    Not_found -> let () = add tbl dest (Reg(r)) in
                                                                 putInHashTable prog tbl newRegList offset)
                                           | Tmp2AddrBinop(binop,arg,dest) -> 
                                               (try (let _ = find tbl dest
                                                     in putInHashTable prog tbl newRegList offset) with
                                                    Not_found -> let () = add tbl dest (Reg(r)) in
                                                                 putInHashTable prog tbl newRegList offset)
                                           | Tmp2AddrReturn(arg) -> ()))
(* *)
let translate tbl (instr : tmp2AddrInstr) =
  match instr with
        Tmp2AddrMov(arg, dest) -> (match arg with
                                         TmpLoc(t) -> MOV(AssemLoc(find tbl t), find tbl dest)
                                       | TmpConst(c) -> MOV(Const(c), find tbl dest))
      | Tmp2AddrBinop(TmpBinop(binop), arg, dest) -> (match arg with
                                                            TmpLoc(t) -> BINOP(binop, AssemLoc(find tbl t), find tbl dest)
                                                          | TmpConst(c) -> BINOP(binop, Const(c), find tbl dest))
      | Tmp2AddrReturn(arg) -> (match arg with
                                      TmpLoc(t) -> MOV(AssemLoc(find tbl t), Reg(EAX))
                                    | TmpConst(c) -> MOV(Const(c), Reg(EAX)))

let regAlloc (instrList : tmp2AddrProg) =
  let regList = [EBX; ECX; ESI; R8; R9; R10; R11; R12; R13; R14; R15] in
  (* DO NOT ALLOCATE THE SPILLAGE REGISTER HERE!!! *)
  let tmpToAssemLocTable = create 100 in
  let () = putInHashTable instrList tmpToAssemLocTable regList (-4) in
  List.concat [ (* [PUSH(EBX)]; [PUSH(RSP)]; [PUSH(ESI)]; [PUSH(EDI)]; 
                [PUSH(R12)]; [PUSH(R13)]; [PUSH(R14)]; [PUSH(R15)]; *) [(PUSH(RBP))];
                [MOVQ(AssemLoc(Reg(RSP)), Reg(RBP))]; 
                (List.map (translate tmpToAssemLocTable) instrList); 
                (* [POP(EBX)]; [POP(RSP)]; [POP(ESI)]; [POP(EDI)]; 
                [POP(R12)]; [POP(R13)]; [POP(R14)]; [POP(R15)]; *) [POP(RBP)];
                [RETURN] ]


