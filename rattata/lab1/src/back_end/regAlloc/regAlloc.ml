open Hashtbl
open Datatypesv1
open PrintDatatypes


let rec putInHashTable (instrList : tmp2AddrProg) (tbl : (tmp, assemLoc) Hashtbl.t) 
                       (regList : reg list) (offset : int ref) =
  match instrList with
        [] -> ()
      | instr :: prog ->
        (match regList with
                                [] -> (match instr with
                                             Tmp2AddrMov(arg,dest) -> 
                                               (try (let _ = find tbl dest in
                                                     putInHashTable prog tbl [] offset)
                                                with
                                                    Not_found -> (let () = (add tbl dest (MemAddr(RSP, !offset))) in
                                                                  let () = (offset := !offset + 4) in
                                                                  putInHashTable prog tbl [] offset))
                                           | Tmp2AddrBinop(binop,arg,dest) -> 
                                               (try (let _ = find tbl dest in
                                                    putInHashTable prog tbl [] offset)
                                                with
                                                    Not_found -> (let () = (add tbl dest (MemAddr(RSP, !offset))) in
                                                                  let () = (offset := !offset + 4) in
                                                                  putInHashTable prog tbl [] offset))
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

let fixOffsets tbl i x y =
  match y with
        MemAddr(RSP, offset) -> replace tbl x (MemAddr(RSP, offset - i))
      | _ -> ()

let translate tbl (instr : tmp2AddrInstr) =
  match instr with
        Tmp2AddrMov(arg, dest) -> (match arg with
                                         TmpLoc(t) -> MOV(AssemLoc(find tbl t), find tbl dest)
                                       | TmpConst(c) -> MOV(Const(c), find tbl dest))
      | Tmp2AddrBinop(TmpBinop(binop), arg, dest) -> (match arg with
                                                            TmpLoc(t) -> BINOP(binop, AssemLoc(find tbl t), find tbl dest)
                                                          | TmpConst(c) -> BINOP(binop, Const(c), find tbl dest))
      | Tmp2AddrReturn(arg) -> RETURN

let regAlloc (instrList : tmp2AddrProg) =
  let regList = [EBX; ECX; ESI; EDI; R8; R9; R10; R11; R12; R13; R14; R15] in
  let offset = ref 0 in
  let tmpToAssemLocTable = create 100 in
  let () = putInHashTable instrList tmpToAssemLocTable regList offset in
  let () = iter (fixOffsets tmpToAssemLocTable !offset) tmpToAssemLocTable in
  List.map (translate tmpToAssemLocTable) instrList
