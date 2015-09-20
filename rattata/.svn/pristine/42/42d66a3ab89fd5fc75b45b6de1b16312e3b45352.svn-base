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
        MemAddr(RSP, offset) -> replace tbl x (MemAddr(RSP, i - offset))
      | _ -> ()

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

let spillReg = EDI

let regAlloc (instrList : tmp2AddrProg) =
  let regList = [EBX; ECX; ESI] in
  let offset = ref 0 in
  let tmpToAssemLocTable = create 100 in
  let () = putInHashTable instrList tmpToAssemLocTable regList offset in
  let () = iter (fixOffsets tmpToAssemLocTable !offset) tmpToAssemLocTable in
  List.concat [[BINOP(SUBQ,Const(!offset),Reg(RSP))]; (List.map (translate tmpToAssemLocTable) instrList); [RETURN]]


