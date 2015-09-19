open Core.Std
open REF
open Datatypesv1

(* adds a temp to the set of temps *)
let addTmpToSet (instr : tmp2AddrInstr) (S : "tmp set") =
  match instr with
        Tmp2AddrMov(arg, temp) -> "add temp to set"
      | Tmp2AddrBinop(binop, arg, temp) -> "add temp to set" 
      | Tmp2AddrReturn(retVal) -> (match retVal with
                                         TmpLoc(tmp) -> "add temp to set"
                                       | Const(c) -> "do nothing")

(* maps a temp to a register and puts the pair in our hashtable *)
let rec mapTmpToReg (L : reg list) (S : "tmp set") (H : tmp hashtbl) =
  match (L, S) with
      | (x::L',s::S') -> let (add H (Reg(x)) s) in mapTmpsToRegs L' S' H
      | ([], S') -> ()
      | (_, []) -> ()

(* maps a temp to a memory address and puts the pair in our hashtable *)
let rec mapTmpToMemAddr (S : "tmp set") (H : tmp hashtbl) (offset : int ref) =
  match S with
        [] -> ()
      | s::S' -> let (add H s (MemAddr(RSP, offset))) 
                 and offset := !offset + 4 
                 in mapTmpToMemAddr S' H offset

let rec regAlloc (instrList : tmp2AddrProg) =
  let tempSet = "initialize empty set of temps"
  (* list of 12 registers. Not sure if EBP is allowed to be used*)
  and regList = [EBX; ECX; ESI; EDI; R8; R9; R10; R11; R12; R13; R14; R15] in
  (* iterate through prog, add all temps to set of temps *)
  List.iter (addTmpToSet i tempSet) instrList
  Set.iter (mapTmpToReg regList tempS)  
