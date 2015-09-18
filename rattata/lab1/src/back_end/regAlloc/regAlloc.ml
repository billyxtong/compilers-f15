open Core.Std.Hashtbl
open REF
open Datatypesv1

let leftHandTmp (instr : tmp2AddrInstr) =
  match instr with
        Tmp2AddrMov(arg, temp) -> temp
      | Tmp2AddrBinop(arg, temp, binop) -> temp
      | Tmp2AddrReturn(retVal) -> ()

let rec regAlloc (L : tmp2AddrProg) =
  (* step 1: find out how many temps/registers we have with the exception of eax and edx, used for idiv *)
  (* step 2: if more than 13, move RSP down by 4*(# spilled temps) bytes *)
  (* step 3: assign 13 temps to registers (or however many registers aren't used in wonky instructions) *)
  (* remember: no memory -> memory instructions *)

  (* step 1: start stepping through L. Each time a new temp is discovered, assign it to a register (up to 12) by
   * having a hash table of type (tmp, assemLoc). Once we run out of registers, start assigning memory addresses 
   * by moving RSP down by 4 bytes each time we need a new one *)

  let tbl = create 200 in (* hash table holding all our mappings of tmps to memory addresses/registers *)
  let regCount = ref 0 in (* counter for the number of registers we've already allocated to tmps *)
  List.iter (let f (instr : wonkyInstr) = match instr with Tmp2AddrInstr(t) -> (match t with
                                                                                            )
                                                         | _ -> ) 
  
