
open Datatypesv1

let handleOneInstr (instr: assemInstr) : assemInstr list =
    match instr with
        MOV(AssemLoc(MemAddr memSrc), MemAddr memDest) ->
        (if memSrc = memDest then [] else
        
             MOV(AssemLoc (MemAddr memSrc), AllocForFun.spillReg)::
             MOV(AssemLoc AllocForFun.spillReg, MemAddr memDest)::[])
      | BOOL_INSTR (TEST (AssemLoc(MemAddr memSrc), MemAddr memDest)) ->
             MOV(AssemLoc (MemAddr memDest), AllocForFun.spillReg)::
             BOOL_INSTR (TEST(AssemLoc (MemAddr memSrc), AllocForFun.spillReg))::
             MOV(AssemLoc AllocForFun.spillReg, MemAddr memDest)::[]
      | BOOL_INSTR (CMP (AssemLoc(MemAddr memSrc), MemAddr memDest)) ->
             MOV(AssemLoc (MemAddr memDest), AllocForFun.spillReg)::
             BOOL_INSTR (CMP (AssemLoc (MemAddr memSrc), AllocForFun.spillReg))::
             MOV(AssemLoc AllocForFun.spillReg, MemAddr memDest)::[]
      | INT_BINOP(op, AssemLoc(MemAddr memSrc), MemAddr memDest) ->
             MOV(AssemLoc (MemAddr memDest), AllocForFun.spillReg)::
             (* We always put the register as the destination here,
                because suppose the op was MUL and we put the
                register as the first operand. Then when we went to
                do MUL, we would have to switch them again,
                because imul can't have MemAddr as 2nd operand.
                But we can't switch them because we already
                used spillReg. So that's why. *)
             INT_BINOP(op, AssemLoc (MemAddr memSrc), AllocForFun.spillReg)::
             MOV(AssemLoc AllocForFun.spillReg, MemAddr memDest)::[]
      | otherInstr -> [otherInstr]

let rec removeFromFun instrs =
     match instrs with
         [] -> []
       | instr::rest -> handleOneInstr instr @ removeFromFun rest

let rec removeMemMemInstrs (prog: assemProg) : assemProg =
    match prog with
       [] -> []
     | AssemFunDef(fName, instrs)::rest ->
           AssemFunDef(fName, removeFromFun instrs)::removeMemMemInstrs rest
