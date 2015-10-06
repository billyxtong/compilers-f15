
open Datatypesv1

let handleOneInstr (instr: assemInstr) : assemInstr list =
    match instr with
        MOV(AssemLoc(MemAddr memSrc), MemAddr memDest) ->
             MOV(AssemLoc (MemAddr memSrc), RegAlloc.spillReg)::
             MOV(AssemLoc RegAlloc.spillReg, MemAddr memDest)::[]
      | INT_BINOP(op, AssemLoc(MemAddr memSrc), MemAddr memDest) ->
             MOV(AssemLoc (MemAddr memDest), RegAlloc.spillReg)::
             (* We always put the register as the destination here,
                because suppose the op was MUL and we put the
                register as the first operand. Then when we went to
                do MUL, we would have to switch them again,
                because imul can't have MemAddr as 2nd operand.
                But we can't switch them because we already
                used spillReg. So that's why. *)
             INT_BINOP(op, AssemLoc (MemAddr memSrc), RegAlloc.spillReg)::
             MOV(AssemLoc RegAlloc.spillReg, MemAddr memDest)::[]
      | otherInstr -> [otherInstr]

let rec removeMemMemInstrs (prog: assemProg) : assemProg =
    match prog with
       [] -> []
     | instr::instrs -> handleOneInstr instr
                        @ removeMemMemInstrs instrs
