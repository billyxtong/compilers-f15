
open Datatypesv1

let handleOneInstr (instr: assemInstr) : assemInstr list =
    match instr with
        MOV(AssemLoc(MemAddr memSrc), MemAddr memDest) ->
             MOV(AssemLoc (MemAddr memSrc), RegAlloc.spillReg)::
             MOV(AssemLoc RegAlloc.spillReg, MemAddr memDest)::[]
      | BINOP(op, AssemLoc(MemAddr memSrc), MemAddr memDest) ->
             MOV(AssemLoc (MemAddr memSrc), RegAlloc.spillReg)::
             BINOP(op, AssemLoc RegAlloc.spillReg, MemAddr memDest)::[]
      | otherInstr -> [otherInstr]

let rec removeMemMemInstrs (prog: assemProg) : assemProg =
    match prog with
       [] -> []
     | instr::instrs -> handleOneInstr instr
                        @ removeMemMemInstrs instrs
