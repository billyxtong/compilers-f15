
open Datatypesv1

let spillReg = Reg RegAlloc.spillReg
let handleOneInstr (instr: assemInstr) : assemInstr list =
    match instr with
        MOV(AssemLoc(MemAddr memSrc), MemAddr memDest) ->
             MOV(AssemLoc (MemAddr memSrc), spillReg)::
             MOV(AssemLoc spillReg, MemAddr memDest)::[]
      | BINOP(op, AssemLoc(MemAddr memSrc), MemAddr memDest) -> []
             (* MOV(AssemLoc (MemAddr memSrc), spillReg):: *)
             (* BINOP(op, AssemLoc spillReg, MemAddr memDest]::[] *)
      | otherInstr -> [otherInstr]

let rec removeMemMemInstrs (prog: assemProg) : assemProg =
    match prog with
       [] -> []
     | instr::instrs -> handleOneInstr instr
                        @ removeMemMemInstrs instrs
