
open Datatypesv1

let spillReg = Reg EAX

let handleOneInstr (instr: assemInstr) : assemInstr list =
    match instr with
        MOV(AssemLoc(MemAddr memSrc), MemAddr memDest)) ->
             MOV(memSrc, spillReg)::
             MOV(AssemLoc spillReg, memDest)::[]
e      | BINOP(op, AssemLoc(MemAddr memSrc), MemAddr memDest) ->
             MOV(memSrc, spillReg)::
             BINOP(op, AssemLoc spillReg, memDest]::[]
      | otherInstr -> [otherInstr]

let rec removeMemMemInstrs (prog: assemProgWonky) : assemProgWonky =
    match prog with
       [] -> []
     | instr::instrs -> handleOneInstr instr
                        @ removeMemMemInstrs instrs
