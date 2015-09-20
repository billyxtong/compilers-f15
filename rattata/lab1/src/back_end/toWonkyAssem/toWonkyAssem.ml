open Datatypesv1

(* Currently just never use EAX and EDX for anything else. *)
let instrToWonky = function
  (* The modulo is left in EDX, the quotient is in EAX *)
    BINOP(FAKEDIV, divisor, dest) ->
        AssemInstr(MOV(AssemLoc dest, Reg EAX))::CDQ::IDIV divisor
        ::AssemInstr(MOV(AssemLoc (Reg EAX), dest))::[]
  | BINOP(FAKEMOD, divisor, dest) ->
        AssemInstr(MOV(AssemLoc dest, Reg EAX))::CDQ::IDIV divisor
        ::AssemInstr(MOV(AssemLoc (Reg EDX), dest))::[]
  | instr -> [AssemInstr instr]

let rec toWonky2Addr = function
    [] -> []
  | instr::instrs -> instrToWonky instr @ toWonky2Addr instrs
