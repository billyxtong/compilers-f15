open Datatypesv1

(* Currently just never use EAX and EDX for anything else. *)
(* Also now never use ECX for anything other than shift src *)    
let instrToWonky = function
  (* The modulo is left in EDX, the quotient is in EAX *)
    INT_BINOP(FAKEDIV, divisor, dest) ->
        AssemInstr(MOV(AssemLoc dest, Reg EAX))::CDQ::IDIV divisor
        ::AssemInstr(MOV(AssemLoc (Reg EAX), dest))::[]
  | INT_BINOP(FAKEMOD, divisor, dest) ->
        AssemInstr(MOV(AssemLoc dest, Reg EAX))::CDQ::IDIV divisor
        ::AssemInstr(MOV(AssemLoc (Reg EDX), dest))::[]
  | INT_BINOP(MUL, src, MemAddr memDest) ->
        AssemInstr(MOV (AssemLoc (MemAddr memDest), AllocForFun.spillReg))
        ::AssemInstr(INT_BINOP(MUL, src, AllocForFun.spillReg))
        ::AssemInstr(MOV(AssemLoc AllocForFun.spillReg, MemAddr memDest))::[]
  | INT_BINOP(LSHIFT, src, dest) ->
        AssemInstr(MOV(src, Reg ECX))
        ::AssemInstr(INT_BINOP (LSHIFT, AssemLoc(Reg ECX), dest))
        ::[]
  | INT_BINOP(RSHIFT, src, dest) ->
        AssemInstr(MOV(src, Reg ECX))
        ::AssemInstr(INT_BINOP (RSHIFT, AssemLoc(Reg ECX), dest))
        ::[]
  | instr -> [AssemInstr instr]

let rec funToWonkyAssem = function
    [] -> []
  | instr::instrs -> instrToWonky instr @ funToWonkyAssem instrs

let rec toWonkyAssem = function
    [] -> []
  | AssemFunDef(fName, instrs)::rest ->
       WonkyFunDef(fName, funToWonkyAssem instrs) :: toWonkyAssem rest
