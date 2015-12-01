open Datatypesv1

(* Currently just never use EAX and EDX for anything else. *)
(* Also now never use ECX for anything other than shift src *)    
let instrToWonky = function
  (* The modulo is left in EDX, the quotient is in EAX *)
    INT_BINOP(FAKEDIV, divisor, dest) ->
        AssemInstr(MOV(BIT32, AssemLoc dest, Reg EAX))::CDQ::IDIV divisor
        ::AssemInstr(MOV(BIT32, AssemLoc (Reg EAX), dest))::[]
  | INT_BINOP(FAKEMOD, divisor, dest) ->
        AssemInstr(MOV(BIT32, AssemLoc dest, Reg EAX))::CDQ::IDIV divisor
        ::AssemInstr(MOV(BIT32, AssemLoc (Reg EDX), dest))::[]
  | INT_BINOP(MUL, src, MemAddr memDest) ->
        AssemInstr(MOV (BIT32, AssemLoc (MemAddr memDest), AllocForFun.firstSpillReg))
        ::AssemInstr(INT_BINOP(MUL, src, AllocForFun.firstSpillReg))
        ::AssemInstr(MOV(BIT32, AssemLoc AllocForFun.firstSpillReg,
                         MemAddr memDest))::[]
  | INT_BINOP(LSHIFT, src, dest) ->
        AssemInstr(MOV(BIT32, src, Reg ECX))
        ::AssemInstr(INT_BINOP (LSHIFT, AssemLoc(Reg ECX), dest))
        ::[]
  | INT_BINOP(RSHIFT, src, dest) ->
        AssemInstr(MOV(BIT32, src, Reg ECX))
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
