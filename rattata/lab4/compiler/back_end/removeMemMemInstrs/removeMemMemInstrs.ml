open Datatypesv1

(* Do a first pass where remove MemAddrDerefs, and then a second pass where we
   remove MemAddr -> MemAddr instrs *)

let Reg secondSpillRegNoConstr = AllocForFun.secondSpillReg
let Reg firstSpillRegNoConstr = AllocForFun.firstSpillReg

let handleMemMemInstr (instr: assemInstr) : assemInstr list =
    match instr with
        MOV(opSize, AssemLoc(MemAddr memSrc), MemAddr memDest) ->
        (if memSrc = memDest then [] else
             MOV(opSize, AssemLoc (MemAddr memSrc), AllocForFun.firstSpillReg)::
             MOV(opSize, AssemLoc AllocForFun.firstSpillReg, MemAddr memDest)::[])
      | BOOL_INSTR (TEST (AssemLoc(MemAddr memSrc), MemAddr memDest)) ->
            (* TEST always takes bools, which are 32-bit *)
             MOV(BIT32, AssemLoc (MemAddr memDest), AllocForFun.firstSpillReg)::
             BOOL_INSTR (TEST(AssemLoc (MemAddr memSrc), AllocForFun.firstSpillReg))::
             MOV(BIT32, AssemLoc AllocForFun.firstSpillReg, MemAddr memDest)::[]
      | BOOL_INSTR (CMP (opSize, AssemLoc(MemAddr memSrc), MemAddr memDest)) ->
             MOV(opSize, AssemLoc (MemAddr memDest), AllocForFun.firstSpillReg)::
             BOOL_INSTR (CMP (opSize, AssemLoc (MemAddr memSrc),
                              AllocForFun.firstSpillReg))::
             MOV(opSize, AssemLoc AllocForFun.firstSpillReg, MemAddr memDest)::[]
      | INT_BINOP(op, AssemLoc(MemAddr memSrc), MemAddr memDest) ->
             MOV(BIT32, AssemLoc (MemAddr memDest), AllocForFun.firstSpillReg)::
             (* We always put the register as the destination here,
                because suppose the op was MUL and we put the
                register as the first operand. Then when we went to
                do MUL, we would have to switch them again,
                because imul can't have MemAddr as 2nd operand.
                But we can't switch them because we already
                used firstSpillReg. So that's why. *)
             INT_BINOP(op, AssemLoc (MemAddr memSrc), AllocForFun.firstSpillReg)::
             MOV(BIT32, AssemLoc AllocForFun.firstSpillReg, MemAddr memDest)::[]
      | PTR_BINOP(op, AssemLoc(MemAddr memSrc), MemAddr memDest) ->
             MOV(BIT64, AssemLoc (MemAddr memDest), AllocForFun.firstSpillReg)::
             (* Not sure if the thing about putting the register as the
                destination matters for ptrs, because there's no ptr mul,
                but as well *)
             PTR_BINOP(op, AssemLoc (MemAddr memSrc), AllocForFun.firstSpillReg)::
             MOV(BIT64, AssemLoc AllocForFun.firstSpillReg, MemAddr memDest)::[]
      | otherInstr -> [otherInstr]

(* Everywhere where just the src is a MemAddrDeref, do:
   1. Move the memSrc to secondSpillReg.
   2. Move the Deref of the secondSpillReg to dest
   When just the dest is a MemAddrDeref, do:
   1. Move the memDest to secondSpillReg,
   2. Move the src into the Deref of secondSpillReg
   When both are MemAddrDerefs, do:
   (Note: we can use both spillRegs here, because we'll ensure that
   the resulting instrs have no MemAddrs, so they will be ignored during
   handleMemMemInstr).
   1. Move the memSrc into firstSpillReg.
   2. Move the Deref of the firstSpillReg BACK INTO firstSpillReg
   3. Move the memDest into secondSpillReg
   4. Move firstSpillReg into Deref of secondSpillReg
*)
(* Note: BIT64 not opSize because memDerefSrc/Dest is a pointer! *)
let handleMemAddrDerefInstr = function
     MOV(opSize, AssemLoc(MemAddrDeref memDerefSrc), MemAddrDeref memDerefDest) ->
        MOV(BIT64, AssemLoc (MemAddr memDerefSrc), AllocForFun.firstSpillReg)::
        MOV(opSize, AssemLoc (RegDeref firstSpillRegNoConstr),
            AllocForFun.firstSpillReg)::
        MOV(BIT64, AssemLoc (MemAddr memDerefDest), AllocForFun.secondSpillReg)::
        MOV(opSize, AssemLoc (AllocForFun.firstSpillReg),
            RegDeref secondSpillRegNoConstr)::[]
   | MOV(opSize, AssemLoc(MemAddrDeref memDerefSrc), dest) ->
        MOV(BIT64, AssemLoc (MemAddr memDerefSrc), AllocForFun.secondSpillReg)::
        MOV(opSize, AssemLoc (RegDeref secondSpillRegNoConstr), dest)::[]
   | MOV(opSize, src, MemAddrDeref memDerefDest) ->
        MOV(BIT64, AssemLoc (MemAddr memDerefDest), AllocForFun.secondSpillReg)::
        MOV(opSize, src, RegDeref secondSpillRegNoConstr)::[]
   | INT_BINOP(op, AssemLoc(MemAddrDeref memDerefSrc), dest) ->
        MOV(BIT64, AssemLoc (MemAddr memDerefSrc), AllocForFun.secondSpillReg)::
        INT_BINOP(op, AssemLoc (RegDeref secondSpillRegNoConstr), dest)::[]
   | INT_BINOP(op, src, MemAddrDeref memDerefDest) ->
        MOV(BIT64, AssemLoc (MemAddr memDerefDest), AllocForFun.secondSpillReg)::
        INT_BINOP(op, src, RegDeref secondSpillRegNoConstr)::[]
   | PTR_BINOP(op, AssemLoc(MemAddrDeref memDerefSrc), dest) ->
        MOV(BIT64, AssemLoc (MemAddr memDerefSrc), AllocForFun.secondSpillReg)::
        PTR_BINOP(op, AssemLoc (RegDeref secondSpillRegNoConstr), dest)::[]
   | PTR_BINOP(op, src, MemAddrDeref memDerefDest) ->
        MOV(BIT64, AssemLoc (MemAddr memDerefDest), AllocForFun.secondSpillReg)::
        PTR_BINOP(op, src, RegDeref secondSpillRegNoConstr)::[]
   | BOOL_INSTR (TEST (AssemLoc(MemAddrDeref memDerefSrc), dest)) ->
        MOV(BIT64, AssemLoc (MemAddr memDerefSrc), AllocForFun.secondSpillReg)::
        BOOL_INSTR (TEST (AssemLoc (RegDeref secondSpillRegNoConstr), dest))::[]
   | BOOL_INSTR (TEST (src, MemAddrDeref memDerefDest)) ->
        MOV(BIT64, AssemLoc (MemAddr memDerefDest), AllocForFun.secondSpillReg)::
        BOOL_INSTR (TEST (src, RegDeref secondSpillRegNoConstr))::[]
   | BOOL_INSTR (CMP (opSize, AssemLoc(MemAddrDeref memDerefSrc), dest)) ->
        MOV(BIT64, AssemLoc (MemAddr memDerefSrc), AllocForFun.secondSpillReg)::
        BOOL_INSTR (CMP (opSize,
                         AssemLoc (RegDeref secondSpillRegNoConstr), dest))::[]
   | BOOL_INSTR (CMP (opSize, src, MemAddrDeref memDerefDest)) ->
        MOV(BIT64, AssemLoc (MemAddr memDerefDest), AllocForFun.secondSpillReg)::
        BOOL_INSTR (CMP (opSize, src, RegDeref secondSpillRegNoConstr))::[]
   | otherInstr -> otherInstr :: []
                                 
let rec handleMemMemForFun instrs =
     match instrs with
         [] -> []
       | instr::rest -> handleMemMemInstr instr @ handleMemMemForFun rest

let rec handleMemAddrDerefForFun = function
     [] -> []
   | instr::rest -> handleMemAddrDerefInstr instr @ handleMemAddrDerefForFun rest

let rec removeMemMemInstrs (prog: assemProg) : assemProg =
    match prog with
       [] -> []
     | AssemFunDef(fName, instrs)::rest ->
           let noMemDerefs = handleMemAddrDerefForFun instrs in
           let noMemMem = handleMemMemForFun noMemDerefs in
           AssemFunDef(fName, noMemMem)::removeMemMemInstrs rest
