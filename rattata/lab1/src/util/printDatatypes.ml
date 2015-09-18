open Datatypesv1
open String

let c0typeToString (c : c0type) =
  match c with
        INT -> "int"
      | PTR -> "pointer"

let constToString ((i, t) : const) = concat "" [string_of_int(i); " of type "; c0typeToString(t)]

let regToString (r : reg) = 
  match r with
        EAX -> "%eax"
      | EBX -> "%ebx"
      | ECX -> "%ecx"
      | EDX -> "%edx"
      | EBP -> "%ebp"
      | ESP -> "%esp"
      | ESI -> "%esi"
      | EDI -> "%edi"
      | R8  -> "%r8"
      | R9  -> "%r9"
      | R10 -> "%r10"
      | R11 -> "%r11"
      | R12 -> "%r12"
      | R13 -> "%r13"
      | R14 -> "%r14"
      | R15 -> "%r15"
      | EAX -> "%eax"
      | EDX -> "%edx"

let memAddrToString ((register, offset) : memAddr) = concat "" [string_of_int(offset); "("; regToString(register); ")"]

let assemLocToString(loc : assemLoc) =
  match loc with
        Reg(r) -> regToString(r)
      | MemAddr(register, offset) -> memAddrToString(register, offset)

let assemArgToString (arg : assemArg) = 
  match arg with
        AssemLoc(loc) -> assemLocToString(loc)
      | Const(value, _) -> concat "" ["$"; string_of_int(value)]

let assemBinopToString(binop : assemBinop) = 
  match binop with
        ADD(src, dest) -> concat "" ["addl "; assemArgToString(src); ", "; assemLocToString(dest)]
      | MUL(src, dest) -> concat "" ["mull "; assemArgToString(src); ", "; assemLocToString(dest)]

let assemInstrToString(instr : assemInstr) = 
  match instr with
        MOV(src, dest) ->
           concat "" ["movl "; assemArgToString(src); ", ";
                      assemLocToString(dest)]
      | BINOP(binop) -> assemBinopToString(binop)
      | RETURN -> "ret"
      | IDIV(divisor) ->
           concat "" ["idiv "; assemArgToString(divisor)]
      | CDQ -> "cdq"

let assemProgToString(assemprog : assemProg) = 
  concat "\n" (List.map assemInstrToString assemprog)

let tmpToString(t : tmp) = concat "" ["t"; string_of_int t]

let tmpAssemLocToString(tloc : tmpAssemLoc) = 
  match tloc with
        Tmp(t) -> tmpToString(t)
      | AssemLocForTmp(aloc) -> assemLocToString(aloc)

let tmpAssemArgToString(tmpArg : tmpAssemArg) = 
  match tmpArg with
        TmpAssemLoc(t) -> tmpAssemLocToString(t)
      | AssemArg(arg) -> (match arg with
                                AssemLoc(loc) -> assemLocToString(loc)
                              | Const(value, _) -> string_of_int(value))

let tmp2AddrBinopToString(tmp2binop : tmp2AddrBinop) =
  match tmp2binop with
        Tmp2AddrAdd(tmpsrc, tmpdest) ->
            concat "" [tmpAssemLocToString(tmpdest); " <-- "; 
            tmpAssemLocToString(tmpdest); " + ";
            tmpAssemArgToString(tmpsrc)]
      | Tmp2AddrSub(tmpsrc, tmpdest) ->
            concat "" [tmpAssemLocToString(tmpdest); " <-- ";
            tmpAssemLocToString(tmpdest); " - ";
            tmpAssemArgToString(tmpsrc)]
      | Tmp2AddrMul(tmpsrc, tmpdest) -> concat "" [tmpAssemLocToString(tmpdest); " <-- "; 
                                                    tmpAssemLocToString(tmpdest); " * "; tmpAssemArgToString(tmpsrc)]
      | Tmp2AddrDiv(tmpsrc, tmpdest) ->
            concat "" [tmpAssemLocToString(tmpdest); " <-- ";
            tmpAssemLocToString(tmpdest); " / ";
            tmpAssemArgToString(tmpsrc)]
      | Tmp2AddrMod(tmpsrc, tmpdest) ->
            concat "" [tmpAssemLocToString(tmpdest); " <-- ";
            tmpAssemLocToString(tmpdest); " % ";
            tmpAssemArgToString(tmpsrc)]

let tmp2AddrInstrToString(tmp2instr : tmp2AddrInstr) = 
  match tmp2instr with
        Tmp2AddrMov(tmpsrc, tmpdest) -> concat "" [tmpAssemLocToString(tmpdest); " <-- "; tmpAssemArgToString(tmpsrc)]
      | Tmp2AddrBinop(tmpbinop) -> tmp2AddrBinopToString(tmpbinop)
      | Tmp2AddrReturn(tmparg) -> concat "" ["return "; tmpAssemArgToString(tmparg)]

let tmp2AddrProgToString(tmp2addrprog : tmp2AddrProg) =
  (* We want a newline at the end *)
  concat "\n" (List.map tmp2AddrInstrToString tmp2addrprog) ^ "\n"

let tmpWonkyInstrToString(wonkyinstr: tmpWonkyInstr) =
   match wonkyinstr with
      Tmp2AddrInstr(instr) -> tmp2AddrInstrToString instr
    | TmpCDQ -> "cdq"
    | TmpIDIV(tmpArg) -> concat "" ["idiv "; tmpAssemArgToString tmpArg]

let tmpWonkyProgToString (wonkyprog: tmpWonkyProg) = 
  concat "\n" (List.map tmpWonkyInstrToString wonkyprog) ^ "\n"

let tmp3AddrBinopToString(tmp3binop : tmp3AddrBinop) =
  match tmp3binop with
        Tmp3AddrAdd(tmpsrc1, tmpsrc2, tmpdest) -> concat "" [tmpAssemLocToString(tmpdest); " <-- "; 
                                                    tmpAssemArgToString(tmpsrc1); " + "; tmpAssemArgToString(tmpsrc2)]
      | Tmp3AddrMul(tmpsrc1, tmpsrc2, tmpdest) -> concat "" [tmpAssemLocToString(tmpdest); " <-- "; 
                                                    tmpAssemArgToString(tmpsrc1); " * "; tmpAssemArgToString(tmpsrc2)]

let tmp3AddrInstrToString(tmp3instr : tmp3AddrInstr) = 
  match tmp3instr with
        Tmp3AddrMov(tmpsrc, tmpdest) -> concat "" [tmpAssemLocToString(tmpdest); " <-- "; tmpAssemArgToString(tmpsrc)]
      | Tmp3AddrBinop(tmpbinop) -> tmp3AddrBinopToString(tmpbinop)
      | Tmp3AddrReturn(tmparg) -> concat "" ["return "; tmpAssemArgToString(tmparg)]

let tmp3AddrProgToString(tmp3addrprog : tmp3AddrProg) = 
  concat "\n" (List.map tmp3AddrInstrToString tmp3addrprog)
