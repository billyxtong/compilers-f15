open Datatypesv1
open String

let c0typeToString (c : c0type) =
  match c with
        INT -> "int"
      | PTR -> "pointer"

let constToString (c : const) = string_of_int(c)

let regToString (r : reg) = 
  match r with
        EAX -> "%eax"
      | EBX -> "%ebx"
      | ECX -> "%ecx"
      | EDX -> "%edx"
      | EBP -> "%ebp"
      | RSP -> "%rsp"
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

let memAddrToString ((register, offset) : memAddr) = 
    concat "" [string_of_int(offset); "("; 
    regToString(register); ")"]

let assemLocToString(loc : assemLoc) =
  match loc with
        Reg(r) -> regToString(r)
      | MemAddr(register, offset) -> memAddrToString(register, offset)

let assemArgToString (arg : assemArg) = 
  match arg with
        AssemLoc(loc) -> assemLocToString(loc)
      | Const(c) -> constToString(c)

let assemBinopToString(binop : assemBinop) = 
  match binop with
        ADD(src, dest) -> 
            concat "" ["addl "; 
            assemArgToString(src); ", "; 
            assemLocToString(dest)]
      | SUB(src, dest) -> 
            concat "" ["subl "; 
            assemArgToString(src); ", "; 
            assemLocToString(dest)]
      | MUL(src, dest) -> 
            concat "" ["imull "; 
            assemArgToString(src); ", "; 
            assemLocToString(dest)]
      | FAKEDIV(src, dest) -> 
            concat "" ["idivl "; 
            assemArgToString(src); ", "; 
            assemLocToString(dest)]
      | FAKEMOD(src, dest) -> 
            concat "" ["imodl "; 
            assemArgToString(src); ", "; 
            assemLocToString(dest)]

let assemInstrToString(instr : assemInstr) = 
  match instr with
        MOV(src, dest) ->
            concat "" ["movl "; 
            assemArgToString(src); ", ";
            assemLocToString(dest)]
      | BINOP(binop) -> assemBinopToString(binop)
      | RETURN -> "ret"

let assemProgToString(assemprog : assemProg) = 
  concat "\n" (List.map assemInstrToString assemprog) ^ "\n"

let assemInstrWonkyToString(wonkyInstr : assemInstrWonky) = 
  match wonkyInstr with
        AssemInstr(instr) -> assemInstrToString(instr)
      | CDQ -> "cdq"
      | IDIV(divisor) ->
           concat "" ["idiv "; 
           assemArgToString(divisor)]

let assemProgWonkyToString(wonkyAssemProg : assemProgWonky) = 
  concat "\n" (List.map assemInstrWonkyToString wonkyAssemProg) ^ "\n"

let tmpToString(Tmp(t) : tmp) = concat "" ["t"; string_of_int t]

let tmpArgToString(tArg : tmpArg) = 
  match tArg with
        TmpLoc(t) -> tmpToString(t)
      | TmpConst(c) -> constToString(c)

let tmpBinopToString(binop) = 
  match binop with
        TmpAdd -> " + "
      | TmpSub -> " - "
      | TmpMul -> " * "
      | TmpDiv -> " / "
      | TmpMod -> " % "

let tmp2AddrBinopToString(tmp2binop : tmp2AddrBinop) =
  match tmp2binop with
        Tmp2AddrAdd(binop, arg, temp) ->
            concat "" [tmpToString(temp); " <-- "; 
            tmpToString(temp); tmpBinopToString(binop);
            tmpArgToString(arg)]
      | Tmp2AddrSub(binop, arg, temp) ->
            concat "" [tmpToString(temp); " <-- ";
            tmpToString(temp); tmpBinopToString(binop);
            tmpArgToString(arg)]
      | Tmp2AddrMul(binop, arg, temp) -> 
            concat "" [tmpToString(temp); " <-- "; 
            tmpToString(temp); tmpBinopToString(binop); 
            tmpArgToString(arg)]
      | Tmp2AddrDiv(binop, arg, temp) ->
            concat "" [tmpToString(temp); " <-- ";
            tmpToString(temp); tmpBinopToString(binop);
            tmpArgToString(arg)]
      | Tmp2AddrMod(binop, arg, temp) ->
            concat "" [tmpToString(temp); " <-- ";
            tmpToString(temp); tmpBinopToString(binop);
            tmpArgToString(arg)]

let tmp2AddrInstrToString(tmp2instr : tmp2AddrInstr) = 
  match tmp2instr with
        Tmp2AddrMov(arg, temp) -> 
            concat "" [tmpToString(temp); " <-- "; 
            tmpArgToString(arg)]
      | Tmp2AddrBinop(tmpbinop) -> tmp2AddrBinopToString(tmpbinop)
      | Tmp2AddrReturn(tmparg) -> 
            concat "" ["return "; tmpArgToString(tmparg)]

let tmp2AddrProgToString(tmp2addrprog : tmp2AddrProg) =
  (* We want a newline at the end *)
  concat "\n" (List.map tmp2AddrInstrToString tmp2addrprog) ^ "\n"

let tmp3AddrBinopToString(tmp3binop : tmp3AddrBinop) =
  match tmp3binop with
        Tmp3AddrAdd(binop, arg1, arg2, temp) -> 
            concat "" [tmpToString(temp); " <-- "; 
            tmpArgToString(arg1); tmpBinopToString(binop); 
            tmpArgToString(arg2)]
      | Tmp3AddrSub(binop, arg1, arg2, temp) -> 
            concat "" [tmpToString(temp); " <-- "; 
            tmpArgToString(arg1); tmpBinopToString(binop); 
            tmpArgToString(arg2)]
      | Tmp3AddrMul(binop, arg1, arg2, temp) -> 
            concat "" [tmpToString(temp); " <-- "; 
            tmpArgToString(arg1); tmpBinopToString(binop); 
            tmpArgToString(arg2)]
      | Tmp3AddrDiv(binop, arg1, arg2, temp) -> 
            concat "" [tmpToString(temp); " <-- "; 
            tmpArgToString(arg1); tmpBinopToString(binop); 
            tmpArgToString(arg2)]
      | Tmp3AddrMod(binop, arg1, arg2, temp) -> 
            concat "" [tmpToString(temp); " <-- "; 
            tmpArgToString(arg1); tmpBinopToString(binop); 
            tmpArgToString(arg2)]

let tmp3AddrInstrToString(tmp3instr : tmp3AddrInstr) = 
  match tmp3instr with
        Tmp3AddrMov(arg, temp) -> 
            concat "" [tmpToString(temp); " <-- "; 
            tmpArgToString(arg)]
      | Tmp3AddrBinop(tmpbinop) -> tmp3AddrBinopToString(tmpbinop)
      | Tmp3AddrReturn(tmparg) -> 
            concat "" ["return "; tmpArgToString(tmparg)]

let tmp3AddrProgToString(tmp3addrprog : tmp3AddrProg) = 
  concat "\n" (List.map tmp3AddrInstrToString tmp3addrprog) ^ "\n"
