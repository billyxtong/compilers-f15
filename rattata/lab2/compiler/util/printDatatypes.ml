open Datatypesv1
open String

let c0typeToString (c : c0type) =
  match c with
        INT -> "int "
      | BOOL -> "bool "

let constToString (c : const) = string_of_int(c)

let regToString (r : reg) = 
  match r with
        EAX -> "%eax"
      | EBX -> "%ebx" (* callee-saved *)
      | ECX -> "%ecx"
      | EDX -> "%edx"
      | RBP -> "%rbp" (* callee-saved *)
      | RSP -> "%rsp" (* callee-saved *)
      | ESI -> "%esi" (* callee-saved *) 
      | EDI -> "%edi" (* callee-saved *)
      | R8  -> "%r8d"
      | R9  -> "%r9d"
      | R10 -> "%r10d"
      | R11 -> "%r11d"
      | R12 -> "%r12d" (* callee-saved *)
      | R13 -> "%r13d" (* callee-saved *)
      | R14 -> "%r14d" (* callee-saved *)
      | R15 -> "%r15d" (* callee-saved *)

let memAddrToString ((register, offset) : memAddr) = 
    concat "" [string_of_int(offset); "("; regToString(register); ")"]

let assemLocToString(loc : assemLoc) =
  match loc with
        Reg(r) -> regToString(r)
      | MemAddr(register, offset) -> memAddrToString(register, offset)

let assemArgToString (arg : assemArg) = 
  match arg with
        AssemLoc(loc) -> assemLocToString(loc)
      | Const(c) -> concat "" ["$"; constToString(c)]

let intBinopToString (op: intBinop) =
  match op with
        ADD -> "addl "
      | SUB -> "subl "
      | MUL -> "imull "
      | FAKEDIV -> "fakediv "
      | FAKEMOD -> "fakemod "
      | BIT_AND -> "andl "
      | BIT_OR -> "orl "
      | BIT_XOR -> "xorl "
      | RSHIFT -> "sarl "
      | LSHIFT -> "sall "

let boolInstrToString (instr : boolInstr) =
  match instr with
        TEST(arg1, arg2) -> concat "" ["test "; assemArgToString(arg1); ", "; 
                                                assemLocToString(arg2)]
      | CMP(arg1, arg2) -> concat "" ["cmpl "; assemArgToString(arg1); ", "; 
                                               assemLocToString(arg2)]

let assemIntInstrToString((intOp, src, dest) : assemBinopInstr) = 
    concat "" [intBinopToString intOp; assemArgToString(src); ", "; 
               assemLocToString(dest)]

let jumpToString (j : jump) =
  match j with
        JNE -> "jne "
      | JE -> "je "
      | JG -> "jg "
      | JLE -> "jle "
      | JMP_UNCOND -> "jmp "

let labelToString (l : label) = concat "" [".L"; string_of_int(l)]

let jumpInstrToString (j, l) = concat "" [jumpToString(j); labelToString(l)]

let assemInstrToString(instr : assemInstr) = 
  match instr with
        MOV(src, dest) ->
            concat "" ["movl "; 
            assemArgToString(src); ", ";
            assemLocToString(dest)]
      | MOVQ(src, dest) ->
            concat "" ["movq "; 
            assemArgToString(src); ", ";
            assemLocToString(dest)]
      | INT_BINOP(intinstr) -> assemIntInstrToString(intinstr)
      | PUSH(r) -> concat "" ["push "; regToString(r)]
      | POP(r) -> concat "" ["pop "; regToString(r)]
      | RETURN -> "ret"
      | JUMP(jInstr) -> jumpInstrToString(jInstr)
      | BOOL_INSTR(bInstr) -> boolInstrToString(bInstr)
      | LABEL(l) -> labelToString(l)

let assemProgToString(assemprog : assemProg) = 
  concat "\n" (List.map assemInstrToString assemprog) ^ "\n"

let assemInstrWonkyToString(wonkyInstr : assemInstrWonky) = 
  match wonkyInstr with
        AssemInstr(instr) -> assemInstrToString(instr)
      | CDQ -> "cdq"
      | IDIV(divisor) ->
           concat "" ["idivl "; 
           assemArgToString(divisor)]

let assemProgWonkyToString(wonkyAssemProg : assemProgWonky) = 
  concat "\n" (List.map assemInstrWonkyToString wonkyAssemProg) ^ "\n"

let tmpToString(Tmp(t) : tmp) = concat "" ["t"; string_of_int t]

let tmpArgToString(tArg : tmpArg) = 
  match tArg with
        TmpLoc(t) -> tmpToString(t)
      | TmpConst(c) -> constToString(c)

let tmpBoolInstrToString(tmpbool : tmpBoolInstr) =
  match tmpbool with
        TmpTest(arg, t) -> concat "" ["test "; tmpArgToString(arg); ", "; 
                                               tmpToString(t)]
      | TmpCmp(arg, t) -> concat "" ["cmpl "; tmpArgToString(arg); ", "; 
                                              tmpToString(t)]

let tmp2AddrBinopToString((binop, arg, temp) : tmp2AddrBinop) =
    concat "" [tmpToString(temp); " <-- "; 
    tmpToString(temp); intBinopToString(binop);
    tmpArgToString(arg)]
     
let tmp2AddrInstrToString(tmp2instr : tmp2AddrInstr) = 
  match tmp2instr with
        Tmp2AddrMov(arg, temp) -> 
            concat "" [tmpToString(temp); " <-- "; 
            tmpArgToString(arg)]
      | Tmp2AddrBinop(tmpbinop) -> tmp2AddrBinopToString(tmpbinop)
      | Tmp2AddrReturn(tmparg) -> 
            concat "" ["return "; tmpArgToString(tmparg)]
      | Tmp2AddrJump(jumpinstr) -> jumpInstrToString(jInstr)
      | Tmp2AddrBoolInstr(boolinstr) -> tmpBoolInstrToString(boolinstr)
      | Tmp2AddrLabel(l) -> labelToString(l)

let tmp2AddrProgToString(tmp2addrprog : tmp2AddrProg) =
  (* We want a newline at the end *)
  concat "\n" (List.map tmp2AddrInstrToString tmp2addrprog) ^ "\n"

let tmp3AddrBinopToString((binop, arg1, arg2, temp) : tmp3AddrBinop) =
    concat "" [tmpToString(temp); " <-- "; 
    tmpArgToString(arg1); intBinopToString(binop); 
    tmpArgToString(arg2)]

let tmp3AddrInstrToString(tmp3instr : tmp3AddrInstr) = 
  match tmp3instr with
        Tmp3AddrMov(arg, temp) -> 
            concat "" [tmpToString(temp); " <-- "; 
            tmpArgToString(arg)]
      | Tmp3AddrBinop(tmpbinop) -> tmp3AddrBinopToString(tmpbinop)
      | Tmp3AddrReturn(tmparg) -> 
            concat "" ["return "; tmpArgToString(tmparg)]
      | Tmp3AddrJump(jumpinstr) -> jumpInstrToString(jumpinstr)
      | Tmp3AddrBoolInstr(boolinstr) -> tmpBoolInstrToString(boolinstr)
      | Tmp3AddrLabel(l) -> labelToString(l)

let tmp3AddrProgToString(tmp3addrprog : tmp3AddrProg) = 
  concat "\n" (List.map tmp3AddrInstrToString tmp3addrprog) ^ "\n"

