open Datatypesv1
open String
open Ast
let identToString(i : ident) = i

let c0typeToString (c : c0type) =
  match c with
        INT -> "int "
      | BOOL -> "bool "
      | VOID -> "void "
      | TypedefType(identifier) -> identToString(identifier) ^ " "

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

let regToString64 (r : reg) = 
  match r with
        EAX -> "%rax"
      | EBX -> "%rbx" (* callee-saved *)
      | ECX -> "%rcx"
      | EDX -> "%rdx"
      | RBP -> "%rbp" (* callee-saved *)
      | RSP -> "%rsp" (* callee-saved *)
      | ESI -> "%rsi" (* callee-saved *) 
      | EDI -> "%rdi" (* callee-saved *)
      | R8  -> "%r8"
      | R9  -> "%r9"
      | R10 -> "%r10"
      | R11 -> "%r11"
      | R12 -> "%r12" (* callee-saved *)
      | R13 -> "%r13" (* callee-saved *)
      | R14 -> "%r14" (* callee-saved *)
      | R15 -> "%r15" (* callee-saved *)

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
        TEST(arg1, arg2) -> concat "" ["testl "; assemArgToString(arg1); ", "; 
                                                assemLocToString(arg2)]
      | CMP(arg1, arg2) -> concat "" ["cmpl "; assemArgToString(arg1); ", "; 
                                               assemLocToString(arg2)]

let assemIntInstrToString((intOp, src, dest) : assemIntInstr) =
    let srcString = (match (intOp, src) with
                       (LSHIFT, AssemLoc (Reg ECX)) -> "%cl"
                     | (RSHIFT, AssemLoc (Reg ECX)) -> "%cl"
                     | _ -> assemArgToString(src)) in
    concat "" [intBinopToString intOp; srcString; ", "; 
               assemLocToString(dest)]

let jumpToString (j : jump) =
  match j with
        JNE -> "jne "
      | JE -> "je "
      | JG -> "jg "
      | JLE -> "jle "
      | JL -> "jl "
      | JGE -> "jge "
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
      | ADDQ(src, dest) ->
            concat "" ["addq "; 
            assemArgToString(src); ", ";
            assemLocToString(dest)]
      | SUBQ(src, dest) ->
            concat "" ["subq "; 
            assemArgToString(src); ", ";
            assemLocToString(dest)]
      | INT_BINOP(intinstr) -> assemIntInstrToString(intinstr)
      | PUSH(r) -> concat "" ["push "; regToString64(r)]
      | POP(r) -> concat "" ["pop "; regToString64(r)]
      | RETURN -> "ret"
      | JUMP(jInstr) -> jumpInstrToString(jInstr)
      | BOOL_INSTR(bInstr) -> boolInstrToString(bInstr)
      | LABEL(l) -> labelToString(l) ^ ":"
      | CALL(i) -> "call " ^ identToString(i)

let assemFunDefToString(AssemFunDef(funcName, instrList)) =
  ".globl " ^ identToString(funcName) ^ "\n" ^
  identToString(funcName)  ^ ":\n" ^ 
  (concat "\n" (List.map assemInstrToString instrList)) ^ "\n"

let assemProgToString(assemprog : assemFunDef list) = 
  concat "\n" (List.map assemFunDefToString assemprog) ^ "\n"

let assemInstrWonkyToString(wonkyInstr : assemInstrWonky) = 
  match wonkyInstr with
        AssemInstr(instr) -> assemInstrToString(instr)
      | CDQ -> "cdq"
      | IDIV(divisor) ->
           concat "" ["idivl "; 
           assemArgToString(divisor)]

let assemFunDefWonkyToString(WonkyFunDef(funcName, instrList)) =
  ".globl " ^ identToString(funcName) ^ "\n" ^
  identToString(funcName)  ^ ":\n" ^ 
  (concat "\n" (List.map assemInstrWonkyToString instrList)) ^ "\n"

let assemProgWonkyToString(prog: assemProgWonky) = 
  concat "\n" (List.map assemFunDefWonkyToString prog) ^ "\n"

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
      | Tmp2AddrJump(jumpinstr) -> jumpInstrToString(jumpinstr)
      | Tmp2AddrBoolInstr(boolinstr) -> tmpBoolInstrToString(boolinstr)
      | Tmp2AddrLabel(l) -> labelToString(l)
      | Tmp2AddrFunCall(i,args,tmpOpt) ->
          let thing = identToString(i) ^ "(" ^ (concat "," (List.map tmpArgToString args)) ^ ")" in
          (match tmpOpt with
                 Some t -> tmpToString(t) ^ " <--" ^ thing
               | None -> thing)

let tmp2AddrFunDefToString (Tmp2AddrFunDef (i,temps,instrList)) =
  identToString(i) ^ "(" ^ (concat ", " (List.map tmpToString temps)) ^ ") {\n" ^
  (concat "\n" (List.map tmp2AddrInstrToString instrList)) ^ "}\n"

let tmp2AddrProgToString(tmp2addrprog : tmp2AddrProg) =
  (* We want a newline at the end *)
  concat "\n" (List.map tmp2AddrFunDefToString tmp2addrprog) ^ "\n"

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
      | Tmp3AddrFunCall(i,args,tmpOpt) ->
          let thing = identToString(i) ^ "(" ^ (concat "," (List.map tmpArgToString args)) ^ ")" in
          (match tmpOpt with
                 Some t -> tmpToString(t) ^ " <--" ^ thing
               | None -> thing)

let tmp3AddrFunDefToString (Tmp3AddrFunDef (i,temps,instrList)) =
  identToString(i) ^ "(" ^ (concat ", " (List.map tmpToString temps)) ^ ") {\n" ^
  (concat "\n" (List.map tmp3AddrInstrToString instrList)) ^ "}\n"


let tmp3AddrProgToString(tmp3addrprog : tmp3AddrProg) = 
  concat "\n" (List.map tmp3AddrFunDefToString tmp3addrprog) ^ "\n"

let rec tmpIntExprToString(tmpintexpr : tmpIntExpr) =
  match tmpintexpr with
        TmpIntArg(tArg) -> tmpArgToString(tArg)
      | TmpInfAddrBinopExpr(op, expr1, expr2) -> concat "" ["("; tmpIntExprToString(expr1); 
                                                            " "; intBinopToString(op); 
                                                            " "; tmpIntExprToString(expr2); ")"]
      | TmpInfAddrIntFunCall(i,args) ->
          identToString(i) ^ "(" ^ (concat "," (List.map tmpExprToString args)) ^ ")"

and tmpBoolExprToString(bExpr : tmpBoolExpr) = 
  match bExpr with
        TmpBoolArg(t) -> tmpArgToString(t)
      | TmpInfAddrBoolFunCall(i,args) ->
          identToString(i) ^ "(" ^ (concat "," (List.map tmpExprToString args)) ^ ")"


and tmpExprToString(tExpr : tmpExpr) =
  match tExpr with
        TmpBoolExpr(bExpr) -> tmpBoolExprToString(bExpr)
      | TmpIntExpr(iExpr) -> tmpIntExprToString(iExpr)

and tmpInfAddrBoolInstrToString(tInstr : tmpInfAddrBoolInstr) =
  match tInstr with
        TmpInfAddrTest(bExpr1, bExpr2) -> concat "" ["test "; tmpBoolExprToString(bExpr1); ", "; tmpBoolExprToString(bExpr2)]
      | TmpInfAddrCmp(iExpr1, iExpr2) -> concat "" ["cmp "; tmpIntExprToString(iExpr1); ", "; tmpIntExprToString(iExpr2)]

let tmpInfAddrInstrToString(t : tmpInfAddrInstr) =
  match t with
        TmpInfAddrMov(tExpr,t) -> concat "" [tmpToString(t); " <-- "; tmpExprToString(tExpr)]
      | TmpInfAddrJump(jInstr) -> jumpInstrToString(jInstr)
      | TmpInfAddrBoolInstr(bInstr) -> tmpInfAddrBoolInstrToString(bInstr)
      | TmpInfAddrLabel(l) -> labelToString(l)
      | TmpInfAddrReturn(tExpr) -> concat "" ["return "; tmpExprToString(tExpr)]
      | TmpInfAddrVoidFunCall(i,args) ->
          identToString(i) ^ "(" ^ (concat "," (List.map tmpExprToString args)) ^ ")"

let tmpInfAddrFunDefToString (TmpInfAddrFunDef (i,temps,instrList)) =
  identToString(i) ^ "(" ^ (concat ", " (List.map tmpToString temps)) ^ ") {\n" ^
  (concat "\n" (List.map tmpInfAddrInstrToString instrList)) ^ "}\n"


let tmpInfAddrProgToString(instrs : tmpInfAddrFunDef list) = 
  concat "\n" (List.map tmpInfAddrFunDefToString instrs) ^ "\n"

