open Datatypesv1
open String
open Ast
let identToString(i : ident) = i

let sizeToString = function
      BIT32 -> "(32-bit)"
    | BIT64 -> "(64-bit)"

let rec c0typeToString (c : c0type) =
  match c with
        INT -> "int "
      | BOOL -> "bool "
      | VOID -> "void "
      | TypedefType(identifier) -> identToString(identifier) ^ " "
      | Pointer(c) -> c0typeToString(c) ^ "*"
      | Array(c) -> c0typeToString(c) ^ "[]"
      | Struct(i) -> "struct " ^ identToString(i) ^ " "
      | Poop -> "nulltype"
        
let constToString (c : const) = string_of_int(c)

let regToString32 (r : reg) = 
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

let regToString (r : reg) regSize =
    match regSize with
        BIT32 -> regToString32 r
      | BIT64 -> regToString64 r

let memAddrToString ((register, offset) : memAddr) = 
    concat "" [string_of_int(offset); "("; regToString64(register); ")"]

let assemLocToString(loc : assemLoc) locSize =
  match loc with
        Reg(r) -> regToString r locSize
      | MemAddr(register, offset) -> memAddrToString(register, offset)
      | RegDeref(r) -> "(" ^ regToString64(r) ^ ")"
      | MemAddrDeref(register, offset) -> memAddrToString(register, offset)

let assemArgToString (arg : assemArg) argSize = 
  match arg with
        AssemLoc(loc) -> assemLocToString loc argSize
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
        TEST(arg1, arg2) -> concat "" ["testl "; assemArgToString arg1 BIT32;
                                       ", "; assemLocToString arg2 BIT32]
      | CMP(s, arg1, arg2) -> 
          (match s with
                 BIT32 -> concat "" ["cmpl "; assemArgToString(arg1) s; ", "; 
                                               assemLocToString(arg2) s]
               | BIT64 -> concat "" ["cmpq "; assemArgToString(arg1) s; ", "; 
                                               assemLocToString(arg2) s])

let assemIntInstrToString((intOp, src, dest) : assemIntInstr) =
    let srcString = (match (intOp, src) with
                       (LSHIFT, AssemLoc (Reg ECX)) -> "%cl"
                     | (RSHIFT, AssemLoc (Reg ECX)) -> "%cl"
                     | _ -> assemArgToString(src) BIT32) in
    concat "" [intBinopToString intOp; srcString; ", "; 
               assemLocToString(dest) BIT32]

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
        MOV(s, src, dest) ->
          (match s with
                 BIT32 -> concat "" ["movl "; 
                          assemArgToString(src) s; ", ";
                          assemLocToString(dest) s]
               | BIT64 -> concat "" ["movq "; 
                          assemArgToString(src) s; ", ";
                          assemLocToString(dest) s])
      | PTR_BINOP(op,src,dest) -> 
          (match op with
                 PTR_ADD -> concat "" ["addq "; 
                            assemArgToString(src) BIT64; ", ";
                            assemLocToString(dest) BIT64]
               | PTR_SUB -> concat "" ["subq "; 
                            assemArgToString(src) BIT64; ", ";
                            assemLocToString(dest) BIT64])
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
           assemArgToString(divisor) BIT32]

let assemFunDefWonkyToString(WonkyFunDef(funcName, instrList)) =
  ".globl " ^ identToString(funcName) ^ "\n" ^
  identToString(funcName)  ^ ":\n" ^ 
  (concat "\n" (List.map assemInstrWonkyToString instrList)) ^ "\n"

let assemProgWonkyToString(prog: assemProgWonky) = 
  concat "\n" (List.map assemFunDefWonkyToString prog) ^ "\n"

let tmpToString(Tmp(t) : tmp) = concat "" ["t"; string_of_int t]

let tmpLocToString (tLoc : tmpLoc) =
    match tLoc with
       TmpVar t -> tmpToString t
     | TmpDeref(t) -> "*" ^ tmpToString(t)                

let tmpArgToString(tArg : tmpArg) = 
  match tArg with
        TmpLoc(t) -> tmpLocToString(t)
      | TmpConst(c) -> constToString(c)

let tmpBoolInstrToString(tmpbool : tmpBoolInstr) =
  match tmpbool with
        TmpTest(arg, t) -> concat "" ["test "; tmpArgToString(arg); ", "; 
                                               tmpLocToString(t)]
      | TmpCmp(s, arg, t) ->
         (match s with
                BIT32 -> concat "" ["cmpl "; tmpArgToString(arg); ", "; 
                                              tmpLocToString(t)]
              | BIT64 -> concat "" ["cmpq "; tmpArgToString(arg); ", "; 
                                              tmpLocToString(t)])

let tmp2AddrBinopToString((binop, arg, temp) : tmp2AddrBinop) =
    concat "" [tmpLocToString(temp); " <-- "; 
    tmpLocToString(temp); intBinopToString(binop);
    tmpArgToString(arg)]
     
let tmp2AddrInstrToString(tmp2instr : tmp2AddrInstr) = 
  match tmp2instr with
        Tmp2AddrMov(s, arg, temp) -> concat "" [tmpLocToString(temp); " <--";
                                                sizeToString s; " ";
                                      tmpArgToString(arg)]
      | Tmp2AddrPtrBinop(op, arg, temp) -> 
          (match op with
                 PTR_ADD -> concat "" [tmpLocToString temp; " <-- ";
                                 tmpLocToString temp; "addq "; tmpArgToString arg]
               | PTR_SUB -> concat "" [tmpLocToString temp; " <-- ";
                                 tmpLocToString temp; "subq "; tmpArgToString arg])
      | Tmp2AddrBinop(tmpbinop) -> tmp2AddrBinopToString(tmpbinop)
      | Tmp2AddrReturn(s, tmparg) -> 
            concat "" ["return"; sizeToString s; " "; tmpArgToString(tmparg)]
      | Tmp2AddrJump(jumpinstr) -> jumpInstrToString(jumpinstr)
      | Tmp2AddrBoolInstr(boolinstr) -> tmpBoolInstrToString(boolinstr)
      | Tmp2AddrLabel(l) -> labelToString(l)
      | Tmp2AddrFunCall(s, i,args,tmpOpt) ->
          let thing = identToString(i) ^ "(" ^ (concat "," (List.map tmpArgToString args)) ^ ")" in
          (match tmpOpt with
                 Some t -> tmpLocToString(t) ^ " <--" ^ sizeToString s
                             ^ " " ^ thing
               | None -> thing)

let tmp2AddrFunDefToString (Tmp2AddrFunDef (i,temps,instrList)) =
  identToString(i) ^ "(" ^ (concat ", "
                   (List.map (fun (t, s) -> tmpToString t) temps)) ^ ") {\n" ^
  (concat "\n" (List.map tmp2AddrInstrToString instrList)) ^ "}\n"

let tmp2AddrProgToString(tmp2addrprog : tmp2AddrProg) =
  (* We want a newline at the end *)
  concat "\n" (List.map tmp2AddrFunDefToString tmp2addrprog) ^ "\n"

let tmp3AddrBinopToString((binop, arg1, arg2, temp) : tmp3AddrBinop) =
    concat "" [tmpLocToString(temp); " <-- "; 
    tmpArgToString(arg1); intBinopToString(binop); 
    tmpArgToString(arg2)]

let tmp3AddrInstrToString(tmp3instr : tmp3AddrInstr) = 
  match tmp3instr with
        Tmp3AddrMov(s, arg, temp) -> 
            concat "" [tmpLocToString(temp); " <--"; sizeToString s; " "; 
            tmpArgToString(arg)]
      | Tmp3AddrPtrBinop(op, arg1, arg2, temp) -> 
          (match op with
                 PTR_ADD -> concat "" [tmpLocToString(temp); "<-- ";
                         tmpArgToString(arg1); "addq "; tmpArgToString(arg2)] 
               | PTR_SUB -> concat "" [tmpLocToString(temp); "<-- ";
                         tmpArgToString(arg1); "subq "; tmpArgToString(arg2)])
      | Tmp3AddrBinop(tmpbinop) -> tmp3AddrBinopToString(tmpbinop)
      | Tmp3AddrReturn(s, tmparg) -> 
            concat "" ["return "; tmpArgToString(tmparg)]
      | Tmp3AddrJump(jumpinstr) -> jumpInstrToString(jumpinstr)
      | Tmp3AddrBoolInstr(boolinstr) -> tmpBoolInstrToString(boolinstr)
      | Tmp3AddrLabel(l) -> labelToString(l)
      | Tmp3AddrFunCall(s, i,args,tmpOpt) ->
          let thing = identToString(i) ^ "(" ^ (concat "," (List.map tmpArgToString args)) ^ ")" in
          (match tmpOpt with
                 Some t -> tmpLocToString(t) ^ " <--" ^ sizeToString s
                           ^ " " ^ thing
               | None -> thing)

let tmp3AddrFunDefToString (Tmp3AddrFunDef (i,temps,instrList)) =
  identToString(i) ^ "(" ^ (concat ", "
             (List.map (fun (t, s) -> tmpToString t) temps)) ^ ") {\n" ^
  (concat "\n" (List.map tmp3AddrInstrToString instrList)) ^ "}\n"


let tmp3AddrProgToString(tmp3addrprog : tmp3AddrProg) = 
  concat "\n" (List.map tmp3AddrFunDefToString tmp3addrprog) ^ "\n"

let rec tmpSharedTypeExprToString(t : tmpSharedTypeExpr) =
  match t with
        TmpInfAddrFunCall(func,args) -> identToString(func) ^ "(" ^ 
        (concat "" (List.map tmpExprToString args)) ^ ")"
      | TmpInfAddrFieldAccess(i1,p,i2) -> tmpPtrExprToString(p) ^ "->" ^ 
                    identToString(i2) ^ "(" ^ identToString(i1) ^ ")"
      | TmpInfAddrArrayAccess(p,i) -> tmpPtrExprToString(p) ^ "[" ^ 
                                      tmpIntExprToString(i) ^ "]"
      | TmpInfAddrDeref(p) -> "*(" ^ tmpPtrExprToString(p) ^")"
      | TmpLValExpr (lval) -> tmpLValToString(lval)                               

and tmpPtrExprToString(p : tmpPtrExpr) =
  match p with
        TmpPtrArg(arg) -> tmpArgToString(arg)
      | TmpPtrSharedExpr(s) -> tmpSharedTypeExprToString(s)
      | TmpAlloc(c) -> "alloc(" ^ c0typeToString(c) ^ ")"
      | TmpAllocArray(c,i) -> "alloc_array(" ^ c0typeToString(c) ^ ", " ^ tmpIntExprToString(i) ^ ")"
      | TmpInfAddrPtrBinop(op,arg,temp) -> 
          (match op with
                 PTR_ADD -> concat "" ["addq "; 
                            tmpPtrExprToString(arg); ", ";
                            tmpIntExprToString(temp)]
               | PTR_SUB -> concat "" ["subq "; 
                            tmpPtrExprToString(arg); ", ";
                            tmpIntExprToString(temp)])

and tmpIntExprToString(tmpintexpr : tmpIntExpr) =
  match tmpintexpr with
        TmpIntArg(tArg) -> tmpArgToString(tArg)
      | TmpIntSharedExpr(s) -> tmpSharedTypeExprToString(s)
      | TmpInfAddrBinopExpr(op, expr1, expr2) -> concat "" ["("; tmpIntExprToString(expr1); 
                                                            " "; intBinopToString(op); 
                                                            " "; tmpIntExprToString(expr2); ")"]
      

and tmpBoolExprToString(bExpr : tmpBoolExpr) = 
  match bExpr with
        TmpBoolArg(t) -> tmpArgToString(t)
      | TmpBoolSharedExpr(s) -> tmpSharedTypeExprToString(s)

and tmpExprToString(tExpr : tmpExpr) =
  match tExpr with
        TmpBoolExpr(bExpr) -> tmpBoolExprToString(bExpr)
      | TmpIntExpr(iExpr) -> tmpIntExprToString(iExpr)
      | TmpPtrExpr(pExpr) -> tmpPtrExprToString(pExpr)

and tmpInfAddrBoolInstrToString(tInstr : tmpInfAddrBoolInstr) =
  match tInstr with
        TmpInfAddrTest(bExpr1, bExpr2) -> concat "" ["test "; tmpBoolExprToString(bExpr1); ", "; tmpBoolExprToString(bExpr2)]
      | TmpInfAddrCmp(s, iExpr1, iExpr2) ->
            concat "" ["cmp"; sizeToString s; " "; tmpExprToString(iExpr1);
                       ", "; tmpExprToString(iExpr2)]

and tmpLValToString (tLVal: tmpLVal) =
  match tLVal with
       TmpFieldAccessLVal (sName, p, fieldName) -> tmpLValToString(p) ^ "->" ^
                          identToString(fieldName) ^ "(" ^ identToString(fieldName) ^ ")"
     | TmpArrayAccessLVal (a, idx) -> tmpLValToString(a) ^ "[" ^ tmpIntExprToString(idx)
                                        ^ "]"
     | TmpDerefLVal (p) -> "*" ^ tmpLValToString(p)
     | TmpVarLVal t -> tmpToString(t)

let tmpInfAddrInstrToString(t : tmpInfAddrInstr) =
  match t with
        TmpInfAddrMov(s, tExpr,lval) -> concat "" [tmpLValToString lval; " <--";
                                sizeToString s; " "; tmpExprToString(tExpr)]
      | TmpInfAddrJump(jInstr) -> jumpInstrToString(jInstr)
      | TmpInfAddrBoolInstr(bInstr) -> tmpInfAddrBoolInstrToString(bInstr)
      | TmpInfAddrLabel(l) -> labelToString(l)
      | TmpInfAddrReturn(s, tExpr) -> concat "" ["return"; sizeToString s;
                                                 " "; tmpExprToString(tExpr)]
      | TmpInfAddrVoidFunCall(i,args) ->
          identToString(i) ^ "(" ^ (concat "," (List.map tmpExprToString args)) ^ ")"

let tmpFieldToString(c,i) = c0typeToString(c) ^ " " ^ identToString(i) ^ ";\n"

let tmpInfAddrGlobalDeclToString (decl : tmpInfAddrGlobalDecl) =
  match decl with
        TmpInfAddrFunDef (i,temps,instrList) ->
  identToString(i) ^ "(" ^ (concat ", "
              (List.map (fun (t,s) -> tmpToString t) temps)) ^ ") {\n" ^
  (concat "\n" (List.map tmpInfAddrInstrToString instrList)) ^ "}\n"
      | TmpStructDef(i, ts) -> "struct " ^ identToString(i) ^ "{\n" ^ 
      (concat "" (List.map tmpFieldToString ts)) ^ "};"

let tmpInfAddrProgToString(instrs : tmpInfAddrGlobalDecl list) = 
  concat "\n" (List.map tmpInfAddrGlobalDeclToString instrs) ^ "\n"

