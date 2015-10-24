(* ident and c0type have to be in here to avoid a circular build
   error :( *)
type c0type = INT | BOOL | VOID | TypedefType of ident | Pointer of c0type
            | Array of c0type
            | Struct of ident 
and ident = string
(* everything in c0 is an int! *)              
type const = int

type reg = EAX | EBX | ECX | EDX | RBP | RSP | ESI | EDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

(* the int is the memory offest from the register *)
(* type memAddr = DirectOffset of reg * int *)
(*              | ArrayStyleOffset of reg * reg * int *)
       (* Direct is 4(rsp) which is *(rsp + 4), ArrayStyle is (rsp, rbx, 4),
          which is *(rsp + rbx * 4) *)

(* actually I think I'm just going to use old memAddr type for now
   (which is only direct offsets *)
type memAddr = reg * int                                   

(* These are for actual assembly instructions. Tmps are not allowed. *)
type intBinop = ADD | MUL | SUB | FAKEDIV | FAKEMOD
              | BIT_AND | BIT_OR | BIT_XOR
              | RSHIFT | LSHIFT              
type assemLoc = Reg of reg | MemAddr of memAddr
type assemArg = AssemLoc of assemLoc | Const of const
type boolInstr = TEST of assemArg * assemLoc
               | CMP32 of assemArg * assemLoc
               | CMP64 of assemArg * assemLoc
type assemIntInstr = intBinop * assemArg * assemLoc
type jump = JNE | JE | JG | JLE | JL | JGE | JMP_UNCOND 
type label = int
type jumpInstr = jump * label
type assemInstr = MOV32 of assemArg * assemLoc
          (* These are the three 64 bit instructions we're using,
             and they're specified separately everywhere. Because
             I don't really want to allow all of the other binops
             for 64 bits *)
                | MOV64 of assemArg * assemLoc
                | SUB64 of assemArg * assemLoc
                | ADD64 of assemArg * assemLoc
                | INT_BINOP of assemIntInstr
                | PUSH of reg
                | POP of reg
                | RETURN
                | JUMP of jumpInstr
                | BOOL_INSTR of boolInstr
                | LABEL of label
                | CALL of ident
type assemFunDef = AssemFunDef of ident * assemInstr list
type assemProg = assemFunDef list

(* Assembly Code with wonky instructions (i.e. idiv, etc) *)
(* This comes after 2Addr in the pipeline, but needs to be below
   here so that they can refer to 2AddrInstrs (since this is a
   strict superset of normal 2Addr *)
type assemInstrWonky = AssemInstr of assemInstr
                   | CDQ (* needed for idiv *)
                   | IDIV of assemArg
type wonkyFunDef = WonkyFunDef of ident * assemInstrWonky list
type assemProgWonky = wonkyFunDef list

(* Below here allows tmps, but also allows actual assembly instructions.
   Note: Because we allow actual assembly instructions, memory
   addresses are allowed. Memory addresses should not occur prior
   to register allocation, at least not in L1 (unless I'm missing
   something) *)
type tmp = Tmp of int
type tmpArg = TmpLoc of tmp | TmpConst of const
(* Two Address Code *)
type tmpBoolInstr = TmpTest of tmpArg * tmp
                  | TmpCmp32 of tmpArg * tmp
                  | TmpCmp64 of tmpArg * tmp (* for pointers *)
type tmp2AddrBinop = intBinop * tmpArg * tmp
type tmp2AddrInstr = Tmp2AddrMov32 of tmpArg * tmp
                   | Tmp2AddrMov64 of tmpArg * tmp
                   | Tmp2AddrAdd64 of tmpArg * tmp
                   | Tmp2AddrSub64 of tmpArg * tmp
                   | Tmp2AddrBinop of tmp2AddrBinop
                   | Tmp2AddrReturn32 of tmpArg
                   | Tmp2AddrReturn64 of tmpArg
                   | Tmp2AddrJump of jumpInstr
                   | Tmp2AddrBoolInstr of tmpBoolInstr
                   | Tmp2AddrLabel of label
                    (* tmp option because voids have no dest *)
                   | Tmp2AddrFunCall of ident * tmpArg list * tmp option
type tmp2AddrFunDef = Tmp2AddrFunDef of ident * tmp list *
                                        tmp2AddrInstr list
type tmp2AddrProg = tmp2AddrFunDef list

(* Three Address Code *)
type tmp3AddrBinop = intBinop * tmpArg * tmpArg *  tmp
type tmp3AddrInstr = Tmp3AddrMov32 of tmpArg *  tmp
                   | Tmp3AddrMov64 of tmpArg *  tmp
                (* ptr derefences are subsumed by mov *)
                   | Tmp3AddrAdd64 of tmpArg * tmp
                   | Tmp3AddrSub64 of tmpArg * tmp
                   | Tmp3AddrBinop of tmp3AddrBinop
                   | Tmp3AddrReturn32 of tmpArg
                   | Tmp3AddrReturn64 of tmpArg
                   | Tmp3AddrJump of jumpInstr
                   | Tmp3AddrBoolInstr of tmpBoolInstr
                   | Tmp3AddrLabel of label
                  (* function name, arg list, dest. Dest is an
                  option because void function don't need to move
                  the result anywhere *)
                   | Tmp3AddrFunCall of ident * tmpArg list * tmp option
               (* alloc, alloc_array become call malloc *)
type tmp3AddrFunDef = Tmp3AddrFunDef of ident * tmp list *
                                        tmp3AddrInstr list
type tmp3AddrProg = tmp3AddrFunDef list

(* Inf Address Code: any number of operands on right hand side *)
(* There are still types for unsimplified memory operations
   (alloc_array, field access, etc) because I'm first going to convert
   to infAddr without touching those. Then, in a second pass, I will
   turn them all into the simplified instructions (call malloc,
   field access using movs, etc. But by the time we're converting
   to 3Addr, all mem ops should be simplified. *)
type tmpSharedTypeExpr = TmpInfAddrFunCall of ident * tmpExpr list
                       | TmpInfAddrFieldAccess of ident * tmpPtrExpr
                                                  * ident
                      (* first ident is the struct type name, for
                         example, "a" in "struct a {int x;};".
                         second ident is the field name. We need to
                         know the struct name so that we know the
                         proper memory offset to get the field
                         (and there might be multiple structs with a
                         field named x *)
                       | TmpInfAddrArrayAccess of tmpPtrExpr * tmpIntExpr
                      (* Again, the tmpIntExpr here is the index, not the
                         offset *)
                       | TmpInfAddrDeref of tmpPtrExpr
and tmpPtrExpr = TmpPtrArg of tmpArg
               | TmpPtrSharedExpr of tmpSharedTypeExpr
               | TmpAlloc of c0type
               | TmpAllocArray of c0type * int
               | TmpInfAddrAdd64 of tmpPtrExpr * tmpIntExpr
               | TmpInfAddrSub64 of tmpPtrExpr * tmpIntExpr
and tmpIntExpr = TmpIntArg of tmpArg
               | TmpIntSharedExpr of tmpSharedTypeExpr
               | TmpInfAddrBinopExpr of intBinop * tmpIntExpr * 
                                         tmpIntExpr
and tmpBoolExpr = TmpBoolArg of tmpArg
                | TmpBoolSharedExpr of tmpSharedTypeExpr
and tmpExpr = TmpBoolExpr of tmpBoolExpr
             | TmpIntExpr of tmpIntExpr
             | TmpPtrExpr of tmpPtrExpr
and tmpInfAddrBoolInstr = TmpInfAddrTest of tmpBoolExpr * tmpBoolExpr
(* Note: by this point, we've already reversed the operationds for
   cmp. That is, cmp (a,b) followed by jg will jump if b > a *)
                        | TmpInfAddrCmp32 of tmpExpr * tmpExpr
                        | TmpInfAddrCmp64 of tmpExpr * tmpExpr
type tmpInfAddrInstr = TmpInfAddrMov32 of tmpExpr * tmp
                   | TmpInfAddrMov64 of tmpPtrExpr * tmp
                   | TmpInfAddrJump of jumpInstr
                   | TmpInfAddrBoolInstr of tmpInfAddrBoolInstr
                   | TmpInfAddrLabel of label
                   | TmpInfAddrReturn32 of tmpExpr
                   | TmpInfAddrReturn64 of tmpExpr
                   | TmpInfAddrVoidFunCall of ident * tmpExpr list
type tmpField = c0type * ident
                
type tmpInfAddrGlobalDecl =
    TmpInfAddrFunDef of ident * tmp list * tmpInfAddrInstr list
    (* function name, param names, instruction list *)
  | TmpStructDef of ident * tmpField list
    (* still need struct defs until the second pass of converting
       to infAddr where we simplify mem instrs *)
type tmpInfAddrProg = tmpInfAddrGlobalDecl list
