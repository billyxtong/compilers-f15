
let necessityRule1(Tmp2AddrReturn(arg) : tmp2AddrInstr) =
  match arg with
    TmpLoc(TmpVar(t)) -> [t]
   |TmpLoc(TmpDeref(t)) -> [t]
   |_ -> []

let necessityRuleHelper (arg : tmpArg) (loc : tmpLoc) =
    match (arg,loc) with
        (TmpLoc(TmpVar(t1)),TmpVar(t2))-> [t1,t2]
       |(TmpLoc(TmpVar(t1)),TmpDeref(t2))-> [t1,t2]
       |(TmpLoc(TmpDeref(t1)),TmpVar(t2))-> [t1,t2]
       |(TmpLoc(TmpDeref(t1)),TmpDeref(t2))-> [t1,t2]
       |(_,TmpVar(t))-> [t]
       |(_,TmpDeref(t))-> [t]

let necessityRule2(Tmp2AddrBinop(op,arg,loc) : tmp2AddrInstr) =
  match op with
    FAKEDIV-> necessityRuleHelper arg loc
   |FAKEMOD-> necessityRuleHelper arg loc
   |RSHIFT-> necessityRuleHelper arg loc
   |LSHIFT-> necessityRuleHelper arg loc
   |_ -> []

let necessityRule3(Tmp2AddrBoolInstr(boolInstr) : tmp2AddrInstr) =
  match boolInstr with
    TmpTest(arg,loc)-> necessityRuleHelper arg loc
   |TmpCmp(s,arg,loc)-> necessityRuleHelper arg loc

let necessityRule4or5(Tmp2AddrMov(s,arg,loc) : tmp2AddrInstr) =
  match (arg,loc) with
    (TmpLoc(TmpDeref(t1)), TmpVar(t2)) -> [t1]
   |(TmpLoc(TmpVar(t1)),TmpDeref(t2)) -> [t1,t2]
   |_ -> []

(* Returns a list of temps necessary for the instruction to execute.
 * Note that only certain types of instrs have "necessary" temps. *)
let getNecessaryTemps (currLine : int) (indexedProg : tmp2AddrInstr array) =
  let instr = indexedProg.(currLine) in 
  match instr with
    Tmp2AddrReturn(_) -> necessityRule1(instr)
   |Tmp2AddrBinop(_) -> necessityRule2(instr)
   |Tmp2AddrBoolInstr(_) -> necessityRule3(instr)
   |Tmp2AddrMov(_) -> necessityRule4or5(instr)
   |_ -> []
