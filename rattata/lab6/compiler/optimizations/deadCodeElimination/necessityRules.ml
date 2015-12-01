open Datatypesv1

let necessityRule1(Tmp2AddrReturn(s,arg) : tmp2AddrInstr) =
  match arg with
    TmpLoc(TmpVar(Tmp t)) -> [t]
   |TmpLoc(TmpDeref(Tmp t)) -> [t]
   |_ -> []

let necessityRuleHelper (arg : tmpArg) (loc : tmpLoc) =
    match (arg,loc) with
        (TmpLoc(TmpVar(Tmp t1)),TmpVar(Tmp t2))-> [t1;t2]
       |(TmpLoc(TmpVar(Tmp t1)),TmpDeref(Tmp t2))-> [t1;t2]
       |(TmpLoc(TmpDeref(Tmp t1)),TmpVar(Tmp t2))-> [t1;t2]
       |(TmpLoc(TmpDeref(Tmp t1)),TmpDeref(Tmp t2))-> [t1;t2]
       |(_,TmpVar(Tmp t))-> [t]
       |(_,TmpDeref(Tmp t))-> [t]

let necessityRule2(Tmp2AddrBinop(op,arg,loc) : tmp2AddrInstr) =
  match op with
    FAKEDIV-> necessityRuleHelper arg loc
   |FAKEMOD-> necessityRuleHelper arg loc
   |_ -> []

let necessityRule3(Tmp2AddrBoolInstr(boolInstr) : tmp2AddrInstr) =
  match boolInstr with
    TmpTest(arg,loc)-> necessityRuleHelper arg loc
   |TmpCmp(s,arg,loc)-> necessityRuleHelper arg loc

let necessityRule4(Tmp2AddrMov(s,arg,loc) : tmp2AddrInstr) =
  match (arg,loc) with
    (TmpLoc(TmpDeref(Tmp t1)), TmpVar(Tmp t2)) -> [t1]
   |(TmpLoc(TmpVar(Tmp t1)),TmpDeref(Tmp t2)) -> [t1;t2]
   |(TmpLoc(TmpDeref(Tmp t1)), TmpDeref(Tmp t2)) -> [t1;t2] 
   (* is this true? it falls under memory store, so it should be; will test later *)
   |_ -> [] (* var to var moves have no "necessary" temps *)

let rec necessityRule5Helper (funcArgs : tmpArg list) (l : int list) =
  match funcArgs with
    [] -> l
   |arg :: args -> 
       (match arg with
          TmpLoc(TmpVar(Tmp t)) -> necessityRule5Helper args (t :: l)
         |TmpLoc(TmpDeref(Tmp t)) -> necessityRule5Helper args (t :: l)
         |_ -> necessityRule5Helper args l)

let necessityRule5(Tmp2AddrFunCall(s,fName,fArgs,dest) : tmp2AddrInstr) =
  match dest with
    Some d -> necessityRule5Helper (TmpLoc(d) :: fArgs) []
   |None -> necessityRule5Helper (TmpLoc(TmpVar (Tmp (Temp.create()))) :: fArgs) []

(* Returns a list of temps necessary for the instruction to execute.
 * Note that only certain types of instrs have "necessary" temps. *)
let getNecessaryTemps (currLine : int) (indexedProg : tmp2AddrInstr array) =
  let instr = indexedProg.(currLine) in 
  match instr with
    Tmp2AddrReturn(_) -> necessityRule1(instr)
   |Tmp2AddrBinop(_) -> necessityRule2(instr)
   |Tmp2AddrBoolInstr(_) -> necessityRule3(instr)
   |Tmp2AddrMov(_) -> necessityRule4(instr)
   |Tmp2AddrFunCall(_) -> necessityRule5(instr)
   |_ -> []
