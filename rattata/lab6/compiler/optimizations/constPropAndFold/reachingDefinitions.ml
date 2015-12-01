module H = Hashtbl
module L = List
open NeedednessRules

(* populates reachingDefinitionsTable by mapping every defined var to its definitions
 * each definition is mapped to the lines at which it applies *)
let addTempToTable (temp : int) (defLine : int) reachingDefinitionsTable =
  (* if temp has already been defined, we update reachingDefinitionsTable by 
   * adding this definition to its def table *)
  try 
    let defTable = H.find reachingDefinitionsTable temp in
    H.add defTable line (H.create 5)
  (* if temp hasn't been defined yet, we create a new def table for it,
   * add defLine to the new def table, and add the mapping 
   * temp -> newDefTable to reachingDefinitionsTable *)
  with Not_found -> 
    let newDefTable = H.create 5 in
    let () = H.add newDefTable line (H.create 5) in
    H.add reachingDefinitionsTable dest newDefTable

(* gets the defined temp, if any, at a given line in the program, and 
 * calls addTempToTable *)
let getDefTemps prog line reachingDefinitionsTable =
  match (prog.(line)) with
    Tmp3AddrMov(opSize, src, TmpVar (Tmp dest)) -> 
      addTempToTable dest line reachingDefinitionsTable
  | Tmp3AddrBinop(op, src, TmpVar (Tmp dest)) -> 
      addTempToTable dest line reachingDefinitionsTable
  | Tmp3AddrPtrBinop(op, src, TmpVar (Tmp dest)) -> 
      addTempToTable dest line reachingDefinitionsTable
  | Tmp3AddrFunCall(opSize, fName, args, Some (TmpVar (Tmp dest))) -> 
      addTempToTable dest line reachingDefinitionsTable
  | Tmp3AddrMaskUpper (Tmp t) -> 
      addTempToTable t line reachingDefinitionsTable
  | _ -> ()

let finishAddingLinesReached =
  

let addLinesReachedPerTmpDef (t : int) defs prog reachingDefsTbl lineToSuccTbl =
  H.iter (fun def -> fun reachedLines -> 
    let defSuccs = lineToSuccTbl.(def) in
    L.iter (fun succ -> 
      let () = H.add reachedLines succ () in
      finishAddingLinesReached ___________ ) defSuccs) defs
  
let findLinesReachedByDef prog reachingDefsTbl lineToSuccTbl =
  H.iter (fun temp -> fun defs ->
    addLinesReachedPerTmpDef temp defs prog reachingDefsTbl lineToSuccTbl)
  reachingDefsTbl

let findUsedTmps (instr : tmp3AddrInstr) =
  match instr with
    Tmp3AddrMov(s,arg,loc)-> argToTmp arg
  | Tmp3AddrPtrBinop(op,arg1,arg2,loc)-> [] (* I think? *)
  | Tmp3AddrBinop(op,arg,loc)-> (locToTmp loc) :: (argToTmp arg)
  | Tmp3AddrMaskUpper(t)-> [t]
  | Tmp3AddrReturn(s,arg)-> argToTmp arg
  | Tmp3AddrJump(j)-> []
  | Tmp3AddrBoolInstr(boolInstr)-> 
      (match boolInstr with
        TmpTest(arg,loc)-> (locToTmp loc) :: (argToTmp arg)
       |TmpCmp(s,arg,loc)-> (locToTmp loc) :: (argToTmp arg))
  | Tmp3AddrLabel(l)-> []
  | Tmp3AddrFunCall(s,i,args,dest)-> L.map (argToTmp) args

let findActiveDefs (usedTmps : int list) (currLine : int) reachingDefsTbl =
  L.map (fun temp -> 
    let tmpDefs = H.find reachingDefsTbl temp in
    let activeDefs = H.create 1 in
    let () = H.iter 
      (fun def -> fun linesReached -> 
        try 
          let () = H.find currLine linesReached in
          H.add activeDefs def
        with Not_found -> ()) tmpDefs in
    (temp, activeDefs)) usedTmps

let applyDefs activeDefs (instr : tmp3AddrInstr) in
  match instr with

let applyDefinitions reachingDefsTbl oldProg currLine newProg =
  if currLine = A.length oldProg then (List.rev newProg) else
  let currentInstr = oldProg.(currLine) in
  let usedTmps = findUsedTmps currentInstr in (* list of used tmps in a given line *)
  let activeDefs = findActiveDefs usedTmps currLine reachingDefsTbl in
  let newInstr = applyDefs activeDefs currentInstr in
  applyDefinitions reachingDefsTbl oldProg (currLine + 1) (currentInstr :: newProg)



