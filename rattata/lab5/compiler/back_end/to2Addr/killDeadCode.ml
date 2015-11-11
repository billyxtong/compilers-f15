(* Optimization: Dead Code Elimination
 * We remove 2-address code that can be considered "dead"; i.e. has no effect on the rest of the program.
 * Structure of two-address instructions:
   * moves: t0 <-- t1
   * pointer operations: ____
   * non-pointer operations: ____
   * returns: ret t0
   * jumps: jmp L4
   * masking the upper 32 bits: ??? (halp)
   * bool instructions like test and compare: test t1 t7; cmp t2 t9
   * labels: L4
   * function calls: f(t0, t1); t0 <-- f(t1, t2)
 * Rule for dead code: a statement is "dead code" if it is not NEEDED.
 * Rules for neededness: 
   * If a variable is defined and unneeded after it is defined, the definition is unneeded.
   * If a variable is used in an effect-free statement that is unneeded, it is unneeded at that line.
   *  
 * *)
module A = Array
module L = List
module H = Hashtbl
open LivenessAnalysis

let linesToNeededTempsSet = H.create 10 
(* linesToNeededTempsSet maps each line to a hashtable of temps where it's needed. We add
 * temps to linesToNeededTempsSet based on the rules for neededness in a backward dataflow analysis *)

(* isNeeded checks is a tmp is needed anywhere else in the program. 
 * If it is, returns true; else return false. 
 *
 * This is done by checking our table of temp to needed lines table. If temp maps to an empty list, 
 * then it is not needed on any lines.*)

(* t is a var, and currLine is the successor line of prevLine. This function determines 
 * whether t is needed on prevLine using 3 criteria: 
   * it is needed on currLine, 
   * prevLine is a predecessor of currLine, which is assumed,
   * and t is not defined on prevLine. 
 * If these criteria are met, we add t to the list of temps needed at prevLine. *)

let isNecessary (t : tmp) (currLine)

let needednessRule1 (t : tmp) (currLine : int) =
  let l = H.find currLine in
  if isNecessary t currLine
  then t :: l
  else l

let needednessRule2 (t : tmp) (currLine : int) (prevLine : int) =
  let l = H.find prevLine in
  if (isNeeded t currLine) && not (isDefined t prevLine) 
  then t :: l
  else l


let needednessRule3 (t : tmp) (currLine : int) (prevLine : int) =

let isNeeded (t : tmpLoc) (l : int) =
  match t with
    TmpVar(var) -> 
      try 
        let 
  | TmpDeref(ptr) -> ""




let killDeadInstr (lineToNeededTempsArray : (int list) array) (instr : tmp2AddrInstr) =
  match instr with
    Tmp2AddrMov(s,arg,loc) -> 
      if not isNeeded loc lineToNeededTempsArray then [] else [Tmp2AddrMov(s,arg,loc)] 
  | Tmp2AddrPtrBinop(op,arg,loc) -> ""
  | Tmp2AddrBinop(op,arg,loc) -> ""
  | _ -> [instr] (* dead code will always be a mov or a binop *)

let rec findNeededTemps lineToTempsArray lineToPredecessorsArray currLine funcBodyArray =
  let instr = funcBodyArray.(currLine) in
  match instr with
    Tmp2AddrReturn(s,arg) -> 
      (match arg with
         TmpLoc(TmpVar(t)) -> 
           let neededTempsTable = lineToTempsArray.(currLine) in
           let () = lineToTempsArray.(currLine) <- H.add neededTempsTable t () in

       | TmpLoc(TmpDeref(t)) -> ""
       | _ -> "")

let killDeadCodeInFunctions ((funcName, funcParams, funcBody) : tmp2AddrFunDef) = 
  let funcBodyArray = A.of_list funcBody in 
  (* naturally maps each instr to its line # in the prog *)
  let len = A.length funcBodyArray in 
  let lineToPredecessorsArray = A.make len [] in 
  (* maps each line to a list of its predecessors*)
  let lineToTempsArray = A.make len (H.create 5) in
  (* maps each line to a table of needed temps at that line *)
  let () = findPredecessors lineToPredecessorsArray funcBodyArray in
  (* populates lineToPredecessorsArray *)
  let newFuncBodyArray = A.map (killDeadInstr lineToPredecessorsArray) funcBodyArray in
  (* kills dead instrs *)
  let newFuncBody = L.flatten (A.to_list newFuncBodyArray) in
  (* changes result back into a list of 2-address instrs *)
  (funcName, funcParams, newFuncBody)

let killDeadCode (prog : tmp2AddrProg) =
  L.map killDeadCodeInFunctions tmp2AddrProg
