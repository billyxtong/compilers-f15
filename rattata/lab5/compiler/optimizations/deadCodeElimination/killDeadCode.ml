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
open NeedednessRules
open LivenessAnalysis
open Datatypesv1

(* creates an array where each index holds an array of its successors *)
let findSuccessors (lineToPredecessorsArray : (int list) array) : ((int list) array) =
  let successorsArray = A.make (A.length lineToPredecessorsArray) [] in
  let () = A.iteri 
  (fun index -> fun predecessors -> L.iter 
      (fun predecessor -> (successorsArray.(predecessor) <- (index :: (successorsArray.(predecessor))))) predecessors) 
  lineToPredecessorsArray in
  successorsArray

(* sees if a temp is needed at a given lineNum by checking its needed temps *)
let isNeeded (tLoc : tmpLoc) (linesToNeededTempsArray : (int list) array) 
              (lineNum : int) =
  match tLoc with
    TmpVar(Tmp t) -> (L.exists (fun temp -> t = temp) (linesToNeededTempsArray.(lineNum)))
   |TmpDeref(Tmp t) -> (L.exists (fun temp -> t = temp) (linesToNeededTempsArray.(lineNum)))

let killDeadInstr (lineNum : int) (instr : tmp2AddrInstr)
                  (lineToNeededTempsArray : (int list) array) 
                  (lineToSuccessorsArray : (int list) array) =
  let successors = lineToSuccessorsArray.(lineNum) in
  match instr with
    Tmp2AddrMov(s,arg,loc) ->
      (match L.filter (fun l -> isNeeded loc lineToNeededTempsArray l) successors with
        [] -> []
       |_ -> [Tmp2AddrMov(s,arg,loc)])
      (* note: a declared temp can be killed only if it is not needed in any of the successors *) 
  | Tmp2AddrPtrBinop(op,arg,loc) ->
      (match L.filter (fun l -> isNeeded loc lineToNeededTempsArray l) successors with
        [] -> []
       |_ -> [Tmp2AddrPtrBinop(op,arg,loc)])
  | Tmp2AddrBinop(op,arg,loc) ->
      (match op with
         FAKEDIV-> [Tmp2AddrBinop(op,arg,loc)]
        |FAKEMOD-> [Tmp2AddrBinop(op,arg,loc)]
        |RSHIFT-> [Tmp2AddrBinop(op,arg,loc)]
        |LSHIFT-> [Tmp2AddrBinop(op,arg,loc)]
        |_ -> 
            (match L.filter (fun l -> isNeeded loc lineToNeededTempsArray l) successors with
               [] -> []
              |_ ->[Tmp2AddrBinop(op,arg,loc)]))
  | _ -> [instr] (* dead code will always be a mov or a binop *)

let killDeadCodeInFunctions (Tmp2AddrFunDef(funcName, funcParams, funcBody) : tmp2AddrFunDef) : tmp2AddrFunDef =
  let funcBodyArray = A.of_list funcBody in
  (* naturally maps each instr to its line # in the prog *)
  let len = A.length funcBodyArray in
  let lineToPredecessorsArray = A.make len [] in
  (* maps each line to a list of its predecessors *)
  let () = findPredecessors lineToPredecessorsArray funcBodyArray 0 in
  (* populates lineToPredecessorsArray *)
  let lineToTempsArray = A.make len [] in
  (* maps each line to a list of needed temps at that line *)
  let () = getNeededTemps funcBodyArray lineToPredecessorsArray
                          lineToTempsArray (len - 1) (len - 1) in
  (* populates lineToTempsArray with the temps needed at each line *)
  let lineToSuccessorsArray = findSuccessors lineToPredecessorsArray in
  (* gives us an array mapping line numbers to their successors *)
  let newFuncBodyArray = A.mapi (fun lineNum -> fun instr ->
              killDeadInstr lineNum instr lineToTempsArray lineToSuccessorsArray) funcBodyArray in
  (* kills dead instrs *)
  let newFuncBody = L.flatten (A.to_list newFuncBodyArray) in
  (* changes result back into a list of 2-address instrs *)
  Tmp2AddrFunDef(funcName, funcParams, newFuncBody)

let killDeadCode (prog : tmp2AddrProg) : tmp2AddrProg =
  L.map killDeadCodeInFunctions prog
