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

let isNeeded (lineNum : int) (tLoc : tmpLoc) 
             (linesToNeededTempsArray : (int list) array) =
  match tLoc with
    TmpVar(t) ->
      if (L.exists (fun temp -> t = temp) (linesToNeededTempsArray.(lineNum)))
      then true else false
   |TmpDeref(t) ->
      if (L.exists (fun temp -> t = temp) (linesToNeededTempsArray.(lineNum)))
      then true else false

let killDeadInstr (lineNum : int) (instr : tmp2AddrInstr)
                  (lineToNeededTempsArray : (int list) array) =
  match instr with
    Tmp2AddrMov(s,arg,loc) ->
      if not isNeeded lineNum loc lineToNeededTempsArray
      then [] else [Tmp2AddrMov(s,arg,loc)]
  | Tmp2AddrPtrBinop(op,arg,loc) ->
      if not isNeeded lineNum loc lineToNeededTempsArray
      then [] else [Tmp2AddrPtrBinop(op,arg,loc)]
  | Tmp2AddrBinop(op,arg,loc) ->
      (match op with
         FAKEDIV-> [Tmp2AddrBinop(op,arg,loc)]
        |FAKEMOD-> [Tmp2AddrBinop(op,arg,loc)]
        |RSHIFT-> [Tmp2AddrBinop(op,arg,loc)]
        |LSHIFT-> [Tmp2AddrBinop(op,arg,loc)]
        |_ -> if not isNeeded lineNum loc lineToNeededTempsArray
              then [] else [Tmp2AddrBinop(op,arg,loc)])
  | _ -> [instr] (* dead code will always be a mov or a binop *)

let killDeadCodeInFunctions ((funcName, funcParams, funcBody) : tmp2AddrFunDef) =
  let funcBodyArray = A.of_list funcBody in
  (* naturally maps each instr to its line # in the prog *)
  let len = A.length funcBodyArray in
  let lineToPredecessorsArray = A.make len [] in
  (* maps each line to a list of its predecessors *)
  let () = findPredecessors lineToPredecessorsArray funcBodyArray in
  (* populates lineToPredecessorsArray *)
  let lineToTempsArray = A.make len [] in
  (* maps each line to a list of needed temps at that line *)
  let () = getNeededTemps funcBodyArray lineToPredecessorsArray
                          lineToTempsArray (len - 1) (len - 1) in
  (* populates lineToTempsArray with the temps needed at each line *)
  let newFuncBodyArray = A.mapi (fun lineNum -> fun instr ->
              killDeadInstr lineNum instr lineToTempsArray) funcBodyArray in
  (* kills dead instrs *)
  let newFuncBody = L.flatten (A.to_list newFuncBodyArray) in
  (* changes result back into a list of 2-address instrs *)
  (funcName, funcParams, newFuncBody)

let killDeadCode (prog : tmp2AddrProg) =
  L.map killDeadCodeInFunctions tmp2AddrProg
