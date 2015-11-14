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

let rec findAllTemps instrList tempSet =
  match instrList with
    [] -> tempSet
   |instr :: instrs ->
       (match instr with
          Tmp2AddrMov(s,arg,loc)->
            let () = L.iter (fun t -> H.replace tempSet t (H.create 5)) (locToTmp loc :: argToTmp arg) in
            findAllTemps instrs tempSet
         |Tmp2AddrBinop(op,arg,loc)->
            let () = L.iter (fun t -> H.replace tempSet t (H.create 5)) (locToTmp loc :: argToTmp arg) in
            findAllTemps instrs tempSet
         |Tmp2AddrPtrBinop(op,arg,loc)->
            let () = L.iter (fun t -> H.replace tempSet t (H.create 5)) (locToTmp loc :: argToTmp arg) in
            findAllTemps instrs tempSet
         |Tmp2AddrFunCall(s,i,args,Some(TmpVar(Tmp dest)))->
            let () = H.replace tempSet dest (H.create 5) in
            findAllTemps instrs tempSet
         |Tmp2AddrFunCall(s,i,args,Some(TmpDeref(Tmp dest)))->
            let () = H.replace tempSet dest (H.create 5) in
            findAllTemps instrs tempSet
         |Tmp2AddrMaskUpper(Tmp t) -> 
            let () = H.replace tempSet t (H.create 5) in
            findAllTemps instrs tempSet
         |Tmp2AddrReturn(s,arg) ->
            let () = L.iter (fun t -> H.replace tempSet t (H.create 5)) (argToTmp arg) in
            findAllTemps instrs tempSet
         |_ -> findAllTemps instrs tempSet)


let findNeededLinesForTemps predsPerLine tempsToLines prog =
  let () = neededR1 ((A.length prog) - 1) tempsToLines prog in
  let () = neededR3 ((A.length prog) - 1) tempsToLines prog in
  neededR2 predsPerLine tempsToLines prog

let killDeadInstr currLine prog tempsToLines =
  match getDefVars prog currLine with
    [t] -> if (H.length (try H.find tempsToLines t with Not_found -> raise (Failure "killDeadInstr")) > 0) then [prog.(currLine)] else []
   |[] -> [prog.(currLine)]

let killDeadCodeInFunctions (Tmp2AddrFunDef(funcName, funcParams, funcBody)) =
  let tempsToLines = findAllTemps funcBody (H.create 5) in
  let () = L.iter(fun (Tmp t, _) -> H.replace tempsToLines t (H.create 5)) funcParams in
  let funcBodyArray = A.of_list funcBody in
  let predsPerLine = A.make (A.length funcBodyArray) [] in
  let () = findPredecessors predsPerLine funcBodyArray 0 in
  let () = findNeededLinesForTemps predsPerLine tempsToLines funcBodyArray in
  let newFuncBody = 
    L.flatten(A.to_list(
      A.mapi (fun lineNum -> fun _ -> killDeadInstr lineNum funcBodyArray 
                                      tempsToLines) funcBodyArray)) in
  Tmp2AddrFunDef(funcName, funcParams, newFuncBody)

let killDeadCode (prog : tmp2AddrProg) : tmp2AddrProg =
  L.map killDeadCodeInFunctions prog
