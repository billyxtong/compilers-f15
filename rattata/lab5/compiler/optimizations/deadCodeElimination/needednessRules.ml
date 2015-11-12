module L = List
open LivenessAnalysis
open NecessityRules
open Datatypesv1

(* N1: all necessary temps are also neeeded *)
let needednessRule1 (currLine : int) (indexedProg : tmp2AddrInstr array) = 
  getNecessaryTemps currLine indexedProg

(* N2: A temp that is needed in succLine is needed in currLine
 * if it isn't defined in currLine. 
 *
 * Note: this is the primary rule for propogating neededness up. *)
let needednessRule2 (lineToPredecessorsArray : (int list) array) 
                    (indexedProg : tmp2AddrInstr array) 
                    (lineToNeededTempsArray : (tmp list) array)
                    (succLine : int) (currLine : int) =

  L.filter (fun x -> not (isDef x indexedProg currLine)) (lineToNeededTempsArray.(succLine))

(* N3: Assume l: x <- y + z, where "+" is an effect-free operator.
 * If x is needed in l's successor l', then y and z are both needed in l.
 * If x is not needed in l', then y and z are NOT needed in l'. *)
let needednessRule3 (lineToPredecessorsArray : (int list) array) 
                    (indexedProg : tmp2AddrInstr array) 
                    (lineToNeededTempsArray : (tmp list) array)
                    (succLine : int) (currLine : int) =
  match (indexedProg.(currLine)) with
    Tmp2AddrMov(s,arg,loc)->
      (match (arg,loc) with
         (TmpConst(_),_)->[]
        |(TmpLoc(TmpVar(t1)),TmpVar(t2))->
            if 
            L.exists (fun t -> t = t2) (lineToNeededTempsArray.(succLine))
            then [t1] else []
        |(TmpLoc(TmpDeref(t1)),TmpVar(t2))->
            if 
            L.exists (fun t -> t = t2) (lineToNeededTempsArray.(succLine))
            then [t1] else []
        |(TmpLoc(TmpVar(t1)),TmpDeref(t2))->
            if 
            L.exists (fun t -> t = t2) (lineToNeededTempsArray.(succLine))
            then [t1] else []
        |(TmpLoc(TmpDeref(t1)),TmpDeref(t2))->
            if 
            L.exists (fun t -> t = t2) (lineToNeededTempsArray.(succLine))
            then [t1] else [])
   |Tmp2AddrPtrBinop(op,arg,loc)->
      (match (arg,loc) with
         (TmpConst(_),TmpVar(t1)) -> 
            if 
            L.exists (fun t -> t = t1) (lineToNeededTempsArray.(succLine))
            then [t1] else []
        |(TmpConst(_),TmpDeref(t1)) -> 
            if 
            L.exists (fun t -> t = t1) (lineToNeededTempsArray.(succLine))
            then [t1] else [])
            (* are any of the below cases necessary?
        |(TmpLoc(TmpVar(t1)),TmpVar(t2))->
            if 
            L.exists (fun t -> t = t2) (lineToNeededTempsArray.(succLine))
            then [t1] else []
        |(TmpLoc(TmpDeref(t1)),TmpVar(t2))->
            if 
            L.exists (fun t -> t = t2) (lineToNeededTempsArray.(succLine))
            then [t1] else []
        |(TmpLoc(TmpVar(t1)),TmpDeref(t2))->
            if 
            L.exists (fun t -> t = t2) (lineToNeededTempsArray.(succLine))
            then [t1] else []
        |(TmpLoc(TmpDeref(t1)),TmpDeref(t2))->
            if 
            L.exists (fun t -> t = t2) (lineToNeededTempsArray.(succLine))
            then [t1] else []) *)
   |Tmp2AddrBinop(op,arg,loc)-> (* 2 address binops always include the loc *)
       (match op with
          FAKEDIV -> [] (* all of these ops have already been handled in necessity *)
         |FAKEMOD -> []
         |RSHIFT -> []
         |LSHIFT -> []
         |_ -> (* effect-free ops! *)
          (match (arg,loc) with
            (TmpLoc(TmpVar(t1)),TmpVar(t2))-> 
              if 
              L.exists (fun t -> t = t2) (lineToNeededTempsArray.(succLine))
              then [t1;t2] else []
           |(TmpLoc(TmpVar(t1)),TmpDeref(t2))-> 
              if 
              L.exists (fun t -> t = t2) (lineToNeededTempsArray.(succLine))
              then [t1;t2] else []
           |(TmpLoc(TmpDeref(t1)),TmpVar(t2))-> 
              if 
              L.exists (fun t -> t = t2) (lineToNeededTempsArray.(succLine))
              then [t1;t2] else []
           |(TmpLoc(TmpDeref(t1)),TmpDeref(t2))-> 
              if 
              L.exists (fun t -> t = t2) (lineToNeededTempsArray.(succLine))
              then [t1;t2] else []
           |(_,TmpVar(t1))->
              if 
              L.exists (fun t -> t = t1) (lineToNeededTempsArray.(succLine))
              then [t1] else []
           |(_,TmpDeref(t1))->
              if 
              L.exists (fun t -> t = t1) (lineToNeededTempsArray.(succLine))
              then [t1] else []))
   |_->[]

let rec getNeededTemps (indexedProg : tmp2AddrInstr array) 
                       (linesToPredecessorsArray : (int list) array)
                       (linesToNeededTempsArray : (tmp list) array)
                       (succLine : int) (currLine : int) =

  let neededFromRule1 = needednessRule1 currLine indexedProg in
  let neededFromRule2 = needednessRule2 lineToPredecessorsArray indexProg succLine currLine in
  let neededFromRule3 = needednessRule3 lineToPredecessorsArray indexProg succLine currLine in
  let () = lineToNeededTempsArray.(currLine) <- (neededFromRule1 @ neededFromRule2 @ neededFromRule3) in
  L.iter (getNeededTemps indexedProg linesToPredecessorsArray linesToNeededTempsArray currLine)
  (linesToPredecessorsArray.(currLine))

