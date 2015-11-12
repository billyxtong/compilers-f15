module L = List
open LivenessAnalysis

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

  L.filter (fun x -> if not (isDef x indexedProg currLine) 
                     then true else false) (lineToNeededTempsArray.(succLine))

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
         () -> "")
   |Tmp2AddrBinop(op,arg,loc)->""
   |_->[]
