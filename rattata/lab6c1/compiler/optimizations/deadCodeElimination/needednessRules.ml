module L = List
module H = Hashtbl
open LivenessAnalysis
open NecessityRules
open Datatypesv1
open PrintDatatypes

let rec neededR1 currLine tempsToLines prog =
  if (currLine = (-1)) then () else
  let necessaryTempsOnCurrLine = getNecessaryTemps currLine prog in
  let () = L.iter
    (fun temp -> 
      let lines = 
        (try H.find tempsToLines temp 
         with Not_found -> 
           let () = print_string (tmp2AddrInstrToString (prog.(currLine))) in
           let () = print_string ("missing t" ^ (string_of_int temp) ^ "\n") in
           raise (Failure "neededR1\n")) 
         in H.add lines currLine ()) necessaryTempsOnCurrLine in
  neededR1 (currLine - 1) tempsToLines prog

let locToTmp loc =
  match loc with
    TmpVar(Tmp t) -> t
   |TmpDeref(Tmp t) -> t

let argToTmp arg =
  match arg with
    TmpConst _ -> [] 
    (* we give this form so we don't have to do additional cases*)
   |TmpLoc(TmpVar(Tmp t)) -> [t]
   |TmpLoc(TmpDeref(Tmp t)) -> [t]

let helperR3 tempsToLines prog currLine =
  match prog.(currLine) with
    Tmp2AddrMov(s,arg,loc)->
      let dest = locToTmp loc in
      let src = argToTmp arg in
      if (H.length (try H.find tempsToLines dest with Not_found -> raise (Failure "helperR3 mov\n")) > 0) then src else []
   |Tmp2AddrBinop(op,arg,loc)->
      if (op = FAKEDIV || op = FAKEMOD) then [] else 
      let dest = locToTmp loc in
      let src = argToTmp arg in
      if (H.length (try H.find tempsToLines dest with Not_found -> raise (Failure "helperR3 binop\n")) > 0) 
      then dest :: src else []
   |Tmp2AddrPtrBinop(op,arg,loc)-> [locToTmp loc] 
   (* Ben: since the args are all constants and the locs are all pointer vars,
    * the loc in every ptr binop is needed, right? *)
   |_->[]

let rec neededR3 currLine tempsToLines prog =
  if (currLine = (-1)) then () else
  let neededFromApplyingR3 = helperR3 tempsToLines prog currLine in
  let () = L.iter (fun temp -> let lines = (try H.find tempsToLines temp with Not_found -> raise (Failure "neededR3\n")) in
                    H.add lines currLine ()) neededFromApplyingR3 in
  neededR3 (currLine - 1) tempsToLines prog

let rec helperR2 temp neededLines prog predsPerLine currLine =
  let preds = predsPerLine.(currLine) in
  L.iter(fun pred -> 
      try 
        H.find neededLines pred 
      with
       Not_found ->
           if isDef temp prog pred then () else
           let () = H.add neededLines pred () in
           helperR2 temp neededLines prog predsPerLine pred) preds

let neededR2 predsPerLine tempsToLines prog =
  H.iter(fun temp -> fun lines -> 
    (H.iter (fun line -> fun _ -> helperR2 temp lines prog predsPerLine line) lines)) tempsToLines


