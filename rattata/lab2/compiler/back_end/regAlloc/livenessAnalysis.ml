module G = Graph
module H = Hashtbl
module L = List
module A = Array
module S = Set
module M = Core.Std.Map  
open Datatypesv1

let listToString i a = String.concat "" (List.map (fun x -> string_of_int(i)^": " ^string_of_int(x) ^ ", ") a @["\n"])

let listArrayToString a = String.concat "" (Array.to_list(Array.mapi listToString a))

let isLive liveSet line = try let () = H.find liveSet line in true
                          with Not_found -> false

let isDef t prog line =
    match Array.get prog line with
         Tmp2AddrMov(src, dest) -> (dest = Tmp t)
       | Tmp2AddrBinop(op, src, dest) -> (dest = Tmp t)
       | _ -> false

let isUsed t prog line =
    match Array.get prog line with
         Tmp2AddrMov(src, dest) -> (src = TmpLoc (Tmp t))
       | Tmp2AddrBinop(op, src, dest) -> ((src = (TmpLoc (Tmp t)))
                                           || (dest = Tmp t))
       | Tmp2AddrReturn arg -> (arg = TmpLoc (Tmp t))
       | Tmp2AddrBoolInstr (TmpCmp (arg, loc)) ->
              ((arg = (TmpLoc (Tmp t))) || (loc = Tmp t))
       | Tmp2AddrBoolInstr (TmpTest (arg, loc)) ->
              ((arg = (TmpLoc (Tmp t))) || (loc = Tmp t))
       | _ -> false

let seen seenLinesOnThisCall line = match M.find seenLinesOnThisCall line with None -> false | Some () -> true

let rec findLiveLinesRec t prog seenLinesOnThisCall predsPerLine liveLinesSet seenLinesOverall succLine currLine =
    let () = Array.set seenLinesOverall currLine true in
    let () = (if isDef t prog currLine then H.replace liveLinesSet succLine ()) in
    if seen seenLinesOnThisCall currLine then (let () = H.replace liveLinesSet succLine () in liveLinesSet) else
            (* if it's defined here, we're done. Also, if it's defined on this line it's
               always live on the next line,
               I think. This is to deal with temps that are assigned but
               never used *)
    let newSeenLines = M.add seenLinesOnThisCall currLine () in
    try (let () = H.find liveLinesSet currLine in liveLinesSet)
        (* If we've already declared the var live here, we're done *)
    with Not_found -> 
       (* Add it to liveLinesSet if it's live, otherwise do nothing. *)
         let () =
                (if (isLive liveLinesSet succLine && not (isDef t prog currLine))
                  || isUsed t prog currLine (* It's live on this line *)
                then
                  (* let () = (if t=2 then print_string("live!\n")) in *)
                  (* let () = (if t= 2 && (isLive liveLinesSet succLine) then print_string("used!\n")) in *)
                  H.replace liveLinesSet currLine ()) in
          (* Then continue backwards to its predecessors *)
            let _ =
            List.map (findLiveLinesRec t prog newSeenLines predsPerLine
                              liveLinesSet seenLinesOverall currLine)
                 (Array.get predsPerLine currLine) in liveLinesSet

let addLineToList line () acc = line::acc

let allLinesTouched touchedLines = Array.fold_left
      (fun touch1 -> fun touch2 -> touch1 && touch2) true touchedLines

let rec findLiveLinesButCheckThatEveryLineIsTouched t prog predsPerLine result startLine seenLinesOverall =
    let seenLinesOnThisCall = Core.Std.Int.Map.empty in
    (* let () = print_string("line: " ^ string_of_int(startLine) ^ "\n") in *)
    let liveLinesSet = findLiveLinesRec t prog seenLinesOnThisCall predsPerLine
                       result seenLinesOverall startLine startLine  in
    if allLinesTouched seenLinesOverall then liveLinesSet
    else findLiveLinesButCheckThatEveryLineIsTouched t prog predsPerLine result (startLine - 1)
        seenLinesOverall
    
    

let findLiveLines t prog predsPerLine =
    let startLine = (Array.length prog) - 1 in
    let result = H.create 500 in
    let seenLinesOverall = Array.make (Array.length prog) false in
    let liveLinesSet = findLiveLinesButCheckThatEveryLineIsTouched
        t prog predsPerLine result startLine seenLinesOverall in
    (* let () = (if t=2 then print_string(listArrayToString predsPerLine)) in *)
    H.fold addLineToList liveLinesSet []
    (* let () = (if true then print_string(listToString t r) ) in r *)


let rec findPredecessors (predecessorsArray : (int list) array)
      (progArray : tmp2AddrInstr array) (lineNum : int) =
  (* Note the -1 here to avoid index-out-of-range! *)
  if lineNum = (A.length progArray) - 1 then ()
  else (match A.get progArray lineNum with
              Tmp2AddrJump(j, l) -> 
                (let () = (match j with
                                JMP_UNCOND -> ()
                              | _ -> (A.set predecessorsArray (lineNum + 1) (lineNum ::
                                                                           (A.get predecessorsArray (lineNum + 1))))) in
                let () = (A.iteri
                            (fun index -> fun instr -> 
                              (match instr with
                                     Tmp2AddrLabel(l') -> 
                                        if l = l'
                                           (* why is this index + 1?? *)
                                        then predecessorsArray.(index) <- (lineNum :: predecessorsArray.(index)) 
                                        else ()
                                   | _ -> () )) progArray)
                in findPredecessors predecessorsArray progArray (lineNum + 1))
            | _ -> let () = (A.set predecessorsArray (lineNum + 1) (lineNum ::
                                                             (A.get predecessorsArray (lineNum + 1)))) in
                   findPredecessors predecessorsArray progArray (lineNum + 1))

let rec drawAllEdges (line : int list) interferenceGraph =
  match line with
        [] -> ()
      | temp :: line' -> (let () = L.iter (fun t -> G.addEdge interferenceGraph (temp, t)) line' in
                          drawAllEdges line' interferenceGraph)

let handleTemp t prog predsPerLine interferenceGraph liveTmpsPerLine =
    let () = G.initVertex interferenceGraph t in
    let liveLinesForT = findLiveLines t prog predsPerLine in
    let () = L.iter (fun line -> (A.set liveTmpsPerLine line (t::A.get liveTmpsPerLine line))) liveLinesForT
        in ()
    

let drawGraph (temps : int list) (prog : tmp2AddrInstr array) predsPerLine =
  let liveTmpsPerLine = A.make (A.length prog) [] in
  let interferenceGraph = G.emptyGraph() in
  let () = L.iter (fun t -> handleTemp t prog predsPerLine interferenceGraph liveTmpsPerLine) temps in
  (* let () = L.iter (fun t -> (let () = G.initVertex interferenceGraph t in *)
  (*                            let liveLines = findLiveLines t prog predsPerLine in *)
  (*                            L.iter (fun l -> (A.set liveTmpsPerLine l (t::liveTmpsPerLine.(l))) liveLines) temps in *)
  let () = A.iter (fun line -> drawAllEdges line interferenceGraph) liveTmpsPerLine in
  interferenceGraph

let analyzeLiveness (prog : tmp2AddrProg) temps =
  let progArray = A.of_list prog in
  let len = A.length progArray in
  let lineToPredecessorsArray = A.make len [] in
  let () = findPredecessors lineToPredecessorsArray progArray 0 in
  drawGraph temps progArray lineToPredecessorsArray
  
