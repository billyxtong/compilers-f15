module G = Graph
module H = Hashtbl
module L = List
module A = Array
module S = Set
module M = Core.Std.Map  
open Datatypesv1

let listToString i a = String.concat "" (List.map
            (fun x -> string_of_int(i)^": " ^string_of_int(x) ^ ", ") a @["\n"])

let listArrayToString a = String.concat "" (Array.to_list(Array.mapi listToString a))

let markedLive liveSet line = try let () = H.find liveSet line in true
                          with Not_found -> false

let getDefVars prog line =
    match Array.get prog line with
         Tmp2AddrMov(s, src, Tmp dest) -> dest::[]
       | Tmp2AddrBinop(op, src, Tmp dest) -> dest::[]
       | Tmp2AddrFunCall(fName, args, Some (Tmp dest)) -> dest::[]
       | _ -> []
  

let isDef t prog line =
    match getDefVars prog line with
          t'::[] -> t = t'
        | [] -> false
        | _ -> assert(false)

let isUsed t prog line =
    match Array.get prog line with
         Tmp2AddrMov(s, src, dest) -> (src = TmpLoc (Tmp t))
       | Tmp2AddrBinop(op, src, dest) -> ((src = (TmpLoc (Tmp t)))
                                           || (dest = Tmp t))
       | Tmp2AddrReturn (s, arg) -> (arg = TmpLoc (Tmp t))
       | Tmp2AddrBoolInstr (TmpCmp (s, arg, loc)) ->
              ((arg = (TmpLoc (Tmp t))) || (loc = Tmp t))
       | Tmp2AddrBoolInstr (TmpTest (arg, loc)) ->
              ((arg = (TmpLoc (Tmp t))) || (loc = Tmp t))
       | Tmp2AddrFunCall(fName, args, dest) ->
            List.exists (fun t' -> TmpLoc (Tmp t) = t') args
       | _ -> false

let rec findLiveLinesForTmpRec t prog predsPerLine liveLinesSet currLine =
    if isDef t prog currLine then () else
    (* If it's defined on this line, kill this recursive call *)
    if markedLive liveLinesSet currLine then () else
        (* If we've already declared the var live here, kill this recursive call *)
    (* We know it's live here, because we're marching backwards from a use and
       stopping when we hit a definition *)
    let () = H.add liveLinesSet currLine () in
          (* Then continue backwards to its predecessors *)
    let _ = List.iter (findLiveLinesForTmpRec t prog predsPerLine liveLinesSet)
                 (Array.get predsPerLine currLine) in ()

let addLineToList line () acc = line::acc

let findLiveLinesForTmp t prog predsPerLine =
    let lineNumList = List.mapi (fun i -> fun _ -> i) (Array.to_list prog) in
    (* Start from each line on which t is used *)
    let usedLines = List.filter (isUsed t prog) lineNumList in
    let result = H.create 500 in
    let _ = List.map (findLiveLinesForTmpRec t prog predsPerLine result) usedLines in
    H.fold addLineToList result []

let rec findPredecessors (predecessorsArray : (int list) array)
      (progArray : tmp2AddrInstr array) (lineNum : int) =
  (* Note the -1 here to avoid index-out-of-range! *)
  if lineNum = (A.length progArray) - 1 then ()
  else (match A.get progArray lineNum with
              Tmp2AddrJump(j, l) ->
      (* Billy please comment this *)
                (let () = (match j with
                           JMP_UNCOND -> ()
                         | _ -> (predecessorsArray.(lineNum + 1) <-
                          (lineNum :: predecessorsArray.(lineNum + 1)))) in
                let () = (A.iteri
                            (fun index -> fun instr -> 
                              (match instr with
                                     Tmp2AddrLabel(l') -> 
                                        if l = l'
                                        then predecessorsArray.(index) <-
                                            (lineNum :: predecessorsArray.(index)) 
                                        else ()
                                   | _ -> () )) progArray)
                in findPredecessors predecessorsArray progArray (lineNum + 1))
            | _ -> let () = predecessorsArray.(lineNum + 1) <-
                          (lineNum :: predecessorsArray.(lineNum + 1)) in
                   findPredecessors predecessorsArray progArray (lineNum + 1))

let drawEdgesForTmp interferenceGraph liveTmpsOnLine t =
    L.iter (fun t' -> G.addEdge interferenceGraph (t, t')) liveTmpsOnLine

let drawEdgesForLine prog line liveTmpsOnLine interferenceGraph =
  if line == 0 then () (* no interference on line 0 *) else
  L.iter (drawEdgesForTmp interferenceGraph liveTmpsOnLine)
         (getDefVars prog (line - 1 ))
      
                    
let handleTemp t prog predsPerLine interferenceGraph liveTmpsPerLine =
    let () = G.initVertex interferenceGraph t in
    let liveLinesForT = findLiveLinesForTmp t prog predsPerLine in
    let () = L.iter (fun line -> (liveTmpsPerLine.(line) <-
                                (t:: liveTmpsPerLine.(line)))) liveLinesForT
        in ()
    

let drawGraph (temps : int list) (prog : tmp2AddrInstr array) predsPerLine =
  let liveTmpsPerLine = A.make (A.length prog) [] in
  let interferenceGraph = G.emptyGraph() in
  let () = L.iter (fun t -> handleTemp t prog predsPerLine
                      interferenceGraph liveTmpsPerLine) temps in
  let lineNums = Array.mapi (fun i -> fun _ -> i) prog in
  let () = A.iter (fun lineNum -> drawEdgesForLine prog lineNum
      (liveTmpsPerLine.(lineNum)) interferenceGraph) lineNums in
  interferenceGraph

let analyzeLiveness (instrs : tmp2AddrInstr list) temps =
  let progArray = A.of_list instrs in
  let len = A.length progArray in
  let lineToPredecessorsArray = A.make len [] in
  let () = findPredecessors lineToPredecessorsArray progArray 0 in
  drawGraph temps progArray lineToPredecessorsArray
