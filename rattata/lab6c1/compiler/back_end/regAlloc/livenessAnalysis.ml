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
         Tmp2AddrMov(opSize, src, TmpVar (Tmp dest)) -> dest::[]
       | Tmp2AddrBinop(op, src, TmpVar (Tmp dest)) -> dest::[]
       | Tmp2AddrPtrBinop(op, src, TmpVar (Tmp dest)) -> dest::[]
       | Tmp2AddrFunCall(opSize, fName, args, Some (TmpVar (Tmp dest))) -> dest::[]
       | Tmp2AddrFunPtrCall(opSize, tLoc, args, Some (TmpVar (Tmp dest))) -> dest::[]
       | Tmp2AddrGetFunAddress (fName, TmpVar (Tmp dest)) -> dest :: []
       (* it's not defined if the dest is its deref! *)
       | Tmp2AddrMov(opSize, src, TmpDeref (Tmp dest)) -> []
       | Tmp2AddrBinop(op, src, TmpDeref (Tmp dest)) -> []
       | Tmp2AddrPtrBinop(op, src, TmpDeref (Tmp dest)) -> []
       | Tmp2AddrFunCall(opSize, fName, args, Some (TmpDeref (Tmp dest))) -> []
       | Tmp2AddrReturn _ -> []
       | Tmp2AddrJump _ -> []
       | Tmp2AddrLabel _ -> []
       | Tmp2AddrBoolInstr _ -> []
       | Tmp2AddrFunCall(opSize, fName, args, None) -> []
       | Tmp2AddrFunPtrCall(opSize, tLoc, args, None) -> []
       | Tmp2AddrFunPtrCall(opSize, tLoc, args, Some (TmpDeref (Tmp dest))) -> []
       | Tmp2AddrGetFunAddress (fName, TmpDeref (Tmp dest)) -> []
       | Tmp2AddrMaskUpper (Tmp t) -> t::[]
  

let isDef t prog line =
    match getDefVars prog line with
          t'::[] -> t = t'
        | [] -> false
        | _ -> assert(false)

let isUsedInTmpArg t = function
    TmpLoc (TmpVar (Tmp t')) -> t = t'
  | TmpLoc (TmpDeref (Tmp t')) -> t = t'
  | _ -> false

let isUsedInTmpLoc t = function
    TmpVar (Tmp t') -> t = t'
  | TmpDeref (Tmp t') -> t = t'
    

let isUsed t prog line =
    match Array.get prog line with
         Tmp2AddrMov(s, src, dest) ->
            (* Mov is special: it's used if the dest is a deref of it,
               but not it is the dest! *)
            (isUsedInTmpArg t src || dest = TmpDeref (Tmp t))
       | Tmp2AddrBinop(op, src, dest) ->
            (isUsedInTmpArg t src) || (isUsedInTmpLoc t dest)
       | Tmp2AddrPtrBinop(op, src, dest) ->
            (isUsedInTmpArg t src) || (isUsedInTmpLoc t dest)
       | Tmp2AddrReturn (s, arg) -> isUsedInTmpArg t arg
       | Tmp2AddrBoolInstr (TmpCmp (s, arg, loc)) ->
            (isUsedInTmpArg t arg) || (isUsedInTmpLoc t loc)
       | Tmp2AddrBoolInstr (TmpTest (arg, loc)) ->
            (isUsedInTmpArg t arg) || (isUsedInTmpLoc t loc)
       | Tmp2AddrFunCall(retSize, fName, args, dest) ->
            List.exists (fun arg -> isUsedInTmpArg t arg) args ||
            (* same deal as mov *)
            dest = Some (TmpDeref (Tmp t))
       | Tmp2AddrFunPtrCall(retSize, tLoc, args, dest) ->
            List.exists (fun arg -> isUsedInTmpArg t arg) args ||
            isUsedInTmpLoc t tLoc ||
            (* same deal as mov *)
            dest = Some (TmpDeref (Tmp t))
       | Tmp2AddrMaskUpper (Tmp t') -> t = t'
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

let drawEdgesForLine paramTmps prog line liveTmpsOnLine interferenceGraph =
  if line == 0 then (* all params are considered to be defined on line -1 *)
    L.iter (drawEdgesForTmp interferenceGraph liveTmpsOnLine) paramTmps

  else
  L.iter (drawEdgesForTmp interferenceGraph liveTmpsOnLine)
         (getDefVars prog (line - 1 ))
      
                    
let handleTemp t prog predsPerLine interferenceGraph liveTmpsPerLine =
    let () = G.initVertex interferenceGraph t in
    let liveLinesForT = findLiveLinesForTmp t prog predsPerLine in
    let () = L.iter (fun line -> (liveTmpsPerLine.(line) <-
                                (t:: liveTmpsPerLine.(line)))) liveLinesForT
        in ()

let rec makeAllParamsInterfere graph = function
    [] -> ()
  | param::rest ->
       let () = G.initVertex graph param in
       let () = L.iter (fun p -> G.addEdge graph (param, p)) rest in
       makeAllParamsInterfere graph rest

(* third and fourth params params are edx and ecx which are reserved for
   division/shifts *)
let make3rdAnd4thParamsInterfereWithEverything paramTmps progTmps graph =
    match paramTmps with
         p1::p2::p3::p4::rest ->
               let () = L.iter (fun t -> G.addEdge graph (p3, t)) progTmps in
               L.iter (fun t -> G.addEdge graph (p4, t)) progTmps
       | p1::p2::p3::[] -> L.iter (fun t -> G.addEdge graph (p3, t)) progTmps 
       | _ -> ()

let rec makeAllPairsInterfere graph tmpList1 tmpList2 =
    match tmpList1 with
         [] -> ()
       | t1::t1rest ->
            let () = L.iter (fun t -> G.addEdge graph(t, t1)) tmpList2 in

            makeAllPairsInterfere graph t1rest tmpList2        

let getArgsThatAreTmps args =
    let removeConsts = List.filter
           (fun arg -> match arg with TmpConst _ -> false | TmpLoc _ -> true) args in
    let undoDerefs = List.map (fun (TmpLoc tLoc) -> match tLoc with
                                         TmpVar (Tmp t) -> t
                                       | TmpDeref (Tmp t) -> t) removeConsts
    in undoDerefs

let makeArgsInterferePerCall graph paramTmps = function
    Tmp2AddrFunCall (retSize, fName, args, dest) ->
        let argsThatAreTmps = getArgsThatAreTmps args in
        makeAllPairsInterfere graph paramTmps argsThatAreTmps
  | _ -> ()


(* not sure if I actually need to do this...maybe it's already taken care of *)
let makeFunPtrsInferenceWithParams graph paramTmps = function
    Tmp2AddrFunPtrCall (retSize, TmpVar (Tmp fPtr), args, dest) ->
        makeAllPairsInterfere graph paramTmps [fPtr]
  | Tmp2AddrFunPtrCall (retSize, TmpDeref (Tmp fPtr), args, dest) ->
        makeAllPairsInterfere graph paramTmps [fPtr]
  | _ -> ()

(* any time we make a function call inside this function, those args interfere
   with the params of the caller *)
(* the tmp that holds the pointer for a function pointer infereres with all params *) 
let handleOtherSpecialFunctionInteference graph paramTmps instrs =
    let () = L.iter (makeArgsInterferePerCall graph paramTmps) instrs in
    let () = L.iter (makeFunPtrsInferenceWithParams graph paramTmps) instrs in
    ()
    

let drawGraph (temps : int list) (prog : tmp2AddrInstr array) predsPerLine
     paramTmps =
     (* here's the deal: pretend we always have at least 6 params. Suppose we have
        two params. Then create 4 new tmps, which we will be live nowhere. But,
        it is useful because they will be assigned to various registers, and we
        can interfere them in args to functions that we call. It's basically
        a hacky way of doing precoloring. *)
  let liveTmpsPerLine = A.make (A.length prog) [] in
  let interferenceGraph = G.emptyGraph() in
  let () = make3rdAnd4thParamsInterfereWithEverything paramTmps temps
        interferenceGraph in
  let () = makeAllParamsInterfere interferenceGraph paramTmps in
  let () = L.iter (fun t -> handleTemp t prog predsPerLine
                      interferenceGraph liveTmpsPerLine) (temps @ paramTmps) in
  let lineNums = Array.mapi (fun i -> fun _ -> i) prog in
  let () = A.iter (fun lineNum -> drawEdgesForLine paramTmps prog lineNum
      (liveTmpsPerLine.(lineNum)) interferenceGraph) lineNums in
  interferenceGraph

let analyzeLiveness (instrs : tmp2AddrInstr list) progTmps paramTmps =
  let progArray = A.of_list instrs in
  let len = A.length progArray in
  let lineToPredecessorsArray = A.make len [] in
  let () = findPredecessors lineToPredecessorsArray progArray 0 in
  let resultGraph = drawGraph progTmps progArray lineToPredecessorsArray paramTmps in
  let () = handleOtherSpecialFunctionInteference resultGraph paramTmps instrs in
  (* let () = makeAllPairsInterfere resultGraph progTmps paramTmps in *)
  resultGraph
