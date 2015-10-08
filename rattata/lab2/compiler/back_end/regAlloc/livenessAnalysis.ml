module G = Graph
module H = Hashtbl
module L = List
module A = Array
module S = Set
open Datatypesv1

let findPredecessors (predecessorsArray : (int list) array) 
      (progArray : tmp2AddrInstr array) (lineNum : int) =
  if lineNum = (A.length progArray) then ()
  else (match progArray.(lineNum) with
              Tmp2AddrJumpInstr(j, l) -> 
                let () = (match j with
                                JMP_UNCOND -> ()
                              | _ -> predecessorsArray.(lineNum + 1) <- (l :: predecessorsArray.(lineNum + 1))) in
                let () = (A.iteri
                            (fun index -> fun instr -> 
                              (match instr with
                                     Tmp2AddrLabel(l') -> 
                                        if l = l' 
                                        then predecessorsArray.(index) <- (l :: predecessorsArray.(index)) 
                                        else ()
                                   | _ -> () )) progArray) 
                in findPredecessors predecessorsArray progArray (lineNum + 1)
            | _ -> let () = (predecessorsArray.(lineNum + 1) <- (lineNum :: predecessorsArray.(lineNum + 1))) in
                   findPredecessors predecessorsArray progArray (lineNum + 1))

let drawAllEdges (line : int list) interferenceGraph =
  match line with
        [] -> ()
      | temp :: line' -> (let () = L.iter (fun t -> G.addEdge interferenceGraph (temp, t)) line' in
                          drawAllEdges line' interferenceGraph)

let drawGraph (temps : tmp list) (prog : (lineNum, instr) array)=
  let liveTmpsPerLine = A.make (A.length prog) [] in
  let interferenceGraph = G.emptyGraph() in
  let () = L.iter (fun t -> (let () = initVertex G t in
                             let liveLines = findLiveLines(t) in
                             L.iter (fun l -> liveTmpsPerLine.(l) = t::liveTmpsPerLine.(l))) liveLines) temps in
  let () = A.iter (fun line -> drawAllEdges line interferenceGraph) liveTmpsPerLine in
  interferenceGraph

let analyzeLiveness (L : tmp2AddrProg) =
  let progArray = A.of_list L
  let len = A.length progArray in
  let lineToPredecessorsArray = A.make len [] in
  let () = addPrecedessors lineToPredecessorsArray progArray 0 in
  let lineToLiveVarsTable = H.create len in
  let () = addLiveVars lineToLiveVarsTable progArray 0
  
