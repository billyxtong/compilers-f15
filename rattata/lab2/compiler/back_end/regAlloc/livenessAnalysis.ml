open Graph
open Hashtbl
(*
let mapLineNumToInstr (lineNum : int) tbl (prog : tmp2AddrProg) = 
  match prog with
        [] -> tbl
      | instr :: prog' -> 
          let () = add tbl lineNum instr in
          mapLineNumToInstr (lineNum + 1) tbl (prog')
*)

let findPrecessors (lineNum : int) tbl = 


let predecessors (prog : tmp2AddrProg) = 
  let indexedProg = List.mapi (fun i -> fun instr -> (i, instr)) prog in
  let predecessorTable = create (List.length indexedProg) in 
  findPredecessors 0 indexProg






(* each of the match helpers takes 3 parameters: 
  * -a type of two-address instruction,
  * -the interference graph,
  * -and the list of current live temps
 * then adds edges to the graph and returns an updated live temps list *)
let matchReturn (Tmp2AddrReturn(arg)) (G : graph) (liveTmps : int list) = 
  match arg with
        TmpLoc(Tmp(t)) -> let () = initVertex G t in (t :: liveTmps)
      | TmpConst(c) -> liveTmps

let matchBinop (Tmp2AddrBinop(binop,arg,temp)) (G : graph) (liveTmps : int list) =
  match (arg, temp) with
        (TmpLoc(Tmp(src)), Tmp(dest)) -> 
          (try 
              List.find (fun y -> y = src) liveTmps
           with
              Not_found -> let () = List.iter (fun y -> addEdge G src y) liveTmps
                           in (src :: liveTmps)
            | _ -> liveTmps)
      | (TmpConst(c), Tmp(dest)) -> liveTmps

let matchMov (Tmp2AddrMov(arg, temp)) (G : graph) (liveTmps : int list) =
  match arg with
        TmpLoc(Tmp(t)) -> 
          (try 
              List.find (fun y -> y = x) liveTmps
           with 
              Not_found -> let () = List.iter (fun y -> addEdge G x y) liveTmps
                           in (t :: liveTmps)
            | _ -> liveTmps)
      | TmpConst(c) -> List.filter (fun y -> y = dest) liveTmps

let instrMatch (instr : tmp2AddrInstr) (G : graph) (liveTmps : int list) =
  match instr with
        Tmp2AddrReturn(arg) -> matchReturn instr G liveTmps
      | Tmp2AddrBinop(binop, arg, temp) -> matchBinop instr G liveTmps
      | Tmp2AddrMov(arg, temp) -> matchMov instr G liveTmps

let rec analyzeLiveness' (L : tmp2AddrProg) (G : graph) (liveTmps : int list) =
  match L with
        [] -> G
      | instr :: L' -> (let newLiveList = instrMatch instr G liveTmps
                        in analyzeLiveness' L' G newLiveList)

(* 1. Reverse list of 2-address instructions because we want to look from the bottom
 * 2. Initialize an empty graph so we can start adding temps (vertices) to our interference graph
 * 3. Pass reversed list to helper function that does all the work
 * 4. Apply max cardinality search and greedy coloring function to return a (tmp, color) hashtable *)
let analyzeLiveness (L : tmp2AddrProg) = 
  let reversed_instrs = List.rev L in
  let myGraph = emptyGraph() in
  let liveTmpList = [] in
  let interferenceGraph = analyzeLiveness' L myGraph liveTmpList in
  let tmpList = maxCardSearch interferenceGraph 0 in
  greedilyColor interferenceGraph tmpList




