
(* CHANGE REPRs *)
(* Using an adjacency map representation: each vertex is
   represented by an int. This is easy for tmps, who each have
   distinct ints. Right now, we're just not using eax/edx for
   anything other than wonky idiv, but at some point, we
   probably want to incorporate them here too. *)
(* open Datatypesv1 *)
open Hashtbl
module PQ = Core.Std.Heap

type vertex = int
(* a set of neighbors *)  
type neighbors = (vertex, unit) Hashtbl.t
(* each vertex mapped to a set of its neighbors *)
(* Note, these are undirected graphs, so a vertex u is
   in the neighbors of v iff v is in the neighbors of u *)
type graph = (vertex, neighbors) Hashtbl.t

(* the 10 is the estimate of the size *)

let expectedNumNeighs = 5
let expectedNumVerts = 10

let emptyGraph () : graph = create expectedNumVerts

let getNeighbors g v =
     match find_all g v with (* list of bindings of v *)
         x1::x2::_ -> failwith "one vertex should not have\
                        multiple sets of neighbors\n"
       | [] -> None
       | x::[] -> Some x

let initVertex g v =
    match getNeighbors g v with
       None -> add g v (create expectedNumNeighs)
     | Some _ -> ()

let addDirectedEdge g (v1, v2) =
    let () = initVertex g v1 in
    match getNeighbors g v1 with
        None -> failwith "Just initialized vertex, what?\n"
      | Some neighs -> add neighs v2 ()

let hasEdge g (v1, v2) =
    match getNeighbors g v1 with
       None -> false
     | Some neighs -> not (find_all neighs v2 = [])

let addEdge g (v1, v2) =
    if hasEdge g (v1, v2) then () else
    let _ = addDirectedEdge g (v1, v2) in
    addDirectedEdge g (v2, v1)

let neighsHaveColor neighs coloring curr_color =
    fold (fun neigh -> fun () -> fun already ->
             already || try find coloring neigh = curr_color
                        with Not_found -> false)
        neighs false

let rec nextColor g coloring v curr_color =
   match getNeighbors g v with
      None -> curr_color
    | Some neighs ->
        if neighsHaveColor neighs coloring curr_color 
        (* try next color *)
        then nextColor g coloring v (curr_color + 1)
        else curr_color

let rec greedilyColorActual g ordering coloring =
    match ordering with
       [] -> coloring
     | v::vs ->
          let _ = add coloring v (nextColor g coloring v 0) in
          greedilyColorActual g vs coloring

(* Just a warpper for greedilyColorActual *)
let greedilyColor g ordering =
    let coloring = create expectedNumVerts in
    let _ = greedilyColorActual g ordering coloring in
    coloring

(* increment the weight of a particular vertex *)
let incrWt wts_pq v =
    match PQ.find wts_pq (fun (u, _) -> u = v) with
       None -> () (* this vertex may have already been
                     removed from the PQ *)
     | Some (_, wt) -> PQ.add wts_pq (v, wt + 1)

let rec process_vert g wts_pq processed_verts v =
   match find_all processed_verts v with
     [] -> let () = add processed_verts v () in
        (* haven't seen this one yet, process it *)
        (match getNeighbors g v with
          None -> v::actualMaxCardSearch g wts_pq processed_verts
        | Some neighs -> let () = iter
                (fun u -> fun _ -> incrWt wts_pq u) neighs in
                 v::actualMaxCardSearch g wts_pq processed_verts)
   | _ -> actualMaxCardSearch g wts_pq processed_verts

(* pop off the max wt vertex, increment neighbors,
   add it to the ordering and recurse *)
and actualMaxCardSearch (g:graph) wts_pq processed_verts =
  if length processed_verts = length g then [] else
    (* if we've processed all the vertices, we're done *)
    match PQ.pop wts_pq with
        None -> failwith "I don't think we should ever\
                  reach this case, because we should have\
                  terminated from the if statement\n"
      | Some (v, _) -> process_vert g wts_pq processed_verts v
                
let maxCardSearch (g:graph) tieBreakFunc =
    let pq_elems =
       fold (fun v -> fun _ -> fun acc -> (v, 0)::acc) g [] in
    let cmp (v1, wt1) (v2, wt2) =
        if not (wt2 - wt1 = 0) then wt2 - wt1 else tieBreakFunc v1 v2
                                 in
    (* wt2 - wt1 because we actually want a max-heap here *)
    let wts_pq = PQ.of_list pq_elems cmp in
    actualMaxCardSearch g wts_pq (create (length g))
    
    

let g = emptyGraph();;
let _ = addEdge g (0,1);;
let _ = addEdge g (1,2);;
let _ = addEdge g (2,3);;

