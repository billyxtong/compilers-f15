
(* CHANGE REPRs *)
(* Using an adjacency map representation: each vertex is
   represented by an int. This is easy for tmps, who each have
   distinct ints. Right now, we're just not using eax/edx for
   anything other than wonky idiv, but at some point, we
   probably want to incorporate them here too. *)
(* open Datatypesv1 *)
open Hashtbl

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

let addEdge g (v1, v2) =
    let _ = addDirectedEdge g (v1, v2) in
    addDirectedEdge g (v2, v1)

let hasEdge g (v1, v2) =
    match getNeighbors g v1 with
       None -> false
     | Some neighs -> not (find_all neighs v2 = [])

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
                 
let g = emptyGraph();;
let _ = addEdge g (0,1);;
let _ = addEdge g (1,2);;
let _ = addEdge g (2,3);;

