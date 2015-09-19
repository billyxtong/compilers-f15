(* Graph Library that will contain basic addEdge, etc, operations,
   as well as color and max-cardinality search. *)

type vertex
type neighbors
type graph

val emptyGraph: unit -> graph

val addEdge: graph -> (vertex * vertex) -> unit

val hasEdge: graph -> (vertex * vertex) -> bool

val getNeighbors: graph -> vertex -> neighbors
                                 
(* Given a graph (an interference graph) and the vertex at which
   to start, returns a simplicial elimination order of the
   vertices, in a list *)
val maxCardSearch: graph -> vertex -> vertex list
    
(* Takes an interference graph and an ordering of vertices,
   returns an mapping of each vertex to a color (an integer) *)
val greedilyColor: graph -> vertex list -> (vertex, int) Core.Std.Hashtbl.t
