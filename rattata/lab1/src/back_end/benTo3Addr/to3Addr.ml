open Datatypesv1
(* until we switch to using only our datatypes, be careful
   of namespace collisions: i.e., we have an exp in Datatypesv1
   but there's also a Tree.exp *)

(* For now just going to use the same temp generation thing
   as in temp.ml, because then we won't overwrite temps
   used in toInfAddr prior. *)

(* Returns the statement list, and what tmp the expression
   is ending up in *)
let expTo3Addr (e: Tree.exp):
  (tmp3AddrInstr list * tmpAssemLoc) =
   match e with
      CONST(x) -> let t = Temp.create() in
       (* currently need Int32.to_int because our datatype
          uses int, not int32. Maybe change this? *)
                  ([Tmp3AddrMov(AssemArg(Const(Int32.to_int x,
                                               INT)),
                                Tmp(Temp.create()))],
                   Tmp(t))
    | TEMP(t) -> ([], Tmp(t))
    |                 
  
let stmtTo3Addr (stm: Tree.stm): tmp3AddrInstr list =
   match stm with
     RETURN(e) -> []
   | MOVE(e1, e2) -> []
  

let to3AddrFromTheirInfAddr (infAddrStmts: Tree.stm list):
   tmp3AddrProg = [] 
