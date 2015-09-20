open Datatypesv1
(* until we switch to using only our datatypes, be careful
   of namespace collisions: i.e., we have an exp in Datatypesv1
   but there's also a Tree.exp *)

(* Start using tmps with numbers after what have already been
   used in generating InfAddr. There's definitely a better
   way to do this. *)
let next_tmp_num = Temp.counter;;

(* Return the next tmp, and update next_tmp_num *)
let newTmp (): tmp =
   let result = !next_tmp_num in
   let () = next_tmp_num := !next_tmp_num + 1 in
   result

(* Returns the statement list, and what tmp the expression
   is ending up in *)
let expTo3Addr (e: Tree.exp):
  (tmp3AddrInstr list * tmpAssemLoc) =
   match e with
      CONST(x) -> let t = newTmp() in
       (* currently need Int32.to_int because our datatype
          uses int, not int32. Maybe change this? *)
                  ([Tmp3AddrMov(AssemArg(Const(Int32.to_int x,
                                               INT)),
                                Tmp(newTmp()))],
                   Tmp(t))
    | TEMP(t) -> ([], Tmp(t))
  
let stmtTo3Addr (stm: Tree.stm): tmp3AddrInstr list =
   match stm with
     RETURN(e) -> []
   | MOVE(e1, e2) -> []
  

let to3AddrFromTheirInfAddr (infAddrStmts: Tree.stm list):
   tmp3AddrProg = [] 
