open Datatypesv1
module A = Ast
module T = Tree
module H = Hashtbl

let trans_oper = function
    ADD -> T.ADD
  | SUB -> T.SUB
  | MUL -> T.MUL
  | FAKEDIV -> T.DIV
  | FAKEMOD -> T.MOD

let rec trans_exp varToTmpMap = function
       A.PreElabConstExpr c -> T.CONST (Int32.of_int c)
     | A.IdentExpr id ->
          (try T.TEMP (H.find varToTmpMap id)
           with Not_found ->
             let () = print_string("Undeclared: " ^ id ^ "\n") in
             assert(false))
     | A.PreElabBinop (e1, TmpBinop op, e2) ->
          T.BINOP (trans_oper op, trans_exp varToTmpMap e1,
                   trans_exp varToTmpMap e2)
     | A.UnaryMinus e ->
          T.BINOP (T.SUB, T.CONST Int32.zero, trans_exp varToTmpMap e)
                              
    (* after type-checking, id must be declared; do not guard lookup *)
  (*   A.Var id -> T.TEMP (S.find_exn env id) *)
  (* | A.ConstExp c -> T.CONST c *)
  (* | A.OpExp (oper, [e1; e2]) -> *)
  (*     T.BINOP (trans_oper oper, trans_exp env e1, trans_exp env e2) *)
  (* | A.OpExp (A.NEGATIVE, [e]) -> *)
  (*     T.BINOP (trans_oper A.NEGATIVE, T.CONST Int32.zero, trans_exp env e) *)
  (* | A.Marked marked_exp -> trans_exp env (Mark.data marked_exp) *)
  (* | _ -> assert false *)

(* Creates a new tmp for this variable, unless this variable has
   already been declared, in which case throws an error. *)
let updateTmpMap varToTmpMap id =
     try let _ = H.find varToTmpMap id in
         let () = print_string("Redeclared: " ^ id ^ "\n") in
         assert(false)
     with Not_found -> let t = Temp.create() in
       H.add varToTmpMap id t

(* currently assuming all asnops are just eq, because we expanded
   asnops in c0Parser.mly *)
let rec trans_stms varToTmpMap = function
    (A.PreElabDecl d)::stms ->
        (match d with
           A.NewVar (id, idType) ->
             let () = updateTmpMap varToTmpMap id in
             trans_stms varToTmpMap stms
         | A.Init (id, idType, e) ->
             let () = updateTmpMap varToTmpMap id in
             trans_stms varToTmpMap ((A.SimpAssign (id, A.EQ, e))::stms))
  | (A.SimpAssign (id, op, e))::stms ->
        let dest = T.TEMP (try H.find varToTmpMap id
                   with Not_found -> failwith "undeclared var") in
        T.MOVE (dest, trans_exp varToTmpMap e) :: trans_stms varToTmpMap stms
  | (A.PreElabReturn e)::stms ->
    (* assume there is no code after return CHANGEEEEEEEEEEEE FOR L2 *)
      T.RETURN (trans_exp varToTmpMap e) :: []
  | [] -> assert false                  (* There must be a return! *)

let toInfAddr stms = let varToTmpMap = H.create 50 in
                     trans_stms varToTmpMap stms
