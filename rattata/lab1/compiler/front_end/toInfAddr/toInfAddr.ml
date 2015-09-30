open Datatypesv1
module A = Ast
module T = Tree
module H = Hashtbl

let varToTmpsMap = H.create 50

let trans_oper = function
    ADD -> T.ADD
  | SUB -> T.SUB
  | MUL -> T.MUL
  | FAKEDIV -> T.DIV
  | FAKEMOD -> T.MOD

let rec trans_exp = function
       A.PreElabConstExpr c -> T.CONST (Int32.of_int c)
     | A.IdentExpr id ->
          (try T.TEMP (H.find varToTmpsMap id)
           with Not_found -> failwith "undeclared variable I think\n")
     | A.PreElabBinop (e1, TmpBinop op, e2) ->
          T.BINOP (trans_oper op, trans_exp e1, trans_exp e2)
     | A.UnaryMinus e -> T.BINOP (T.SUB, T.CONST Int32.zero, trans_exp e)
                              
    (* after type-checking, id must be declared; do not guard lookup *)
  (*   A.Var id -> T.TEMP (S.find_exn env id) *)
  (* | A.ConstExp c -> T.CONST c *)
  (* | A.OpExp (oper, [e1; e2]) -> *)
  (*     T.BINOP (trans_oper oper, trans_exp env e1, trans_exp env e2) *)
  (* | A.OpExp (A.NEGATIVE, [e]) -> *)
  (*     T.BINOP (trans_oper A.NEGATIVE, T.CONST Int32.zero, trans_exp env e) *)
  (* | A.Marked marked_exp -> trans_exp env (Mark.data marked_exp) *)
  (* | _ -> assert false *)

(* translate the statement *)
(* trans_stms : Temp.temp Symbol.table -> A.stm list -> Tree.stm list *)
let rec trans_stms = function
    (A.PreElabDecl d)::stms ->
      (match d with
        A.NewVar (id, idType) -> trans_stms stms
      | A.Init (id, idType, e) -> trans_stms ((A.Assign (id, e))::stms))
  | (A.Assign (id,e))::stms ->
      let t = Temp.create () in
      let env' = S.add env ~key:id ~data:t in
      T.MOVE (T.TEMP t, trans_exp env e) :: trans_stms env' stms
  | (A.Return e)::_ ->
      (* ignore code after return *)
      T.RETURN (trans_exp env e) :: []
  | (A.Markeds marked_stm)::stms ->
      trans_stms env ((Mark.data marked_stm)::stms)
  | [] -> assert false                  (* There must be a return! *)

let toInfAddr stms = trans_stms S.empty stms
