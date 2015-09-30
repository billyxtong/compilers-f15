(* L1 Compiler
 * TypeChecker
 * Author: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *
 * Simple typechecker that is based on a unit Symbol.table
 * This is all that is needed since there is only an integer type present
 * Also, since only straightline code is accepted, we hack our way
 * around initialization checks here.
 *
 * Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
 * Now distinguishes between declarations and initialization
 * Modified: Maxime Serrano <mserrano@andrew.cmu.edu> Fall 2014
 * Should be more up-to-date with modern spec.
 * Modified: Matt Bryant <mbryant@andrew.cmu.edu> Fall 2015
 * Handles undefined variables in unreachable code, significant simplifications
 *
 *)

(* open Core.Std *)

module A = Ast
module H = Hashtbl

(* tc_exp : unit Symbol.Map.t -> Ast.exp -> Mark.ext option -> unit *)
let rec tc_exp env ast ext =
  match ast with
    A.Var id ->
      (match S.find env id with
      | None -> ErrorMsg.error ext
          ("undeclared variable `" ^ Symbol.name id ^ "'");
        raise ErrorMsg.Error
      | Some false -> ErrorMsg.error ext
          ("uninitialized variable `" ^ Symbol.name id ^ "'") ;
          raise ErrorMsg.Error
      | Some true -> ())
  | A.ConstExp c -> ()
  | A.OpExp (oper,es) ->
      (* Note: it is syntactically impossible in this language to
       * apply an operator to an incorrect number of arguments
       * so we only check each of the arguments
       *)
      List.iter es ~f:(fun e -> tc_exp env e ext)
  | A.Marked marked_exp ->
      tc_exp env (Mark.data marked_exp) (Mark.ext marked_exp)

(* tc_exp : unit Symbol.Map.t -> Ast.exp -> Mark.ext option -> unit *)
let rec tc_exp' env ast ext =
  match ast with
    A.Var id ->
      (match S.find env id with
      | None -> raise ErrorMsg.Error
      | Some false -> ()
      | Some true -> ())
  | A.ConstExp c -> ()
  | A.OpExp (oper,es) ->
      (* Note: it is syntactically impossible in this language to
       * apply an operator to an incorrect number of arguments
       * so we only check each of the arguments
       *)
      List.iter es ~f:(fun e -> tc_exp' env e ext)
  | A.Marked marked_exp ->
      tc_exp' env (Mark.data marked_exp) (Mark.ext marked_exp)

(* tc_stms :
 *   bool Symbol.Map.t -> Ast.program -> Mark.ext option -> bool -> bool
 * find env id = Some true if id is declared and initialized
 * find env id = Some false if id is declared but not initialized
 * find env id = None if id is not declared *)
let rec tc_stms env ast ext ret =
  match ast with
    [] -> ret
  | A.PreElabDecl(d)::stms ->
      (match d with
        A.NewVar (id, idType) ->
          (try let _ = H.find env id in
               ErrorMsg.error None ("redeclared variable" ^ id ^ "\n");
               raise ErrorMsg.Error
           with Not_found ->
               let () = H.add env id false in
               tc_stms env ast ext ret)
      | A.Init (id, idType, e) ->
          tc_stms env ((A.PreElabDecl (A.NewVar (id, idType)))
          ::(A.SimpAssign(id, A.EQ, e))::stms) ext ret)
          (* tc_stms env (A.Declare(A.NewVar id)::A.Assign(id, e)::stms) ext ret *)
  | A.SimpAssign(id, op, e)::stms ->
      tc_exp env e ext;
          (try
              let _ = H.find env id in (* it's declared, good *)
              let _ = H.replace env id true (* it's now initialized *) in
              tc_stms env ast ext ret
           with Not_found -> 
               ErrorMsg.error None ("undeclared variable" ^ id ^ "\n");
               raise ErrorMsg.Error)
  | A.PreElabReturn(e)::stms ->
      tc_exp env e ext;
      (* Define all variables declared before return *)
      (* let env = S.map env ~f:(fun _ -> true) in *)
      tc_stms env stms ext true
        
(* env maps declared vars to boolean: whether or not it has been
   initialized *)
let rec typecheck' stms = let env = H.create 50 in
  tc_stms env stms None false

let typecheck prog =
  if typecheck' prog then ()
  else (ErrorMsg.error None "main does not return\n"; raise ErrorMsg.Error)
