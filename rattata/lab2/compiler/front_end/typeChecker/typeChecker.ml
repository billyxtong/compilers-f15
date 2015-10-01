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

module A = Ast
module H = Hashtbl

let rec tc_exp env e ext =
  match e with
    A.IdentExpr id ->
     (try
         let init = H.find env id in (* is it declared? *)
         if init then () (* declared and initialized, all good *)
         else (ErrorMsg.error None ("uninitialized variable " ^ id ^ "\n");
              raise ErrorMsg.Error)
       with Not_found -> 
              (ErrorMsg.error None ("undeclared variable " ^ id ^ "\n");
              raise ErrorMsg.Error))
  | A.PreElabConstExpr c -> ()
  | A.PreElabBinop (e1, op, e2) ->
      tc_exp env e1 ext;
      tc_exp env e2 ext
  | A.UnaryMinus e' -> tc_exp env e' ext

(* Not sure what this function is for but it was in the starter code
   so I guess we can keep it around for a bit longer *)
(* let rec tc_exp' env ast ext = *)
(*   match ast with *)
(*     A.Var id -> *)
(*       (match S.find env id with *)
(*       | None -> raise ErrorMsg.Error *)
(*       | Some false -> () *)
(*       | Some true -> ()) *)
(*   | A.ConstExp c -> () *)
(*   | A.OpExp (oper,es) -> *)
(*       (\* Note: it is syntactically impossible in this language to *)
(*        * apply an operator to an incorrect number of arguments *)
(*        * so we only check each of the arguments *)
(*        *\) *)
(*       List.iter es ~f:(fun e -> tc_exp' env e ext) *)
(*   | A.Marked marked_exp -> *)
(*       tc_exp' env (Mark.data marked_exp) (Mark.ext marked_exp) *)

let rec tc_stms env ast ext ret =
  match ast with
    [] -> ret
  | A.PreElabDecl(d)::stms ->
      (match d with
        A.NewVar (id, idType) ->
          (try let _ = H.find env id in 
               ErrorMsg.error None ("redeclared variable " ^ id ^ "\n");
               raise ErrorMsg.Error
           with Not_found ->
               let () = H.add env id false in
               tc_stms env stms ext ret)
      | A.Init (id, idType, e) ->
          tc_stms env ((A.PreElabDecl (A.NewVar (id, idType)))
          ::(A.SimpAssign(id, A.EQ, e))::stms) ext ret)
  | A.SimpAssign(id, op, e)::stms ->
      tc_exp env e ext;
          (try
              let _ = H.find env id in (* it's declared, good *)
              let _ = H.replace env id true (* it's now initialized *) in
              tc_stms env stms ext ret
           with Not_found -> 
               ErrorMsg.error None ("undeclared variable " ^ id ^ "\n");
               raise ErrorMsg.Error)
  | A.PreElabReturn(e)::stms -> 
      let () = tc_exp env e ext in
      (* apparently all variables defined before the first return
         get to be treated as initialized...although those declared
         after don't *)
      let () = H.iter (fun id _ -> H.replace env id true) env in
      tc_stms env stms ext true
        
(* env maps declared vars to boolean: whether or not it has been
   initialized *)
let rec typecheck' stms = let env = H.create 50 in
  tc_stms env stms None false

let typecheck prog =
  if typecheck' prog then ()
  else (ErrorMsg.error None "main does not return\n"; raise ErrorMsg.Error)
