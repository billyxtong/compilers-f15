open Datatypesv1
module A = Ast
module M = Core.Std.Map
open Core.Std             

let rec trans_int_exp idToTmpMap = function
       A.IntConst c -> TmpIntArg (TmpConst c)
     | A.IntIdent id ->
          (match M.find idToTmpMap id with
             None -> 
             let () = print_string("Undeclared: " ^ id ^ "\n") in
             assert(false)
           | Some t -> TmpIntArg (TmpLoc (Tmp t)))
     | A.ASTBinop (e1, op, e2) ->
          TmpInfAddrBinopExpr (op, trans_int_exp idToTmpMap e1,
                   trans_int_exp idToTmpMap e2)
let trans_bool_exp idToTmpMap e = TmpBoolArg (TmpConst 1)

let trans_exp idToTmpMap = function
       A.IntExpr e -> TmpIntExpr (trans_int_exp idToTmpMap e)
     | A.BoolExpr e -> TmpBoolExpr (trans_bool_exp idToTmpMap e)

let rec trans_if idToTmpMap (e, stmtForIf, stmtForElse):tmpInfAddrInstr list =
     match e with
         A.LogNot e' ->
              trans_if idToTmpMap (e', stmtForElse, stmtForIf)
                          (* just switch them *)
       | A.BoolConst c -> (match c with
                              1 -> trans_stmts idToTmpMap stmtForIf
                            | 0 -> trans_stmts idToTmpMap stmtForElse
                            | _ -> assert(false))
       | A.BoolIdent id ->  
          (match M.find idToTmpMap id with
             None -> 
             let () = print_string("Undeclared: " ^ id ^ "\n") in
             assert(false)
           | Some t ->
               let ifLabel = (GenLabel.create()) in
               let elseLabel = (GenLabel.create()) in
               let endLabel = (GenLabel.create()) in
               TmpInfAddrBoolInstr
                 (TmpInfAddrTest (TmpBoolArg (TmpLoc (Tmp t)),
                                  TmpBoolArg (TmpConst 1)))
                 (* check to make sure you don't mix up je and jne *)
               ::TmpInfAddrJump(JNE, ifLabel)
               ::TmpInfAddrJump(JE, elseLabel)
               ::TmpInfAddrLabel(ifLabel)
               ::trans_stmts idToTmpMap stmtForIf
               @ TmpInfAddrLabel(elseLabel)
               ::trans_stmts idToTmpMap stmtForElse
          )
       | _ -> assert(false)
(* currently assuming all asnops are just eq, because we expanded
   asnops in c0Parser.mly *)
and trans_stmts idToTmpMap = function
     A.Decl (id, idType, stmts1)::stmts2 ->
           trans_stmts idToTmpMap stmts1 @ trans_stmts idToTmpMap stmts2
   | A.AssignStmt (id, e)::stmts ->
        let expInfAddr = trans_exp idToTmpMap e in
        let t = Temp.create() in
        (* We have to do the above line before adding t for the following
           reason. First, a variable might be assigned to multiple temps,
           since we create a new temp for every simpAssign. Second,
           assignment evaluates right hand side first, so we need to
           do trans_int_expr with the previous binding of id *)
        let newMap = M.add idToTmpMap id t in
        TmpInfAddrMov (expInfAddr, Tmp t)::trans_stmts idToTmpMap stmts
   | A.Nop::stmts -> trans_stmts idToTmpMap stmts
   | A.If (e, ifStmts, elseStmts) :: stmts ->
        trans_if idToTmpMap (e, ifStmts, elseStmts) @
        trans_stmts idToTmpMap stmts
   | A.Return e :: stmts ->
     (* Can I assume that nothing after the return in a given
        subtree matters? That should be fine, right? *)
        TmpInfAddrReturn (TmpIntExpr (trans_int_exp idToTmpMap e)) :: []
   | _ -> assert(false)                                                        

(* We assume that this is run after typechecking, so everything is
   declared initialized, etc *)
let toInfAddr ast = let idToTmpMap = String.Map.empty in
                     trans_stmts idToTmpMap ast
