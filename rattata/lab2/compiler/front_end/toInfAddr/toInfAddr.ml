open Datatypesv1
module A = Ast
module M = Core.Std.Map
open Core.Std             

(* Tells a conditional expression if it needs to jump to the top
   (i.e., to execute it again, in a loop. *)
type jumpToTop = NoJump | IfJumpsToTop | ElseJumpsToTop

let negate jumpToTopStatus =
    match jumpToTopStatus with
         NoJump -> NoJump
       | IfJumpsToTop -> ElseJumpsToTop
       | ElseJumpsToTop -> IfJumpsToTop

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

(* This returns a tmp t and a list of statements required to put
   e in t. What we do to handle short-circuit here is just say
   if (e) t = true; else t = false;
*)
let rec trans_bool_exp idToTmpMap e =
    let t = Temp.create() in
    (* Ok this is kind of hacky but here it is: the functions that
       take "if (e) t = true; else t = false;" to infAddr all take
       asts as input, but asts only take idents, not tmps. So we
       create an ident that is guaranteed to not already be an ident
       (hence the \\), and map it to t. *)
    let identForT = "\\" ^ string_of_int(t) in
    let newMap = M.add idToTmpMap identForT t in
    let ifStmts = [A.AssignStmt (identForT, A.BoolExpr (A.BoolConst 1))] in
    let elseStmts = [A.AssignStmt (identForT, A.BoolExpr (A.BoolConst 0))] in
    (* Translating this if statement puts expression e in temp t.
       We also need to return the resulting location. *)
    (trans_stmts newMap [A.If (e, ifStmts, elseStmts)],
     Tmp t)

(* For a while loop, typically we jump back to the top if the
   if condition is true. This would be jumpToTopStatus =
   ifJumpsToStop. However, since we switch around if and else to
   deal with negation and such, the else might have to the jump
   back to the top. Of course, for a normal if statement,
   neither jumps to top. *)
and make_cond_instrs idToTmpMap priorInstr stmtsForIf stmtsForElse
    ifJumpType elseJumpType jumpToTopStatus =
    let startLabel = GenLabel.create() in
    let ifLabel = GenLabel.create() in
    let elseLabel = GenLabel.create() in
    let endLabel = GenLabel.create() in
    let (ifEndTarget, elseEndTarget) =
        (match jumpToTopStatus with
             NoJump -> (endLabel, endLabel)
           | IfJumpsToTop -> (startLabel, endLabel)
           | ElseJumpsToTop -> (endLabel, startLabel)) in
    TmpInfAddrLabel(startLabel)
    ::priorInstr
    ::TmpInfAddrJump(ifJumpType, ifLabel)
    ::TmpInfAddrJump(elseJumpType, elseLabel)
    ::TmpInfAddrLabel(ifLabel)
    ::trans_stmts idToTmpMap stmtsForIf
    @ TmpInfAddrJump(JMP_UNCOND, ifEndTarget)
    ::TmpInfAddrLabel(elseLabel)
    ::trans_stmts idToTmpMap stmtsForElse
    @ TmpInfAddrJump(JMP_UNCOND, elseEndTarget)
    ::TmpInfAddrLabel(endLabel) :: []

(* We use this function for both ifs and whiles. We pass in
   make_instrs_fn, which creates the structure which kind of
   wraps the statements in side the if/while. So for if, if will
   create if/else/end labels, and there will be an unconditional
   jump at the end of if and else. But for while, there will be
   a start label (and no else label), and there will be an
   unconditional jump to the start label after the if *)
and trans_cond idToTmpMap (condition, (stmtsForIf: A.postElabAST),
                         (stmtsForElse:A.postElabAST)) jumpToTopStatus : tmpInfAddrInstr list =
     match condition with
         A.LogNot negCondition ->
        (* In this case, just switch the statements for if and else,
           AND ALSO THE JUMPTOTOPSTATUS *)
              trans_cond idToTmpMap (negCondition, stmtsForElse,
                                   stmtsForIf) (negate jumpToTopStatus)
       | A.LogAnd (bool_exp1, bool_exp2) ->
          (* For &&, we break it up into nested if statements, where each
             of them gets the "else" from the original. *)
           let innerIfAst = [A.If (bool_exp2, stmtsForIf, stmtsForElse)] in
            trans_cond idToTmpMap (bool_exp1, innerIfAst,
                                 stmtsForElse) jumpToTopStatus
       | A.BoolConst c -> (match c with
                              1 -> trans_stmts idToTmpMap stmtsForIf
                            | 0 -> trans_stmts idToTmpMap stmtsForElse
                            | _ -> assert(false))
       | A.GreaterThan (int_exp1, int_exp2) -> 
           (* NOTE THAT WE SWITCH THE ORDER BECAUSE CMP IS WEIRD *)
         (* MAKE SURE TO CHECK THIS. Pretty sure it's right though *)
            let priorInstr = TmpInfAddrBoolInstr
                 (TmpInfAddrCmp(trans_int_exp idToTmpMap int_exp2,
                               trans_int_exp idToTmpMap int_exp1)) in
              (make_cond_instrs idToTmpMap priorInstr stmtsForIf
                 stmtsForElse JG JLE jumpToTopStatus)
       | A.IntEquals (int_exp1, int_exp2) ->
            let priorInstr = TmpInfAddrBoolInstr
            (TmpInfAddrCmp(trans_int_exp idToTmpMap int_exp2,
                           trans_int_exp idToTmpMap int_exp1)) in
              (make_cond_instrs idToTmpMap priorInstr stmtsForIf stmtsForElse
                 JE JNE jumpToTopStatus)
       | A.BoolEquals (bool_exp1, bool_exp2) ->
            (* See description of trans_bool_exp *)
            let (instrs_for_exp1, tmp_exp1) =
               trans_bool_exp idToTmpMap bool_exp1 in
            let (instrs_for_exp2, tmp_exp2) =
               trans_bool_exp idToTmpMap bool_exp2 in
            let priorInstr = TmpInfAddrBoolInstr
                (TmpInfAddrCmp (TmpIntArg (TmpLoc tmp_exp1),
                                TmpIntArg (TmpLoc tmp_exp2))) in
            (* The order of instrs_for_exp1/2 shouldn't matter *)
            instrs_for_exp1 @ instrs_for_exp2 @
            (make_cond_instrs idToTmpMap priorInstr stmtsForIf
                 stmtsForElse JE JNE jumpToTopStatus)
       | A.BoolIdent id ->  
          (match M.find idToTmpMap id with
             None -> 
             let () = print_string("Undeclared: " ^ id ^ "\n") in
             assert(false)
           | Some t -> let priorInstr = TmpInfAddrBoolInstr
                 (TmpInfAddrTest (TmpBoolArg (TmpLoc (Tmp t)),
                                  TmpBoolArg (TmpConst 1))) in
                   make_cond_instrs idToTmpMap priorInstr stmtsForIf
                      stmtsForElse JNE JE jumpToTopStatus)
                 (* check to make sure you don't mix up je and jne *)

and trans_stmts idToTmpMap = function
     A.Decl (id, idType)::stmts ->
        trans_stmts idToTmpMap stmts
   | A.AssignStmt (id, A.BoolExpr e)::stmts ->
        (* see description of trans_bool_exp *)
        let (instrs_for_move, Tmp t) = trans_bool_exp idToTmpMap e in
        let newMap = M.add idToTmpMap id t in
        instrs_for_move @ trans_stmts newMap stmts
   | A.AssignStmt (id, A.IntExpr e)::stmts ->
        let expInfAddr = TmpIntExpr (trans_int_exp idToTmpMap e) in
        let t = Temp.create() in
        (* We have to do the above line before adding t for the following
           reason. First, a variable might be assigned to multiple temps,
           since we create a new temp for every simpAssign. Second,
           assignment evaluates right hand side first, so we need to
           do trans_int_expr with the previous binding of id *)
        let newMap = M.add idToTmpMap id t in
        TmpInfAddrMov (expInfAddr, Tmp t)::trans_stmts newMap stmts
   | A.If (e, ifStmts, elseStmts) :: stmts ->
        let jumpToTopStatus = NoJump in (* if never jumps to top *) 
        trans_cond idToTmpMap (e, ifStmts, elseStmts) jumpToTopStatus @
        trans_stmts idToTmpMap stmts
   | A.While (e, whileStmts) :: stmts ->
        let jumpToTopStatus = IfJumpsToTop in
        trans_cond idToTmpMap (e, whileStmts, []) jumpToTopStatus @
        trans_stmts idToTmpMap stmts
   | A.Return e :: stmts ->
     (* Can I assume that nothing after the return in a given
        subtree matters? That should be fine, right? *)
        TmpInfAddrReturn (TmpIntExpr (trans_int_exp idToTmpMap e)) :: []
   | _ -> failwith "there must be a return!"

(* We assume that this is run after typechecking, so everything is
   declared initialized, etc *)
let toInfAddr (ast: A.postElabAST) =
     let idToTmpMap = String.Map.empty in trans_stmts idToTmpMap ast
