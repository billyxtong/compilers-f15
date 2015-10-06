open Datatypesv1
module A = Ast
module M = Core.Std.Map
open Core.Std

let get_or_make_tmp id idToTmpMap = (match M.find idToTmpMap id with
              None -> Temp.create()
            | Some t -> t)

let rec trans_int_exp idToTmpMap = function
       A.IntConst c -> TmpIntArg (TmpConst c)
     | A.IntIdent id -> 
          (match M.find idToTmpMap id with
             None -> 
             let () = print_string("Undeclared: " ^ id ^ "\n") in
             assert(false)
           | Some t -> 
             TmpIntArg (TmpLoc (Tmp t)))
     | A.ASTBinop (e1, op, e2) ->
          TmpInfAddrBinopExpr (op, trans_int_exp idToTmpMap e1,
                   trans_int_exp idToTmpMap e2)

(* This returns a tmp t and a list of statements required to put
   e in t. What we do to handle short-circuit here is just say
   if (e) t = true; else t = false;
*)
let rec trans_bool_exp idToTmpMap e dest =
    let t = (match dest with
                 None -> Temp.create()
               | Some t' -> t') in
    match e with
        A.BoolConst c ->
            (TmpInfAddrMov(TmpBoolExpr (TmpBoolArg (TmpConst c)), Tmp t)
            ::[], Tmp t)
      | _ ->
    (* Ok this is kind of hacky but here it is: the functions that
       take "if (e) t = true; else t = false;" to infAddr all take
       asts as input, but asts only take idents, not tmps. So we
       create an ident that is guaranteed to not already be an ident
       (GenUnusedID basically puts "\\" at the front of id's, which
       cannot be part of the input), and map it to t. *)
    let identForT = GenUnusedID.create() in
    let newMap = M.add idToTmpMap identForT t in
    let ifStmts = [A.TypedPostElabAssignStmt (identForT, A.BoolExpr (A.BoolConst 1))] in
    let elseStmts = [A.TypedPostElabAssignStmt (identForT, A.BoolExpr (A.BoolConst 0))] in
    (* Translating this if statement puts expression e in temp t.
       We also need to return the resulting location. *)
    (trans_stmts newMap [A.TypedPostElabIf (e, ifStmts, elseStmts)],
     Tmp t)

(* For a while loop, typically we jump back to the top if the
   if condition is true. This would be jumpToTopStatus =
   ifJumpsToStop. However, since we switch around if and else to
   deal with negation and such, the else might have to the jump
   back to the top. Of course, for a normal if statement,
   neither jumps to top. *)
and make_cond_instrs idToTmpMap priorInstr stmtsForIf stmtsForElse
    ifJumpType elseJumpType =
    let ifLabel = GenLabel.create() in
    let elseLabel = GenLabel.create() in
    let endLabel = GenLabel.create() in
    priorInstr
    ::TmpInfAddrJump(ifJumpType, ifLabel)
    ::TmpInfAddrJump(elseJumpType, elseLabel)
    ::TmpInfAddrLabel(ifLabel)
    ::trans_stmts idToTmpMap stmtsForIf
    @ TmpInfAddrJump(JMP_UNCOND, endLabel)
    ::TmpInfAddrLabel(elseLabel)
    ::trans_stmts idToTmpMap stmtsForElse
    @ TmpInfAddrJump(JMP_UNCOND, endLabel)
    ::TmpInfAddrLabel(endLabel) :: []

(* We use this function for both ifs and whiles. We pass in
   make_instrs_fn, which creates the structure which kind of
   wraps the statements in side the if/while. So for if, if will
   create if/else/end labels, and there will be an unconditional
   jump at the end of if and else. But for while, there will be
   a start label (and no else label), and there will be an
   unconditional jump to the start label after the if *)
and trans_cond idToTmpMap (condition, stmtsForIf, stmtsForElse) 
  : tmpInfAddrInstr list = 
     match condition with
         A.LogNot negCondition ->
        (* In this case, just switch the statements for if and else,
           AND ALSO THE JUMPTOTOPSTATUS *)
              trans_cond idToTmpMap (negCondition, stmtsForElse,
                                   stmtsForIf)
       | A.LogAnd (bool_exp1, bool_exp2) -> 
          (* For &&, we break it up into nested if statements, where each
             of them gets the "else" from the original. *)
           let innerIfAst = [A.TypedPostElabIf (bool_exp2, stmtsForIf, stmtsForElse)] in
            trans_cond idToTmpMap (bool_exp1, innerIfAst,
                                 stmtsForElse) 
              (* ONLY THE INNER ONE JUMPS TO TOP FIX THISSSSSSSSSSSSSS *)
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
                 stmtsForElse JG JLE )
       | A.IntEquals (int_exp1, int_exp2) ->
            let priorInstr = TmpInfAddrBoolInstr
            (TmpInfAddrCmp(trans_int_exp idToTmpMap int_exp2,
                           trans_int_exp idToTmpMap int_exp1)) in
              (make_cond_instrs idToTmpMap priorInstr stmtsForIf stmtsForElse
                 JE JNE )
       | A.BoolEquals (bool_exp1, bool_exp2) ->
            (* See description of trans_bool_exp *)
            let (instrs_for_exp1, tmp_exp1) =
               trans_bool_exp idToTmpMap bool_exp1 None in
            let (instrs_for_exp2, tmp_exp2) =
               trans_bool_exp idToTmpMap bool_exp2 None in
            let priorInstr = TmpInfAddrBoolInstr
                (TmpInfAddrCmp (TmpIntArg (TmpLoc tmp_exp1),
                                TmpIntArg (TmpLoc tmp_exp2))) in
            (* The order of instrs_for_exp1/2 shouldn't matter *)
            instrs_for_exp1 @ instrs_for_exp2 @
            (make_cond_instrs idToTmpMap priorInstr stmtsForIf
                 stmtsForElse JE JNE )
       | A.BoolIdent id ->  
          (match M.find idToTmpMap id with
             None -> 
             let () = print_string("Undeclared: " ^ id ^ "\n") in
             assert(false)
           | Some t -> let priorInstr = TmpInfAddrBoolInstr
                 (TmpInfAddrTest (TmpBoolArg (TmpLoc (Tmp t)),
                                  TmpBoolArg (TmpConst 1))) in
                   make_cond_instrs idToTmpMap priorInstr stmtsForIf
                      stmtsForElse JNE JE )
                 (* check to make sure you don't mix up je and jne *)

and trans_stmts idToTmpMap = function
     A.TypedPostElabDecl (id, idType)::stmts ->
        trans_stmts idToTmpMap stmts
   | A.TypedPostElabAssignStmt (id, A.BoolExpr e)::stmts ->
        (* Here's the deal: if this id already has a temp associated with it
           in this scope, we need to use that same temp here. The reason is
           that when we break "x = y" where y is a boolean, that turns into
           if (y) x = 1; else x = 0;. Which means the if and else need to write
           to the same temp (even though they're in different scopes.
           So basically if there already is a temp for id, we use that,
           otherwise we create a new one (this is handled in trans_bool_exp) *)
        let dest_for_bool_expr = M.find idToTmpMap id in (* We can just directly
             use the result of find because want an option type anyway,
             see trans_bool_exp*)
        let (instrs_for_move, Tmp new_t) =
          (* trans_bool_exp gives us the instructions it generated, and also
             where it ended up putting the value (if it didn't have to
             create a new one, new_t will just be whatever we passed in.
             We then update the binding in idToTmpMap (which does nothing
             if new_t is what we passed in) *)
               trans_bool_exp idToTmpMap e dest_for_bool_expr in
        let newMap = M.add idToTmpMap id new_t in
        instrs_for_move @ trans_stmts newMap stmts
   | A.TypedPostElabAssignStmt (id, A.IntExpr e)::stmts ->
        let expInfAddr = TmpIntExpr (trans_int_exp idToTmpMap e) in
        let t = get_or_make_tmp id idToTmpMap in
        let newMap = M.add idToTmpMap id t in
        TmpInfAddrMov (expInfAddr, Tmp t)::trans_stmts newMap stmts
   | A.TypedPostElabIf (e, ifStmts, elseStmts) :: stmts ->
        trans_cond idToTmpMap (e, ifStmts, elseStmts) @
        trans_stmts idToTmpMap stmts
   | A.TypedPostElabWhile (e, whileStmts) :: stmts ->
        let topLabel = GenLabel.create() in
        let jumpToTopStmt = A.JumpUncond(topLabel) in
        (* Empty list because no else statements *)
        TmpInfAddrLabel topLabel::
        trans_cond idToTmpMap (e, whileStmts @ [jumpToTopStmt], []) @
        trans_stmts idToTmpMap stmts
   | A.JumpUncond target :: stmts ->
     (* This really shouldn't be an AST instruction
        but I need it for toInfAddr :( *)
        TmpInfAddrJump(JMP_UNCOND, target)::trans_stmts idToTmpMap stmts
   | A.TypedPostElabReturn e :: stmts ->
     (* Can I assume that nothing after the return in a given
        subtree matters? That should be fine, right? *)
        TmpInfAddrReturn (TmpIntExpr (trans_int_exp idToTmpMap e)) :: []
   | _ -> []

(* We assume that this is run after typechecking, so everything is
   declared initialized, etc *)
let toInfAddr (ast: A.typedPostElabAST) =
     let idToTmpMap = String.Map.empty in trans_stmts idToTmpMap ast
