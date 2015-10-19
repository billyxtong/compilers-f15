open Datatypesv1
module A = Ast
module M = Core.Std.Map
open Core.Std

(* Pretty much everything here is mutually recursive because of
   ternary operators *)

let get_or_make_tmp id idToTmpMap = (match M.find idToTmpMap id with
              None -> Temp.create()
            | Some t -> t)

let rec handle_shift retTmp retLabel idToTmpMap (e1, op, e2) =
    let max_shift = A.IntConst 31 in
    let new_id = GenUnusedID.create () in
    let result_tmp = Temp.create() in
    let newMap = M.add idToTmpMap new_id result_tmp in
        (* whatever gets put in new_id should end up in result_tmp *)
      (* Now: we need one for the div-by-zero instr we're about to add *)
        (* NOTE: e2 not e1!! *)
    let baseCaseShiftOp = (match op with
                              LSHIFT -> A.ASTlshift
                            | RSHIFT -> A.ASTrshift
                            | _ -> assert(false)) in
    let isNonNegative = A.GreaterThan(e2, A.IntConst (-1)) in
    let isSmallEnough = A.LogNot (A.GreaterThan(e2, max_shift)) in
    let condition = A.LogAnd (isNonNegative, isSmallEnough) in
          (* If shift is too big, or is negative, div by zero.
             Else, do the shift *)
    let doTheShift = A.TypedPostElabAssignStmt(new_id, A.IntExpr
                 (A.BaseCaseShift(e1, baseCaseShiftOp, e2)))::[] in
    let divByZero = A.TypedPostElabAssignStmt(new_id, A.IntExpr
                 (A.ASTBinop(A.IntConst 666, FAKEDIV, A.IntConst 0)))::[] in
       (trans_cond retTmp retLabel newMap (condition, doTheShift, divByZero),
       TmpIntArg (TmpLoc (Tmp result_tmp)))

and trans_exp retTmp retLabel idToTmpMap = function
      A.IntExpr e -> let (instrs, e') = trans_int_exp retTmp retLabel idToTmpMap e
                     in (instrs, TmpIntExpr e')
    | A.BoolExpr e -> let (instrs, t) = trans_bool_exp retTmp retLabel idToTmpMap e None in
                      (instrs, TmpBoolExpr (TmpBoolArg (TmpLoc t)))

(* Returns a list of tmpExprs, resulting from calling trans_exp on each of the args *)
and trans_fun_args retTmp retLabel idToTmpMap exp_list =
      let instr_and_e_list = List.map exp_list (trans_exp retTmp retLabel idToTmpMap) in
      let instrs = List.concat (List.map instr_and_e_list (fun (instrs, _) -> instrs)) in
      let exps = List.map instr_and_e_list (fun (_, e) -> e) in
      (instrs, exps)

(* Returns a tuple (instrs, e) where e is the resulting expression,
   and instrs is any required instructions (which is empty for everything
   but ternary *)
and trans_int_exp retTmp retLabel idToTmpMap = function
       A.IntConst c -> ([], TmpIntArg (TmpConst c))
     | A.IntIdent id -> 
          (match M.find idToTmpMap id with
             None -> 
             (let () = print_string("Uninitialized: " ^ id ^ "\n") in
             ([], (TmpIntArg (TmpLoc (Tmp (Temp.create()))))))
           | Some t -> 
             ([], TmpIntArg (TmpLoc (Tmp t))))
     | A.ASTBinop (e1, LSHIFT, e2) ->
         handle_shift retTmp retLabel idToTmpMap (e1, LSHIFT, e2)
     | A.ASTBinop (e1, RSHIFT, e2) ->
         handle_shift retTmp retLabel idToTmpMap (e1, RSHIFT, e2)
     | A.BaseCaseShift (e1, shiftOp, e2) ->
         let (instrs_for_exp1, tmp_exp1) =
          trans_int_exp retTmp retLabel idToTmpMap e1 in
         let (instrs_for_exp2, tmp_exp2) =
          trans_int_exp retTmp retLabel idToTmpMap e2 in
     
         let actualShiftOp = (match shiftOp with
                                     A.ASTrshift -> RSHIFT
                                   | A.ASTlshift -> LSHIFT) in
          (instrs_for_exp1 @ instrs_for_exp2,
           TmpInfAddrBinopExpr (actualShiftOp, tmp_exp1, tmp_exp2))
     | A.ASTBinop (e1, op, e2) ->
         let (instrs_for_exp1, tmp_exp1) =
             trans_int_exp retTmp retLabel idToTmpMap e1 in
         let (instrs_for_exp2, tmp_exp2) =
             trans_int_exp retTmp retLabel idToTmpMap e2 in
         (instrs_for_exp1 @ instrs_for_exp2,
          TmpInfAddrBinopExpr (op, tmp_exp1, tmp_exp2))
         
     | A.IntTernary (c, e1, e2) -> (* See BoolTernary in trans_cond *) 
         let ternary_result_tmp = Temp.create() in
         let ternary_result_id = GenUnusedID.create() in
         let newMap = M.add idToTmpMap ternary_result_id ternary_result_tmp in
         let astForIf = [A.TypedPostElabAssignStmt (ternary_result_id,
                                            A.IntExpr e1)] in
         let astForElse = [A.TypedPostElabAssignStmt (ternary_result_id,
                                            A.IntExpr e2)] in
         (trans_cond retTmp retLabel newMap (c, astForIf, astForElse),
          TmpIntArg (TmpLoc (Tmp ternary_result_tmp)))
     | A.IntFunCall (fName, argList) ->
         let (instrs, argExps) = trans_fun_args retTmp retLabel idToTmpMap argList in
         (instrs, TmpInfAddrIntFunCall(fName, argExps))

(* This returns a tmp t and a list of statements required to put
   e in t. What we do to handle short-circuit here is just say
   if (e) t = true; else t = false;
*)
and trans_bool_exp retTmp retLabel idToTmpMap e dest =
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
    let ifStmts = [A.TypedPostElabAssignStmt
                     (identForT, A.BoolExpr (A.BoolConst 1))] in
    let elseStmts = [A.TypedPostElabAssignStmt
                       (identForT, A.BoolExpr (A.BoolConst 0))] in
    (* Translating this if statement puts expression e in temp t.
       We also need to return the resulting location. *)
    (trans_stmts retTmp retLabel newMap [A.TypedPostElabIf (e, ifStmts, elseStmts)],
     Tmp t)

(* For a while loop, typically we jump back to the top if the
   if condition is true. This would be jumpToTopStatus =
   ifJumpsToStop. However, since we switch around if and else to
   deal with negation and such, the else might have to the jump
   back to the top. Of course, for a normal if statement,
   neither jumps to top. *)
and make_cond_instrs retTmp retLabel idToTmpMap priorInstr stmtsForIf stmtsForElse
    ifJumpType elseJumpType =
    let elseLabel = GenLabel.create() in
    let endLabel = GenLabel.create() in
    if stmtsForIf = [] then
          (priorInstr
          ::TmpInfAddrJump(ifJumpType, endLabel)
          ::trans_stmts retTmp retLabel idToTmpMap stmtsForElse
          @ TmpInfAddrLabel(endLabel) :: []) else
    if stmtsForElse = [] then
          (priorInstr
          ::TmpInfAddrJump(elseJumpType, endLabel)
          ::trans_stmts retTmp retLabel idToTmpMap stmtsForIf
          @ TmpInfAddrLabel(endLabel) :: [])
    else
          (priorInstr
          ::TmpInfAddrJump(elseJumpType, elseLabel)
          ::trans_stmts retTmp retLabel idToTmpMap stmtsForIf
          @ TmpInfAddrJump(JMP_UNCOND, endLabel)
          ::TmpInfAddrLabel(elseLabel)
          ::trans_stmts retTmp retLabel idToTmpMap stmtsForElse
          @ TmpInfAddrLabel(endLabel) :: [])


(* We use this function for both ifs and whiles. We pass in
   make_instrs_fn, which creates the structure which kind of
   wraps the statements in side the if/while. So for if, if will
   create if/else/end labels, and there will be an unconditional
   jump at the end of if and else. But for while, there will be
   a start label (and no else label), and there will be an
   unconditional jump to the start label after the if *)
and trans_cond retTmp retLabel idToTmpMap (condition, stmtsForIf, stmtsForElse) 
  : tmpInfAddrInstr list =
     match condition with
         A.LogNot negCondition ->
        (* In this case, just switch the statements for if and else,
           AND ALSO THE JUMPTOTOPSTATUS *)
              trans_cond retTmp retLabel idToTmpMap (negCondition, stmtsForElse,
                                   stmtsForIf)
       | A.BoolTernary (c, bool_exp1, bool_exp2) ->
          (* BoolTernary means the expressions are bools
             (obviously the condition is a bool too *)
         (* We create a new variable and a new ast that puts bool_exp1
            into this variable is c is true, otherwise bool_exp2. Then
            we can just recurse on a new conditional, with that var as
            the condition *)
            let ternary_result_tmp = Temp.create() in
            let ternary_result_id = GenUnusedID.create() in
            let newMap = M.add idToTmpMap ternary_result_id ternary_result_tmp in
            let astForIf = [A.TypedPostElabAssignStmt (ternary_result_id,
                                              A.BoolExpr bool_exp1)] in
            let astForElse = [A.TypedPostElabAssignStmt (ternary_result_id,
                                              A.BoolExpr bool_exp2)] in
            let ternary_instrs = trans_cond retTmp retLabel newMap (c, astForIf, astForElse) in
            ternary_instrs @
            trans_cond retTmp retLabel newMap (A.BoolIdent ternary_result_id, stmtsForIf,
                               stmtsForElse)
       | A.LogAnd (bool_exp1, bool_exp2) -> 
          (* For &&, we break it up into nested if statements, where each
             of them gets the "else" from the original. *)
           let innerIfAst = [A.TypedPostElabIf (bool_exp2, stmtsForIf,
                                                stmtsForElse)] in
            trans_cond retTmp retLabel idToTmpMap (bool_exp1, innerIfAst,
                                 stmtsForElse) 
              (* ONLY THE INNER ONE JUMPS TO TOP FIX THISSSSSSSSSSSSSS *)
       | A.BoolConst c -> (match c with
                              1 -> trans_stmts retTmp retLabel idToTmpMap stmtsForIf
                            | 0 -> trans_stmts retTmp retLabel idToTmpMap stmtsForElse
                            | _ -> assert(false))
       | A.GreaterThan (int_exp1, int_exp2) -> 
           (* NOTE THAT WE SWITCH THE ORDER BECAUSE CMP IS WEIRD *)
         (* MAKE SURE TO CHECK THIS. Pretty sure it's right though *)
            let (instrs_for_exp1, tmp_exp1) =
               trans_int_exp retTmp retLabel idToTmpMap int_exp1 in
            let (instrs_for_exp2, tmp_exp2) =
               trans_int_exp retTmp retLabel idToTmpMap int_exp2 in
            let priorInstr = TmpInfAddrBoolInstr
                 (TmpInfAddrCmp(tmp_exp2, tmp_exp1)) in
            instrs_for_exp1 @ instrs_for_exp2 @
              (make_cond_instrs retTmp retLabel idToTmpMap priorInstr stmtsForIf
                 stmtsForElse JG JLE )
       | A.LessThan (int_exp1, int_exp2) -> 
           (* NOTE THAT WE SWITCH THE ORDER BECAUSE CMP IS WEIRD *)
         (* MAKE SURE TO CHECK THIS. Pretty sure it's right though *)
            let (instrs_for_exp1, tmp_exp1) =
               trans_int_exp retTmp retLabel idToTmpMap int_exp1 in
            let (instrs_for_exp2, tmp_exp2) =
               trans_int_exp retTmp retLabel idToTmpMap int_exp2 in
            let priorInstr = TmpInfAddrBoolInstr
                 (TmpInfAddrCmp(tmp_exp2, tmp_exp1)) in
            instrs_for_exp1 @ instrs_for_exp2 @
              (make_cond_instrs retTmp retLabel idToTmpMap priorInstr stmtsForIf
                 stmtsForElse JL JGE )
       | A.IntEquals (int_exp1, int_exp2) ->
            let (instrs_for_exp1, tmp_exp1) =
               trans_int_exp retTmp retLabel idToTmpMap int_exp1 in
            let (instrs_for_exp2, tmp_exp2) =
               trans_int_exp retTmp retLabel idToTmpMap int_exp2 in
            let priorInstr = TmpInfAddrBoolInstr
            (TmpInfAddrCmp(tmp_exp2, tmp_exp1)) in
            instrs_for_exp1 @ instrs_for_exp2 @
              (make_cond_instrs retTmp retLabel idToTmpMap priorInstr stmtsForIf stmtsForElse
                 JE JNE )
       | A.BoolEquals (bool_exp1, bool_exp2) ->
            (* See description of trans_bool_exp retTmp retLabel *)
            let (instrs_for_exp1, tmp_exp1) =
               trans_bool_exp retTmp retLabel idToTmpMap bool_exp1 None in
            let (instrs_for_exp2, tmp_exp2) =
               trans_bool_exp retTmp retLabel idToTmpMap bool_exp2 None in
            let priorInstr = TmpInfAddrBoolInstr
                (TmpInfAddrCmp (TmpIntArg (TmpLoc tmp_exp1),
                                TmpIntArg (TmpLoc tmp_exp2))) in
            (* The order of instrs_for_exp1/2 shouldn't matter *)
            instrs_for_exp1 @ instrs_for_exp2 @
            (make_cond_instrs retTmp retLabel idToTmpMap priorInstr stmtsForIf
                 stmtsForElse JE JNE )
       | A.BoolIdent id ->  
          (match M.find idToTmpMap id with
             None -> 
             let () = print_string("Undeclared: " ^ id ^ "\n") in
             assert(false)
           | Some t -> let priorInstr = TmpInfAddrBoolInstr
                 (TmpInfAddrTest (TmpBoolArg (TmpLoc (Tmp t)),
                                  TmpBoolArg (TmpConst 1))) in
                   make_cond_instrs retTmp retLabel idToTmpMap priorInstr stmtsForIf
                      stmtsForElse JNE JE )
       | A.BoolFunCall(fName, argList) ->
           let (instrs, argExps) = trans_fun_args retTmp retLabel idToTmpMap argList in
           let result_id = GenUnusedID.create() in
           let result_tmp = Temp.create() in
           let newMap = M.add idToTmpMap result_id result_tmp in
           instrs @ TmpInfAddrMov(TmpBoolExpr (TmpInfAddrBoolFunCall(fName, argExps)),
                                    Tmp result_tmp)::[] @
           trans_cond retTmp retLabel newMap (A.BoolIdent result_id, stmtsForIf, stmtsForElse)

and trans_stmts retTmp retLabel idToTmpMap = function
     A.TypedPostElabDecl (id, idType)::stmts ->
      (* Just create a single temp per variable for now,
         instead of creating one each assignment *)
        let t = Temp.create() in
        let newMap = M.add idToTmpMap id t in
        trans_stmts retTmp retLabel newMap stmts
   | A.TypedPostElabAssignStmt (id, A.BoolExpr e)::stmts ->
        let (instrs_for_move, Tmp new_t) =
          (* trans_bool_exp gives us the instructions it generated, and also
             where it ended up putting the value (if it didn't have to
             create a new one, new_t will just be whatever we passed in.
             We then update the binding in idToTmpMap (which does nothing
             if new_t is what we passed in) *)
               trans_bool_exp retTmp retLabel idToTmpMap e (M.find idToTmpMap id) in
        let newMap = M.add idToTmpMap id new_t in
        instrs_for_move @ trans_stmts retTmp retLabel newMap stmts
   | A.TypedPostElabAssignStmt (id, A.IntExpr e)::stmts ->
        let (instrs_for_e, eInfAddr) = trans_int_exp retTmp retLabel idToTmpMap e in
        let t = get_or_make_tmp id idToTmpMap in
        let newMap = M.add idToTmpMap id t in
        instrs_for_e @
        TmpInfAddrMov (TmpIntExpr eInfAddr, Tmp t)::trans_stmts retTmp retLabel newMap stmts
   | A.TypedPostElabIf (e, ifStmts, elseStmts) :: stmts ->
       trans_cond retTmp retLabel idToTmpMap (e, ifStmts, elseStmts) @
        trans_stmts retTmp retLabel idToTmpMap stmts
   | A.TypedPostElabWhile (e, whileStmts) :: stmts ->
        let topLabel = GenLabel.create() in
        let jumpToTopStmt = A.JumpUncond(topLabel) in
        (* Empty list because no else statements *)
        TmpInfAddrLabel topLabel::
        trans_cond retTmp retLabel idToTmpMap (e, whileStmts @ [jumpToTopStmt], []) @
        trans_stmts retTmp retLabel idToTmpMap stmts
   | A.JumpUncond target :: stmts ->
     (* This really shouldn't be an AST instruction
        but I need it for toInfAddr :( *)
        TmpInfAddrJump(JMP_UNCOND, target)::trans_stmts retTmp retLabel idToTmpMap stmts
   | A.TypedPostElabReturn e :: stmts ->
        let (instrs_for_e, eInfAddr) = trans_exp retTmp retLabel idToTmpMap e in
        (* Move our value to the ret tmp, then jump to the return *)
        instrs_for_e @
        TmpInfAddrMov(eInfAddr, retTmp)::
        TmpInfAddrJump(JMP_UNCOND, retLabel) :: []
   | A.TypedPostElabAssert e :: stmts ->
     (* If e is true we proceed on, else we call the abort function, with
     no arguments *)
        let callAbortAst = A.TypedPostElabIf (e, stmts,
                                     [A.VoidFunCall("abort", [])])::[] in
        trans_stmts retTmp retLabel idToTmpMap callAbortAst
   | A.VoidFunCall (fName, argList)::stmts ->
        let (instrs, argExps) = trans_fun_args retTmp retLabel idToTmpMap argList in
        instrs @ TmpInfAddrVoidFunCall(fName, argExps)::[]
        @ trans_stmts retTmp retLabel idToTmpMap stmts
   | A.TypedPostElabVoidReturn::stmts -> TmpInfAddrJump(JMP_UNCOND, retLabel)::[]
   | [] -> []

(* We assume that this is run after typechecking, so everything is
   declared initialized, etc *)

let initIdToTmpMap params =
  (* Create temps for each of the params, and init the idToTmpMap
     accordingly *)
    let params_with_tmps = List.map params (fun (_,id) -> (id, Temp.create())) in
    match String.Map.of_alist params_with_tmps with
         `Duplicate_key _ -> failwith "params must have unique names"
       | `Ok (result) -> result


let trans_global_decl decl =
  (* Each function has a unique retTmp and retLabel *)
    let retLabel = GenLabel.create() in
    let retTmp = Tmp (Temp.create()) in
    match decl with
        A.TypedPostElabFunDef (typee, fName, params, stmts) ->
            let idToTmpMap = initIdToTmpMap params in
            let param_tmp_list = List.map params (fun (_, id) ->
                   match M.find idToTmpMap id with
                       Some t -> Tmp t
                     | None -> assert(false)) in
            let infAddrStmts = trans_stmts retTmp retLabel idToTmpMap stmts in
            let finalStmts = infAddrStmts @ TmpInfAddrLabel retLabel ::
            TmpInfAddrReturn (TmpIntExpr (TmpIntArg (TmpLoc (retTmp))))::[] in
            TmpInfAddrFunDef (fName, param_tmp_list, finalStmts)

let toInfAddr (funDefList: A.typedPostElabAST) =
     (* We will have a simple return label that all returns jump to *)
     List.map funDefList trans_global_decl
