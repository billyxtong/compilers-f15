(* Removes unnecessary chains of moves, such as
   movl a b
   movl b c
   movl c a
   which might not be ok if the next line where addl b a or something,
   where the fact that we moved a to b matters. We'll see I guess :P *)
open Datatypesv1

let rec handleMove curr_instrs start instrs (src, dest) =
    let newStartFromSrc = (match src with
                                Const _ -> None
                              | AssemLoc loc -> Some loc) in
    (* It can be the new start as long as it's not a constant *)
    match start with
        None -> condenseMovesRec (AssemInstr (MOV (src,dest))::curr_instrs)
                  newStartFromSrc instrs
      | Some startLoc -> (match src with
            (* if it's a constant, it breaks the chain *)
             Const _ -> curr_instrs @ [AssemInstr (MOV(src, dest))] @
                        condenseMovesRec [] None instrs
           | AssemLoc srcLoc ->
                (if startLoc = dest then (* Chain ends, and remove it *)
                condenseMovesRec [] None instrs (* Otherwise, chain continues *)
                else condenseMovesRec (curr_instrs @
                    AssemInstr (MOV(src,dest))::[]) start
                     instrs))

and condenseMovesRec curr_instrs start = function
     [] -> []
   | (AssemInstr (MOV (src, dest))::instrs) ->
     handleMove curr_instrs start instrs (src, dest)
   | instr::instrs -> (* anything else breaks the chain *)
         curr_instrs @ [instr] @ condenseMovesRec [] None instrs

(* curr_instrs is the current run of moves we're on *)
let condenseMoves instrs = condenseMovesRec [] None instrs

