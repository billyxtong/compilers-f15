open Datatypesv1

let rec removeNullMoves = function
    [] -> []
  | instr::instrs -> (match instr with
                        (AssemInstr (MOV (src, dest))) ->
                            (if src = AssemLoc dest then removeNullMoves instrs
                             else instr::removeNullMoves instrs)
                     |  _ -> instr::removeNullMoves instrs)

let rec condenseMovesDoubles = function
     [] -> []
   | instr::[] -> instr::[]
   | instr1::instr2::instrs ->
        match (instr1, instr2) with
             (AssemInstr (MOV (src1, dest1)),
              AssemInstr (MOV (src2, dest2))) ->
     if ((AssemLoc dest1) = src2) && ((AssemLoc dest2) = src1)
     then instr1 :: condenseMovesDoubles instrs
                   else instr1 :: condenseMovesDoubles(instr2::instrs)
           | _ -> instr1 :: condenseMovesDoubles(instr2::instrs)

let condenseMoves instrs = condenseMovesDoubles(removeNullMoves instrs)
