open Datatypesv1
module H = Hashtbl    

let binopTo2Addr dest = function
    (op, arg1, arg2) ->
       let t = Tmp(Temp.create()) in
       (* The order of where arg1 and arg2 go here is kind of
          weird: remember it's Tmp2AddrBinop(op, dest, src) *)
       let op_instr = Tmp2AddrBinop(op, arg2, t) in
       Tmp2AddrMov(arg1, t)::op_instr::
       [Tmp2AddrMov(TmpLoc t, dest)]

let trans_arg paramToTmpMap arg =
    try TmpLoc (H.find paramToTmpMap arg)
    with
        Not_found -> arg

let trans_loc paramToTmpMap loc =
    try H.find paramToTmpMap (TmpLoc loc)
    with
        Not_found -> loc

let instrTo2Addr paramToTmpMap = function
    Tmp3AddrMov (src, dest) -> [Tmp2AddrMov (trans_arg paramToTmpMap src,
                                             trans_loc paramToTmpMap dest)]
  | Tmp3AddrReturn arg -> [Tmp2AddrReturn (trans_arg paramToTmpMap arg)]
  | Tmp3AddrBinop (op, arg1, arg2, dest) ->
     binopTo2Addr dest (op, trans_arg paramToTmpMap arg1,
                        trans_arg paramToTmpMap arg2)
  | Tmp3AddrJump j -> Tmp2AddrJump j::[]
  | Tmp3AddrLabel jumpLabel -> Tmp2AddrLabel jumpLabel::[]
  | Tmp3AddrBoolInstr (TmpTest (arg, loc)) ->
        Tmp2AddrBoolInstr (TmpTest (trans_arg paramToTmpMap arg,
                                    trans_loc paramToTmpMap loc))::[]
  | Tmp3AddrBoolInstr (TmpCmp (arg, loc)) ->
        Tmp2AddrBoolInstr (TmpCmp (trans_arg paramToTmpMap arg,
                                    trans_loc paramToTmpMap loc))::[]
  | Tmp3AddrFunCall (fName, args, dest) ->
      (* At the beginning of the function, move all of the args into new tmps.
         Keep live ranges of pre-colored tmps short! We're doing it here because
         we only have to match against one constructor here *)
         
      Tmp2AddrFunCall (fName, args, dest)::[]

let rec funTo2Addr paramToTmpMap = function
   [] -> []
 | instr::instrs -> instrTo2Addr paramToTmpMap instr @ funTo2Addr paramToTmpMap instrs

(* Returns (moves, map) where moves is a list of moves that move params into
   new tmps. This keeps the live ranges of pre-colored tmps short! map
   maps tmps that were params to the new tmp (the one it ends up in after the moves

   This is important because we need to still be able to use ECX for shifts and
   EDI for division, even though they will contain fun args *)
let rec setUpParams params map =
    match params with
       [] -> []
     | param::rest ->
         let t = Tmp (Temp.create()) in
         let () = H.add map (TmpLoc param) t in
         Tmp2AddrMov(TmpLoc param, t)::setUpParams rest map

let rec to2Addr = function
   [] -> []
 | Tmp3AddrFunDef(fName, params, instrs)::rest ->
   let paramToTmpMap = H.create 100 in
   let setUpInstrs = setUpParams params paramToTmpMap in
   let funInstrs = funTo2Addr paramToTmpMap instrs in
   Tmp2AddrFunDef(fName, params, setUpInstrs @ funInstrs)
   :: to2Addr rest
