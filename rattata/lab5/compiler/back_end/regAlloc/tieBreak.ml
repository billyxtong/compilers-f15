open Datatypesv1
module H = Hashtbl    
    
(* let rec makeLoopDepthMap map depth = function *)
(*     [] -> [] *)
(*   | Tmp2AddrBoolInstr *)

let tmpArgUses = function
    TmpLoc (TmpVar (Tmp t)) -> t::[]
  | TmpLoc (TmpDeref (Tmp t)) -> t::[]
  | _ -> []

let tmpLocUses = function
    TmpVar (Tmp t) -> t::[]
  | TmpDeref (Tmp t) -> t::[]

let instrUses = function
    Tmp2AddrMov(opSize, arg, loc) -> tmpArgUses arg @ tmpLocUses loc
  | Tmp2AddrPtrBinop(op, arg, loc) -> tmpArgUses arg @ tmpLocUses loc        
  | Tmp2AddrBinop(op, arg, loc) -> tmpArgUses arg @ tmpLocUses loc        
  | Tmp2AddrReturn(retSize, arg) -> tmpArgUses arg
  | Tmp2AddrJump _ -> []
  | Tmp2AddrLabel _ -> []
  | Tmp2AddrMaskUpper (Tmp t) -> t :: []
  | Tmp2AddrFunCall(retSize, fName, args, dest) ->
       let destUses = (match dest with None -> []
                             | Some dest' -> tmpLocUses dest') in
       let argUses = List.concat (List.map tmpArgUses args) in
       destUses @ argUses
  | Tmp2AddrBoolInstr(TmpTest (arg, loc)) -> tmpArgUses arg @ tmpLocUses loc
  | Tmp2AddrBoolInstr(TmpCmp (opSize, arg, loc)) -> tmpArgUses arg @ tmpLocUses loc

let incrMap map t =
    (try let currCount = H.find map t in
         H.replace map t (currCount + 1)
     with Not_found -> H.add map t 1)
                      
let rec makeNumUsesMap map = function
   [] -> map
 | instr :: rest ->
      let usesInInstr = instrUses instr in
      let () = List.iter (fun t -> incrMap map t) usesInInstr in
      makeNumUsesMap map rest
        
let tieBreakFunc t1 t2 = t2 - t1

let getTieBreakFunc instrs =
    let numUsesMap = makeNumUsesMap (H.create 100) instrs in
    fun t1 -> fun t2 -> (H.find numUsesMap t2 - H.find numUsesMap t1)
