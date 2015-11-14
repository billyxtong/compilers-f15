open Datatypesv1
module H = Hashtbl    

let getDefVar = function
    Tmp3AddrMov (opSize, src, TmpVar t) -> Some t
  | Tmp3AddrPtrBinop (op, arg1, arg2, TmpVar t) -> Some t
  | Tmp3AddrBinop (op, arg1, arg2, TmpVar t) -> Some t
  | Tmp3AddrMaskUpper t -> Some t
  | Tmp3AddrFunCall (retSize, fName, args, Some (TmpVar t)) -> Some t
  | _ -> None                                                                 
                                                                 

let rec getTempSetMultipleDefs resultSet defdSet = function
    [] -> resultSet
  | instr :: rest ->
       let () = (match getDefVar instr with
             Some t -> (try let () = H.find defdSet t in H.replace resultSet t ()
                            (* if it has already been defd, it now has multiple *)
                        with Not_found -> H.add defdSet t ())
                            (* otherwise it has now been defd *)
           | None -> ()) in
       getTempSetMultipleDefs resultSet defdSet rest

let transArg tmpToConstMap = function
    TmpLoc (TmpVar t) -> (try TmpConst (H.find tmpToConstMap t)
                          with Not_found -> TmpLoc (TmpVar t))
  | arg -> arg                         

let handleMove multDefdTmps tmpToConstMap = function
    (* only propoage bit32 for now: the only bit64 constant is NULL anyway *)
    (BIT32, TmpConst c, TmpVar t) ->
    (* If this tmp is only defd once, remove this instruction, and now t maps to c *)
    (try let () = H.find multDefdTmps t
         in Tmp3AddrMov (BIT32, TmpConst c, TmpVar t)::[]
     with Not_found -> let () = H.replace tmpToConstMap t c in [])
  | (opSize, TmpLoc (TmpVar tSrc), TmpVar tDest) ->
       (* if tSrc is mapped to a constant, then we can get rid of this
          instruction, and tDest also maps to that constant *)
       (try let c = H.find tmpToConstMap tSrc in
            let () = H.replace tmpToConstMap tDest c in []
        with Not_found -> (* kill the current prior mapping of tDest if any,
                           and keep this instruction *)
                let () = H.remove tmpToConstMap tDest in
                Tmp3AddrMov (opSize, TmpLoc (TmpVar tSrc), TmpVar tDest)::[])
  | (opSize, TmpLoc (TmpDeref tSrc), TmpVar tDest) ->
       (* kill the current mapping of t *)
       let () = H.remove tmpToConstMap tDest in
       Tmp3AddrMov (opSize, TmpLoc (TmpDeref tSrc), TmpVar tDest)::[]
  | (opSize, src, dest) -> Tmp3AddrMov (opSize, src, dest)::[]       

let handleMustBeATmp tmpToConstMap tmpSize t =
    try let c = H.find tmpToConstMap t in
        let storeInstr = Tmp3AddrMov (tmpSize, TmpConst c, TmpVar t) in
        storeInstr::[]
    with Not_found -> []

let rec handleInstrsForFunDef multDefdTmps tmpToConstMap = function
    [] -> []
  | Tmp3AddrMov (opSize, src, dest) :: rest ->
       let moveResult = handleMove multDefdTmps tmpToConstMap (opSize, src, dest) in
       (* we have to store the moveResult first, because handleMove will
          actually modify tmpToConstMap *)
       moveResult @ handleInstrsForFunDef multDefdTmps tmpToConstMap rest
  | Tmp3AddrBinop (op, arg1, arg2, dest) :: rest ->
      let new_arg1 = transArg tmpToConstMap arg1 in
      let new_arg2 = transArg tmpToConstMap arg2 in
      (* kill current mapping of dest, if any *)
      let () = (match dest with
                      TmpVar t -> H.remove tmpToConstMap t
                     | _ -> ()) in
      Tmp3AddrBinop (op, new_arg1, new_arg2, dest)
      :: handleInstrsForFunDef multDefdTmps tmpToConstMap rest
  | Tmp3AddrReturn (retSize, arg) :: rest ->
      Tmp3AddrReturn (retSize, transArg tmpToConstMap arg)
      :: handleInstrsForFunDef multDefdTmps tmpToConstMap rest
  | Tmp3AddrLabel lbl :: rest ->
      (* since we only constant propogate for tmps that are defined just once,
         we can ignore labels too *)
      Tmp3AddrLabel lbl :: handleInstrsForFunDef multDefdTmps tmpToConstMap rest
  | Tmp3AddrJump j :: rest ->
      (* we can actually just ignore jumps *)
      Tmp3AddrJump j :: handleInstrsForFunDef multDefdTmps tmpToConstMap rest
  | Tmp3AddrMaskUpper t :: rest ->
           (* anything we mask the upper of is 32-bit *)
       let storeInstrs = handleMustBeATmp tmpToConstMap BIT32 t in
           (* This actually does need to be stored in a tmp :/ *)
       storeInstrs @ Tmp3AddrMaskUpper t :: []
       @ handleInstrsForFunDef multDefdTmps tmpToConstMap rest
  | Tmp3AddrPtrBinop (op, arg1, arg2, dest) :: rest ->
      let new_arg1 = transArg tmpToConstMap arg1 in
      let new_arg2 = transArg tmpToConstMap arg2 in
      (* kill current mapping of dest, if any *)
      let () = (match dest with
                      TmpVar t -> H.remove tmpToConstMap t
                     | _ -> ()) in
      Tmp3AddrPtrBinop (op, new_arg1, new_arg2, dest)
      :: handleInstrsForFunDef multDefdTmps tmpToConstMap rest
  | Tmp3AddrFunCall (retSize, fName, args, dest) :: rest ->
      let new_args = List.map (transArg tmpToConstMap) args in
      (* kill current mapping of dest, if any *)
      let () = (match dest with
                      Some (TmpVar t) -> H.remove tmpToConstMap t
                     | _ -> ()) in
      Tmp3AddrFunCall (retSize, fName, new_args, dest)
      :: handleInstrsForFunDef multDefdTmps tmpToConstMap rest
  | Tmp3AddrBoolInstr (TmpTest (arg, TmpVar t)) :: rest ->
      let new_arg = transArg tmpToConstMap arg in
      let storeInstr = handleMustBeATmp tmpToConstMap BIT32 t in
      storeInstr @ Tmp3AddrBoolInstr (TmpTest (new_arg, TmpVar t))
      :: handleInstrsForFunDef multDefdTmps tmpToConstMap rest
  | Tmp3AddrBoolInstr (TmpCmp (opSize, arg, TmpVar t)) :: rest ->
      let new_arg = transArg tmpToConstMap arg in
      let storeInstr = handleMustBeATmp tmpToConstMap opSize t in
      storeInstr @ Tmp3AddrBoolInstr (TmpCmp (opSize, new_arg, TmpVar t))
      :: handleInstrsForFunDef multDefdTmps tmpToConstMap rest
  | Tmp3AddrBoolInstr (TmpTest (arg, loc)) :: rest ->
      let new_arg = transArg tmpToConstMap arg in
      Tmp3AddrBoolInstr (TmpTest (new_arg, loc))
      :: handleInstrsForFunDef multDefdTmps tmpToConstMap rest
  | Tmp3AddrBoolInstr (TmpCmp (opSize, arg, loc)) :: rest ->
      let new_arg = transArg tmpToConstMap arg in
      Tmp3AddrBoolInstr (TmpCmp (opSize, new_arg, loc))
      :: handleInstrsForFunDef multDefdTmps tmpToConstMap rest
          
let handleFunDef (Tmp3AddrFunDef (fName, params, instrs)) =
    (* maps tmps that just hold constants to the constant they hold *)
    let tmpToConstMap = H.create 100 in
    let multDefdTmps = getTempSetMultipleDefs (H.create 100) (H.create 100) instrs in
    Tmp3AddrFunDef (fName, params, handleInstrsForFunDef multDefdTmps
                      tmpToConstMap instrs)

let constPropAndFold funDefs =
    List.map handleFunDef funDefs
