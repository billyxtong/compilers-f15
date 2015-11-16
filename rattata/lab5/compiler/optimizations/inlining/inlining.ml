open Datatypesv1
module H = Hashtbl

let isRecCall fName = function
    Tmp3AddrFunCall(retSize, callName, args, dest) -> callName = fName
  | _ -> false                                                      

(* maps fName to tmpParam list, instrs, whether it's recursive *)
let rec makeFunDefMap map = function
    [] -> map
  | Tmp3AddrFunDef (fName, params, instrs) :: rest ->
      let isRec = List.exists (isRecCall fName) instrs in
      let () = H.add map fName (params, instrs, isRec) in
      makeFunDefMap map rest

let isNotRet = function
    Tmp3AddrReturn _ -> false
  | _ -> true

let shouldInline funDefs fName =
    try let (params, instrs, isRec) = H.find funDefs fName in
    List.length instrs < 10 && not isRec
    with Not_found -> (* not a function that we're compiling *) false

let rec getParamInstrs args params =
    match (args, params) with
         ([], []) -> []
       | (arg::argsRest, (param, pSize)::paramsRest) ->
             Tmp3AddrMov(pSize, arg, TmpVar param)::
             getParamInstrs argsRest paramsRest
       | _ -> assert(false)               

let rec getInstrsWithNewLabels labelMap = function
    [] -> []
  | Tmp3AddrLabel lbl :: rest ->
       (try let newLbl = H.find labelMap lbl in Tmp3AddrLabel newLbl
        with Not_found -> let newLbl = GenLabel.create () in
                          let () = H.add labelMap lbl newLbl in
                            Tmp3AddrLabel newLbl) ::
                         getInstrsWithNewLabels labelMap rest
  | Tmp3AddrJump (j, lbl) :: rest ->
       (try let newLbl = H.find labelMap lbl in Tmp3AddrJump(j, newLbl)
        with Not_found -> let newLbl = GenLabel.create () in
                          let () = H.add labelMap lbl newLbl in
                            Tmp3AddrJump (j, newLbl)) ::
                         getInstrsWithNewLabels labelMap rest
  | instr :: rest -> instr :: getInstrsWithNewLabels labelMap rest

let handleInstrForFunDef funDefMap = function
    Tmp3AddrFunCall (retSize, fName, args, dest) ->
        if not (shouldInline funDefMap fName) then
          Tmp3AddrFunCall (retSize, fName, args, dest) :: [] else
        let (funParams, funInstrs, isRec) = H.find funDefMap fName in
        let Tmp3AddrReturn (_, retArg) =
             List.nth funInstrs ((List.length funInstrs) - 1) in
        let instrsSansRet = List.filter isNotRet funInstrs in
        let instrsNewLabels = getInstrsWithNewLabels (H.create 100) instrsSansRet in
        let paramInstrs = getParamInstrs args funParams in
        let inlinedRetInstr = (match dest with
                     Some retDest -> Tmp3AddrMov(retSize, retArg, retDest)::[]
                   | None -> []) in
        paramInstrs @ instrsNewLabels @ inlinedRetInstr
  | instr -> instr::[]        

let handleFunDef funDefMap (Tmp3AddrFunDef (fName, params, instrs)) =
    Tmp3AddrFunDef (fName, params,
                    List.concat (List.map (handleInstrForFunDef funDefMap) instrs))

let inlineFuncs funDefs =
    let funDefMap = makeFunDefMap (H.create 10) funDefs in
    List.map (handleFunDef funDefMap) funDefs
