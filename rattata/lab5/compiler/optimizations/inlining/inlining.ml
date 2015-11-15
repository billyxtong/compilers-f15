open Datatypesv1
module H = Hashtbl
  
(* maps fName to tmpParam list, instrs *)
let rec makeFunDefMap map = function
    [] -> map
  | Tmp3AddrFunDef (fName, params, instrs) :: rest ->
      let () = H.add map fName (params, instrs) in
      makeFunDefMap map rest

let isNotRet = function
    Tmp3AddrReturn _ -> false
  | _ -> true

let shouldInline funDefs fName =
    let (params, instrs) = H.find funDefs fName in
    List.length instrs < 10

let rec getParamInstrs args params =
    match (args, params) with
         ([], []) -> []
       | (arg::argsRest, (param, pSize)::paramsRest) ->
             Tmp3AddrMov(pSize, arg, TmpVar param)::
             getParamInstrs argsRest paramsRest
       | _ -> assert(false)               

let handleInstrForFunDef funDefMap = function
    Tmp3AddrFunCall (retSize, fName, args, dest) ->
        let (funParams, funInstrs) = H.find funDefMap fName in
        if shouldInline funDefMap fName then Tmp3AddrFunCall (retSize, fName,
                                                            args, dest) :: [] else
        let Tmp3AddrReturn (_, retArg) =
             List.nth funInstrs ((List.length funInstrs) - 1) in
        let instrsSansRet = List.filter isNotRet funInstrs in
        let paramInstrs = getParamInstrs args funParams in
        let inlinedRetInstr = (match dest with
                     Some retDest -> Tmp3AddrMov(retSize, retArg, retDest)::[]
                   | None -> []) in
        paramInstrs @ instrsSansRet @ inlinedRetInstr
  | instr -> instr::[]        

let handleFunDef funDefMap (Tmp3AddrFunDef (fName, params, instrs)) =
    Tmp3AddrFunDef (fName, params,
                    List.concat (List.map (handleInstrForFunDef funDefMap) instrs))

let inlineFuncs funDefs =
    let funDefMap = makeFunDefMap (H.create 10) funDefs in
    List.map (handleFunDef funDefMap) funDefs
