open Datatypesv1
module H = Hashtbl

let inlinedInstrsMap = H.create 10 (* basically memoizing inlining calls *)

let inlineMaxLength = 50
let maxDepth = 3

(* returns true if targetName is recursive up to depth maxDepth, starting
   the search from fName *)
let rec funIsRecursive maxSearchDepth funDefMap targetName depth fName =
    if depth >= maxSearchDepth then false else
    try (let (params, instrs, isSingleRec) = H.find funDefMap fName in
         let funCalls = List.filter (function Tmp3AddrFunCall _ -> true | _ -> false)
             instrs in
         let calledFunNames = List.map
         (function Tmp3AddrFunCall (retSize, fName, args, dest) -> fName) funCalls in
         List.exists (fun s -> s = targetName) calledFunNames
         || List.exists (funIsRecursive maxSearchDepth funDefMap targetName (depth + 1))
           calledFunNames)
     with Not_found -> (* external function, we can just assume they're not recursive
                          I guess, we won't inline them anyway*) false

let updateWithRecursionStatus funDefMap fName (params, instrs, wasRecBefore) =
    let maxSearchDepth = H.length funDefMap in
    let isActuallyRec = funIsRecursive maxSearchDepth funDefMap fName 0 fName in
    H.replace funDefMap fName (params, instrs, isActuallyRec)

let isRecCall targetName = function
    Tmp3AddrFunCall(retSize, callName, args, dest) -> callName = targetName
  | _ -> false                                                      

(* maps fName to tmpParam list, instrs, whether it's recursive *)
let rec makeFunDefMap map = function
    [] -> map
  | Tmp3AddrFunDef (fName, params, instrs) :: rest ->
      let isRecForNow = false in (* we will search for recursion later *)
      let () = H.add map fName (params, instrs, isRecForNow) in
      makeFunDefMap map rest

let isNotRet = function
    Tmp3AddrReturn _ -> false
  | _ -> true

let shouldInline funDefs fName =
    try let (params, instrs, isRec) = H.find funDefs fName in
    List.length instrs < inlineMaxLength && not isRec
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

let rec handleInstrsForFunDef depth funDefMap = function
    [] -> []
  | Tmp3AddrFunCall (retSize, fName, args, dest) :: rest ->
        if not (shouldInline funDefMap fName) then
          Tmp3AddrFunCall (retSize, fName, args, dest) ::
          handleInstrsForFunDef depth funDefMap rest else
        let (funParams, funInstrs, false) = H.find funDefMap fName in
        (* first, inline those instrs *)
        let inlinedInstrs = (if depth < maxDepth then
                  getInlinedInstrsMemoized (depth + 1) fName funInstrs funDefMap
                             else funInstrs) in
        let Tmp3AddrReturn (_, retArg) =
             List.nth inlinedInstrs ((List.length inlinedInstrs) - 1) in
        let instrsSansRet = List.filter isNotRet inlinedInstrs in
        let instrsNewLabels = getInstrsWithNewLabels (H.create 100) instrsSansRet in
        let paramInstrs = getParamInstrs args funParams in
        let inlinedRetInstr = (match dest with
                     Some retDest -> Tmp3AddrMov(retSize, retArg, retDest)::[]
                   | None -> []) in
        paramInstrs @ instrsNewLabels @ inlinedRetInstr
        @ handleInstrsForFunDef depth funDefMap rest
  | instr :: rest -> instr :: handleInstrsForFunDef depth funDefMap rest

and getInlinedInstrsMemoized depth fName instrs funDefMap =
    let newInstrs = (try H.find inlinedInstrsMap fName
              with Not_found -> handleInstrsForFunDef depth funDefMap instrs) in
    let () = H.replace inlinedInstrsMap fName newInstrs in
    newInstrs

let handleFunDef funDefMap (Tmp3AddrFunDef (fName, params, instrs)) =
    let newInstrs = getInlinedInstrsMemoized 0 fName instrs funDefMap in
    Tmp3AddrFunDef (fName, params, newInstrs)

let inlineFuncs funDefs =
    let funDefMap = makeFunDefMap (H.create 10) funDefs in
    let () = H.iter (updateWithRecursionStatus funDefMap) funDefMap in
    List.map (handleFunDef funDefMap) funDefs
