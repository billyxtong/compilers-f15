open Datatypesv1
module H = Hashtbl

let makeFuncToParamSizeMap map = function
    [] -> ()
  | Tmp2AddrFunDef(fName, params, dest)::rest ->
    let paramSizes = List.map (fun (t, paramSize) -> paramSize) params in
    H.add map fName (Array.of_list paramSizes)

let rec regAllocRec (funDefs: tmp2AddrProg) funcToParamSizeMap : assemProg =
    match funDefs with
       [] -> []
     | Tmp2AddrFunDef(fName, args, dest)::rest ->
         let newFunDef = AllocForFun.allocForFun
               (Tmp2AddrFunDef(fName, args, dest)) funcToParamSizeMap in
         newFunDef ::regAllocRec rest funcToParamSizeMap

let regAlloc (funDefs: tmp2AddrProg) : assemProg =
    let funcToParamSizeMap = makeFuncToParamSizeMap
        (H.create (List.length funDefs)) funDefs in
    regAllocRec funDefs funcToParamSizeMap
