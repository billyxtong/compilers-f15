open Datatypesv1
module H = Hashtbl

let rec makeFuncToParamSizeMap map = function
    [] -> map
  | Tmp2AddrFunDef(fName, params, dest)::rest ->
    let paramSizes = List.map (fun (t, paramSize) -> paramSize) params in
    let () = H.add map fName (Array.of_list paramSizes) in
    makeFuncToParamSizeMap map rest
    

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
    (* Need to also built-functions calloc and abort *)
    let () = H.add funcToParamSizeMap "calloc" (Array.of_list (BIT32::BIT32::[])) in
    let () = H.add funcToParamSizeMap "abort" (Array.of_list (BIT32::[])) in
    regAllocRec funDefs funcToParamSizeMap
