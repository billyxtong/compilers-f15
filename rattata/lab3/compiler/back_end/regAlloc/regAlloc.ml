open Datatypesv1

let rec regAlloc (funDefs: tmp2AddrProg) : assemProg =
    match funDefs with
       [] -> []
     | Tmp2AddrFunDef(fName, args, dest)::rest ->
         let instrsForFun = AllocForFun.AllocForFun
               (Tmp2AddrFunDef(fName, args, dest)) in
         AssemFunDef(fName, instrsForFun)::regAlloc rest
    
