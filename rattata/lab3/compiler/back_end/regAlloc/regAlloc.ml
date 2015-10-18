open Datatypesv1

let rec regAlloc (funDefs: tmp2AddrProg) : assemProg =
    match funDefs with
       [] -> []
     | Tmp2AddrFunDef(fName, args, dest)::rest ->
         let newFunDef = AllocForFun.allocForFun
               (Tmp2AddrFunDef(fName, args, dest)) in
         newFunDef ::regAlloc rest
    
