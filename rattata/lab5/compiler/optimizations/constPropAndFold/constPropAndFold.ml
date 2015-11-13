open Datatypesv1
module H = Hashtbl    

let rec handleInstrsForFunDef tmpToConstMap = function
    [] -> []
  (* | Tmp3AddrMov (opSize, src, TmpVar t) :: rest -> *)
     
      
            

let handleFunDef (Tmp3AddrFunDef (fName, params, instrs)) =
    (* maps tmps that just hold constants to the constant they hold *)
    let tmpToConstMap = H.create 100 in
    Tmp3AddrFunDef (fName, params, handleInstrsForFunDef tmpToConstMap instrs)

let constPropAndFold funDefs =
    List.map handleFunDef funDefs
