module H = Hashtbl
open Datatypesv1  

let isLive liveSet line = try let () = H.find liveSet line in true
                          with Not_found -> false

let isDef t prog line =
    match Array.get prog line with
         Tmp2AddrMov(src, dest) -> (dest = Tmp t)
       | Tmp2AddrBinop(op, src, dest) -> (dest = Tmp t)
       | _ -> false

let isUsed t prog line =
    match Array.get prog line with
         Tmp2AddrMov(src, dest) -> (src = TmpLoc (Tmp t))
       | Tmp2AddrBinop(op, src, dest) -> ((src = (TmpLoc (Tmp t)))
                                           || (dest = Tmp t))
       | Tmp2AddrReturn arg -> (arg = TmpLoc (Tmp t))
       | Tmp2AddrBoolInstr (TmpCmp (arg, loc)) ->
              ((arg = (TmpLoc (Tmp t))) || (loc = Tmp t))
       | Tmp2AddrBoolInstr (TmpTest (arg, loc)) ->
              ((arg = (TmpLoc (Tmp t))) || (loc = Tmp t))
       | _ -> false

let rec findLiveLines t prog liveLinesSet currLine succLine =
    if isLive liveLinesSet currLine then liveLinesSet
        (* If it's already declared live on this line, we're done *)
    else if isLive

let addLineToList line () acc = line::acc

let findLiveLinesForTmp t prog predsPerLine =
    let startLine = (List.length prog) - 1 in
    let result = H.create 500 in
    let liveLinesSet = findLiveLines t prog result startLine startLine
    in H.fold addLineToList liveLinesSet []
