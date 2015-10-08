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

let rec findLiveLinesRec t prog predsPerLine liveLinesSet succLine currLine =
    if isLive liveLinesSet currLine then liveLinesSet
        (* If it's already declared live on this line, we're done *)
    else let () =
       (* Add it to liveLinesSet if it's live, otherwise do nothing. *)
                (if (isLive liveLinesSet succLine && not (isDef t prog currLine))
                  || isUsed t prog currLine (* It's live on this line *)
                then H.add liveLinesSet currLine ()) in
         let () = (if isDef t prog currLine
            (* if it's defined on this line it's always live on the next line,
               I think. This is to deal with temps that are assigned but
               never used *)
                   then H.add liveLinesSet succLine ()) in
          (* Then make the recurisve calls in both cases *)
          let _ = List.map (findLiveLinesRec t prog predsPerLine
                              liveLinesSet currLine)
                 (Array.get predsPerLine currLine) in liveLinesSet
          


let addLineToList line () acc = line::acc

let findLiveLines t prog predsPerLine =
    let startLine = (Array.length prog) - 1 in
    let result = H.create 500 in
    let liveLinesSet = findLiveLinesRec t prog predsPerLine
                       result startLine startLine
    in H.fold addLineToList liveLinesSet []
