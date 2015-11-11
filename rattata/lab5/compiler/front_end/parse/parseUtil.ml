(* We have to have this in a separate file because
   of circular build errors :( *)
let parsingTypedefMap : (string, unit) Hashtbl.t = Hashtbl.create 50
