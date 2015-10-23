(* Puts the program into legitimate assembly .s file syntax,
   ready to be written to file *)
open Datatypesv1
val formatAssem: assemProgWonky -> string
