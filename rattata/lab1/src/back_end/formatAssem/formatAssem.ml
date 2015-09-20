
(* Format the assemProgWonky into a string that can be written to
   a .s file. Currently really simple. *)

let formatAssem finalprog =
    String.concat "" [".globl main\n"; "main:\n";
                      PrintDatatypes.assemProgWonkyToString finalprog]
