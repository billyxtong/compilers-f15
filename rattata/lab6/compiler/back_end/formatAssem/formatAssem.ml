
(* Format the assemProgWonky into a string that can be written to
   a .s file. Currently really simple. *)

let formatAssem finalprog =
    (* String.concat "" [".globl _c0_main\n"; "_c0_main:\n"; *)
                      PrintDatatypes.assemProgWonkyToString finalprog
