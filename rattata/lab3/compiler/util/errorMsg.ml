(* L1 Compiler
 * Error messages
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Annotations: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

open Core.Std

let anyErrors = ref false

let reset () = anyErrors := false

let msg str note =
  anyErrors := true;
  List.iter [":"; str; ":"; note; "\n"] ~f:print_string

let error note =
  anyErrors := true;
  msg "error" note

let warn note = msg "warning" note

exception Error
