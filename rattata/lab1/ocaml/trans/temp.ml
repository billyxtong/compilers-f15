(* L1 Compiler
 * Temporaries
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
*)

open Core.Std

module T = struct
  type t = int with compare, sexp
end

include T

let counter = ref 1

let reset () = counter := 1
let create () =
  let t = !counter in
  let () = counter := !counter + 1 in
  t

let name t = "%t" ^ string_of_int t
let compare a b = compare a b

let format ff t = Format.fprintf ff "%s" (name t)

include Comparable.Make(T)
