(* L1 Compiler
 * Temporaries
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

open Core.Std

type t

include Comparable.S with type t := t

val reset : unit -> unit              (* resets temp numbering *)
val create : unit -> t             (* returns a unique new temp *)
val name : t -> string             (* returns the name of a temp *)
val compare : t -> t -> int     (* comparison function *)

val format : Format.formatter -> t -> unit
