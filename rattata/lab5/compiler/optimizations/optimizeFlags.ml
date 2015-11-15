(* Contains various flags for optimization. These are refs so that they can be
   set by top.ml and accesses throughout the compiler without having to pass
   them as arguments everything *)

(* Defaults are the behavior of -01 *)
let safeMode = ref true (* default: do null ptr, array bounds, shift checks *)
let doRegAlloc = ref true (* default: do register allocation *)
let removeDeadCode = ref false (* don't remove dead code *)
let doConstOpts = ref false (* don't do const propogation/folding *)
let doInlining = ref false (* don't do inlining *)
