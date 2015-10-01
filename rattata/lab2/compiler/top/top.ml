(* L1 Compiler
 * Top Level Environment
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

open Core.Std
open Datatypesv1
open PrintDatatypes

let say = prerr_endline
let newline = prerr_newline

exception EXIT

let spec =
  let open Command.Spec in
  empty
  +> anon (sequence ("files" %: string))
  +> flag "--verbose" ~aliases:["-v"] no_arg ~doc:" Verbose messages"
  +> flag "--dump-parsing" no_arg ~doc:" Pretty print parsing messages"
  +> flag "--dump-ast" no_arg ~doc:" Pretty print the AST"
  +> flag "--dump-ir" no_arg ~doc:" Pretty print the IR"
  +> flag "--dump-assem" no_arg ~doc:" Pretty print the assembly"
  +> flag "--only-typecheck" ~aliases:["-t"] no_arg ~doc:" Halt after typechecking"
  +> flag "--dump-3Addr" no_arg ~doc:" Pretty print the three address code"
  +> flag "--dump-2Addr" no_arg ~doc:" Pretty print the two address code"
  +> flag "--dump-NoMemMem" no_arg ~doc:" Pretty print after handling Mem-Mem instrs"
  +> flag "--dump-wonky" no_arg ~doc:" Pretty print the wonky assembly"
  +> flag "--dump-final" no_arg ~doc:" Pretty print the final assembly"

let say_if flag s =
  if flag then say (s ()) else ()

let main files verbose dump_parsing dump_ast dump_ir dump_assem typecheck_only dump_3Addr dump_2Addr dump_NoMemMem dump_wonky dump_final () =
  try
   
    let source = match files with
    | [] -> say "Error: no input file provided"; raise EXIT
    | [filename] -> filename
    | _ -> say "Error: more than one input file"; raise EXIT
    in

    (* Parse *)
    say_if verbose (fun () -> "Parsing... " ^ source);
    if dump_parsing then ignore (Parsing.set_trace true);

    let ast = Parse.parse source in
    say_if dump_ast (fun () -> Ast.Print.pp_program ast);

    (* Typecheck *)
    say_if verbose (fun () -> "Typecking...");
    TypeChecker.typecheck ast;
    if typecheck_only then exit 0;

    (* Convert Post-Elab AST to Infinte Addr *)
    say_if verbose (fun () -> "converting to Infinite Address code");
    let ir = ToInfAddr.toInfAddr ast in
    say_if dump_ir (fun () -> Tree.Print.pp_program ir);

    (* Convert Inf Addr (arbitrarily nested right hand side)
       to three address *)
    say_if verbose (fun () -> "Allocating Registers");
    let threeAddr = FewTmpsTo3Addr.to3Addr ir in
    say_if dump_3Addr (fun () -> Tree.Print.pp_program threeAddr);

    (* Three address to Two address *)
    say_if verbose (fun () -> "3Addr to 2Addr...");
    let twoAddr = To2Addr.to2Addr threeAddr in
    say_if dump_2Addr (fun () -> tmp2AddrProgToString twoAddr);
    
    (* Allocate Registers *)
    say_if verbose (fun () -> "Allocating Registers...");
    let almostAssem = RegAlloc.regAlloc twoAddr in
    say_if dump_assem (fun () -> assemProgToString almostAssem);

    (* RemoveMemMemInstrs *)
    say_if verbose (fun () -> "Removing mem-mem instrs...");
    let noMemMemAssem =
       RemoveMemMemInstrs.removeMemMemInstrs almostAssem in
    say_if dump_NoMemMem (fun () ->
        assemProgToString noMemMemAssem);

    (* Account for wonky instructions that require specific
       registers, like idiv *)
    say_if verbose (fun () -> "Handling wonky instructions...");
    let wonkyAssem = ToWonkyAssem.toWonkyAssem noMemMemAssem in
    say_if dump_wonky (fun () -> assemProgWonkyToString
                          wonkyAssem);

    (* Format assembly *)
    say_if verbose (fun () -> "Formatting assembly...");
    let finalAssem = FormatAssem.formatAssem wonkyAssem in
    say_if dump_final (fun () -> finalAssem);
    
    (* Output assembly *)
    say_if verbose (fun () -> "Outputting assembly...");
    let afname = (Filename.chop_extension source) ^ ".s" in
    say_if verbose (fun () -> "Writing assembly to " ^ afname ^ " ...");

    Out_channel.with_file afname
      ~f:(fun afstream -> output_string afstream finalAssem)
      
  with
    ErrorMsg.Error -> say "Compilation failed"; exit 1
  | EXIT -> exit 1
  | Arg.Help x -> prerr_string x; exit 1
  | e -> prerr_string (Exn.to_string e); exit 1

let () = Command.run (Command.basic ~summary:"L1 compiler" spec main)
