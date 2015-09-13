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

let say_if flag s =
  if flag then say (s ()) else ()

let main files verbose dump_parsing dump_ast dump_ir dump_assem typecheck_only () =
  try
    
    let c0Type = PTR in  
    let c = (4, c0Type) in print_endline (constToString(c));
    (*let s = InstrName("movl") in print_endline (instrNameToString(s));*)
    let r1 = RAX in print_endline (regToString(r1));
    let r2 = RCX in
    let offset = 8 in print_endline (memAddrToString(r2, offset));
    let testReg1 = Reg(RSI) in print_endline (assemLocToString(testReg1));
    let testAssemLoc1 = MemAddr(RBX, 8) in
    let testAssemArg1 = Const(8, INT) in
    print_endline (assemBinopToString(ADD(testAssemArg1, testAssemLoc1)));
    let testAssemLoc2 = MemAddr(RCX, 16) in
    let testAssemArg2 = AssemLoc(Reg(RAX)) in
    print_endline (assemInstrToString(MOV(testAssemArg2, testAssemLoc2)));


    
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

    (* Allocate Registers *)
    say_if verbose (fun () -> "Allocating Registers");
    let assem = RegAlloc.regAlloc ir in
    say_if dump_assem (fun () -> List.to_string ~f:FormatAssem.formatAssem assem);

    (* Add assembly header and footer *)
    let assem =
      [FormatAssem.DIRECTIVE(".file\t\"" ^ source ^ "\"")]
      @ assem
      @ [FormatAssem.DIRECTIVE ".ident\t\"15-411 L1 reference compiler\""] in
    let code = String.concat (List.map assem ~f:FormatAssem.formatAssem) in

    (* Output assembly *)
    let afname = (Filename.chop_extension source) ^ ".s" in
    say_if verbose (fun () -> "Writing assembly to " ^ afname ^ " ...");

    Out_channel.with_file afname
      ~f:(fun afstream -> output_string afstream code)
  with
    ErrorMsg.Error -> say "Compilation failed"; exit 1
  | EXIT -> exit 1
  | Arg.Help x -> prerr_string x; exit 1
  | e -> prerr_string (Exn.to_string e); exit 1

let () = Command.run (Command.basic ~summary:"L1 compiler" spec main)
