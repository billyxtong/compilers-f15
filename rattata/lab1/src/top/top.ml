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
  +> flag "--dump-wonky" no_arg ~doc:" Pretty print the two address code"

let say_if flag s =
  if flag then say (s ()) else ()

let main files verbose dump_parsing dump_ast dump_ir dump_assem typecheck_only dump_3Addr dump_2Addr dump_wonky () =
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
    let threeAddr = To3Addr.to3Addr ir in
    say_if dump_3Addr (fun () -> Tree.Print.pp_program threeAddr);

    (* Three address to Two address *)
    let twoAddr = To2Addr.to2Addr threeAddr in
    say_if dump_2Addr (fun () -> tmp2AddrProgToString twoAddr);

    
    (* Allocate Registers  TODO *)

    (* Account for wonky instructions that require specific
       registers, like idiv *)
    (* let wonkyAssem = ToWonkyAssem.toWonkyAssem assem in *)
    (* say_if dump_wonky (fun () -> assemProgWonkyToString *)
    (*                       wonkyAssem); *)
    
    (* Add assembly header and footer *)
    (* let assem = *)
    (*   [FormatAssem.DIRECTIVE(".file\t\"" ^ source ^ "\"")] *)
    (*   @ assem *)
    (*   @ [FormatAssem.DIRECTIVE ".ident\t\"15-411 L1 reference compiler\""] in *)
    (* let code = String.concat (List.map assem ~f:FormatAssem.formatAssem) in *)

    (* Output assembly *)
  (*   let afname = (Filename.chop_extension source) ^ ".s" in *)
  (*   say_if verbose (fun () -> "Writing assembly to " ^ afname ^ " ..."); *)

  (*   Out_channel.with_file afname *)
  (*     ~f:(fun afstream -> output_string afstream code) *)
  with
    ErrorMsg.Error -> say "Compilation failed"; exit 1
  | EXIT -> exit 1
  | Arg.Help x -> prerr_string x; exit 1
  | e -> prerr_string (Exn.to_string e); exit 1

let () = Command.run (Command.basic ~summary:"L1 compiler" spec main)


(*
       (* assembly instructions for program: int main () { int x = 8; int y = 9; return x * y; }*) 
    let testInstr1 = MOV(AssemLoc(Reg(RSP)), Reg(RBP)) in
    let testInstr2 = MOV(Const(8, INT), MemAddr(RBP, -8)) in
    let testInstr3 = MOV(Const(9, INT), MemAddr(RBP, -4)) in
    let testInstr4 = MOV(AssemLoc(MemAddr(RBP, -8)), Reg(RAX)) in
    let testInstr5 = BINOP(MUL(AssemLoc(MemAddr(RBP, -4)), Reg(RAX))) in
    let testInstr6 = RETURN in
    print_endline (assemProgToString([testInstr1; testInstr2; testInstr3; testInstr4; testInstr5; testInstr6]));
    (* 2-address fibonacci, return fib(4) *)
    let testTmpInstr0 = Tmp2AddrMov(AssemArg(Const(0, INT)), Tmp(0)) in
    let testTmpInstr1 = Tmp2AddrMov(AssemArg(Const(1, INT)), Tmp(1)) in
    let testTmpInstr2 = Tmp2AddrMov(TmpAssemLoc(Tmp(0)), Tmp(2)) in
    let testTmpInstr3 = Tmp2AddrBinop(Tmp2AddrAdd(TmpAssemLoc(Tmp(1)), Tmp(2))) in
    let testTmpInstr4 = Tmp2AddrMov(TmpAssemLoc(Tmp(1)), Tmp(3)) in
    let testTmpInstr5 = Tmp2AddrBinop(Tmp2AddrAdd(TmpAssemLoc(Tmp(2)), Tmp(3))) in
    let testTmpInstr6 = Tmp2AddrMov(TmpAssemLoc(Tmp(2)), Tmp(4)) in
    let testTmpInstr7 = Tmp2AddrBinop(Tmp2AddrAdd(TmpAssemLoc(Tmp(3)), Tmp(4))) in
    let testTmpInstr8 = Tmp2AddrReturn(TmpAssemLoc(Tmp(4))) in
    print_endline (tmp2AddrProgToString([testTmpInstr0; testTmpInstr1; testTmpInstr2; testTmpInstr3; 
                                         testTmpInstr4; testTmpInstr5; testTmpInstr6; testTmpInstr7; testTmpInstr8]));
    (* 3-address fibonacci, return fact(2) *)
    let testTmpInstr1 = Tmp3AddrMov(AssemArg(Const(1, INT)), Tmp(1)) in
    let testTmpInstr2 = Tmp3AddrMov(AssemArg(Const(2, INT)), Tmp(2)) in
    let testTmpInstr3 = Tmp3AddrBinop(Tmp3AddrMul(TmpAssemLoc(Tmp(1)), TmpAssemLoc(Tmp(2)), Tmp(3))) in
    print_endline (tmp3AddrProgToString([testTmpInstr1; testTmpInstr2; testTmpInstr3]));
    
*)
