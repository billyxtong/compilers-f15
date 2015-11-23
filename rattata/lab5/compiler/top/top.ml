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
  +> flag "-l" (optional string) ~doc:" Header file"
  +> flag "--verbose" ~aliases:["-v"] no_arg ~doc:" Verbose messages"
  +> flag "--dump-parsing" no_arg ~doc:" Pretty print parsing messages"
  +> flag "--dump-ast" no_arg ~doc:" Pretty print the pre-elab AST"
  +> flag "--dump-upeAST" no_arg ~doc:" Pretty print the untyped post-elab AST"
  +> flag "--dump-typedAST" no_arg ~doc:" Pretty print the typed post-elab AST"
  +> flag "--dump-infAddr" no_arg ~doc:" Pretty print the infAddr"
  +> flag "--dump-memInfAddr" no_arg ~doc:" Pretty print the infAddr after mem stuff"
  +> flag "--dump-assem" no_arg ~doc:" Pretty print the assembly"
  +> flag "--only-typecheck" ~aliases:["-t"] no_arg ~doc:" Halt after typechecking"
  +> flag "--dump-3Addr" no_arg ~doc:" Pretty print the three address code"
  +> flag "--dump-ConstOpts" no_arg ~doc:" Pretty print the three address code after constant propogation + folding"
  +> flag "--dump-inlined" no_arg ~doc:" Pretty print the three address code after inlining"
  +> flag "--dump-2Addr" no_arg ~doc:" Pretty print the two address code"
  +> flag "--dump-NoDeadCode" no_arg ~doc:" Removing dead code"
  +> flag "--dump-NoMemMem" no_arg ~doc:" Pretty print after handling Mem-Mem instrs"
  +> flag "--dump-wonky" no_arg ~doc:" Pretty print the wonky assembly"
  +> flag "--dump-final" no_arg ~doc:" Pretty print the final assembly"
  +> flag "--dump-all" no_arg ~doc:" Pretty print everything"
  +> flag "-O0" no_arg ~doc:" Literally do no optimizations, i.e. don't register allocate"
  +> flag "-O1" no_arg ~doc:" Do normal stuff"
  +> flag "-O2" no_arg ~doc:" Do optimizations!"
  +> flag "--unsafe" no_arg ~doc:" Don't check array bounds, null pointers, or shift operands"
  +> flag "--killDeadCode" no_arg ~doc:" Remove dead code using neededness analysis"
  +> flag "--noRegAlloc" no_arg ~doc:" Don't do register allocation"
  +> flag "--doConstOpts" no_arg ~doc:" Do constant propogation and folding "
  +> flag "--doInlining" no_arg ~doc: " Do inlining"
  +> flag "--arrayStrengthReduction" no_arg ~doc: " Do strength reduction on array bounds"
  +> flag "--doTieBreaking" no_arg ~doc: " Try to do reg alloc tie breaking"
  +> flag "--removeJumps" no_arg ~doc: " Remove unneeded unconditional jumps"
  +> flag "-r" (optional string) ~doc: " How many extra general purpose regs to allocate"
let main files header_file verbose dump_parsing dump_ast dump_upeAST dump_typedAST dump_infAddr dump_memInfAddr dump_assem typecheck_only dump_3Addr dump_ConstOps dump_Inlined dump_2Addr dump_NoDeadCode dump_NoMemMem dump_wonky dump_final dump_all opt0 opt1 opt2 unsafe killDeadCode noRegAlloc doConstOpts doInlining arrayStrengthReduction doTieBreaking removeJumps numRegs () =
  try
    let () = if opt0 then OptimizeFlags.doRegAlloc := false in
    let () = if opt1 then OptimizeFlags.numNonParamRegsToAlloc := 0 in
    let () = if opt2 then
        (

        OptimizeFlags.numNonParamRegsToAlloc := 5;
        OptimizeFlags.doConstOpts := true;
        OptimizeFlags.doInlining := true;
        (* OptimizeFlags.removeDeadCode := true; *)
        OptimizeFlags.arrayStrengthReduction := true;

        ()) in
    let () = if unsafe then OptimizeFlags.safeMode := false in
    let () = if doConstOpts then OptimizeFlags.doConstOpts := true in
    let () = if noRegAlloc then OptimizeFlags.doRegAlloc := false in
    let () = if killDeadCode then OptimizeFlags.removeDeadCode := true in
    let () = if doInlining then OptimizeFlags.doInlining := true in
    let () = if arrayStrengthReduction then OptimizeFlags.arrayStrengthReduction := true in
    let () = if doTieBreaking then OptimizeFlags.doRegAllocTieBreaking
                                              := true in
    let () = if removeJumps then OptimizeFlags.removeUnneddedJumps := true in
    let () = (match numRegs with
                  Some n -> OptimizeFlags.numNonParamRegsToAlloc := int_of_string n
                | None -> ()) in
    let say_if flag s = if (dump_all || flag) then say (s ()) else () in

    (* main_source is the .l1/2/3/4 file. This is to distinguish from
       the header file *)
    let main_source = match files with
    | [] -> say "Error: no input file provided"; raise EXIT
    | [filename] -> filename
    | _ -> say "Error: more than one input file"; raise EXIT
    in

    (* Parse *)
    say_if verbose (fun () -> "Parsing... " ^ main_source);
    if dump_parsing then ignore (Parsing.set_trace true);
    let preElabOverallAst = Parse.parse main_source header_file in ();
    say_if dump_ast (fun () -> PrintASTs.preElabASTToString(preElabOverallAst));

    (* Elaborate *)
    say_if verbose (fun () -> "Elaborating... ");
    let untypedPostElabOverallAst = Elab.elaborateOverallAST preElabOverallAst in ();
    say_if dump_upeAST (fun () ->
      PrintASTs.untypedPostElabOverallASTToString(untypedPostElabOverallAst));

    (* Typecheck *)
    say_if verbose (fun () -> "Typechecking...");
    let typedPostElabAst = TypeChecker.typecheck untypedPostElabOverallAst in ();
    say_if dump_typedAST (fun () ->
      PrintASTs.typedPostElabASTToString(typedPostElabAst));
    if typecheck_only then exit 0;

    (* convert Post-Elab AST to Infinte Addr, except for memory stuff *)
    say_if verbose (fun () -> "converting to infAddr, except memory stuff...");
    let infAddr = GeneralToInfAddr.toInfAddr typedPostElabAst in ();
    say_if dump_infAddr (fun () -> tmpInfAddrProgToString infAddr);

    (* convert memory stuff to Inf Addr *)
    say_if verbose (fun () -> "converting memory stuff to Inf Addr...");
    let infAddrWithMem = MemStuffToInfAddr.handleMemStuff infAddr in ();
    say_if dump_memInfAddr (fun () -> tmpInfAddrProgToString infAddrWithMem);
    
    (* Convert Inf Addr (arbitrarily nested right hand side) *)
    (*    to three address *)
    say_if verbose(fun () -> "converting memInfAddr to 3Addr...");
    let threeAddr = FewTmpsTo3Addr.to3Addr infAddrWithMem in ();
    say_if dump_3Addr (fun () -> tmp3AddrProgToString threeAddr);

    (* constant folding and propagation *)
    say_if verbose(fun () -> "performing constant propagation and folding...");
    let threeAddr' = (if !OptimizeFlags.doConstOpts then
                      ConstPropAndFold.constPropAndFold threeAddr
                      else threeAddr) in
    say_if dump_ConstOps (fun () -> tmp3AddrProgToString threeAddr');

    (* inlining *)
    say_if verbose(fun () -> "performing inlining...");
    let inlinedThreeAddr = (if !OptimizeFlags.doInlining then
                      Inlining.inlineFuncs threeAddr'
                      else threeAddr') in
    say_if dump_Inlined (fun () -> tmp3AddrProgToString inlinedThreeAddr);
   

    (* Three address to Two address *)
    say_if verbose (fun () -> "3Addr to 2Addr...");
    let twoAddr = To2Addr.to2Addr inlinedThreeAddr in ();
    say_if dump_2Addr (fun () -> tmp2AddrProgToString twoAddr);

    (* eliminating dead code *)
    (*
    say_if verbose (fun () -> "Killing dead code..."); 
    let noDeadCode = (if !OptimizeFlags.removeDeadCode then
                        KillDeadCode.killDeadCode twoAddr
                        else twoAddr) in (); 
    say_if dump_NoDeadCode (fun () -> tmp2AddrProgToString noDeadCode); *)
     
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

    (* Account for wonky instructions that require specific *)
    (*   registers, like idiv *)
    say_if verbose (fun () -> "Handling wonky instructions...");
    let wonkyAssem = ToWonkyAssem.toWonkyAssem noMemMemAssem in
    say_if dump_wonky (fun () -> assemProgWonkyToString
                          wonkyAssem);

    let lala = CondenseMoves.condenseMoves wonkyAssem in
    (* Format assembly *)
    say_if verbose (fun () -> "Formatting assembly...");
    let finalAssem = FormatAssem.formatAssem lala in
    say_if dump_final (fun () -> finalAssem);
    
    (* Output assembly *)
    say_if verbose (fun () -> "Outputting assembly...");
    let afname = main_source ^ ".s" in
    say_if verbose (fun () -> "Writing assembly to " ^ afname ^ " ...");

    Out_channel.with_file afname
      ~f:(fun afstream -> output_string afstream finalAssem)

  with
    ErrorMsg.Error -> say "Compilation failed"; exit 1
  | EXIT -> exit 1
  | Arg.Help x -> prerr_string x; exit 1
  | e -> prerr_string (Exn.to_string e); exit 1

let () = Command.run (Command.basic ~summary:"L1 compiler" spec main)
