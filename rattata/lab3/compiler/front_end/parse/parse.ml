(* L2 Compiler
 * Parser
 * Authors: Ben Plaut, William Tong
 * Gluing together the pieces produced by ocamllex and ocamlyacc
 *)

(* parse filename = ast
 * will raise ErrorMsg.Error in case of lexing or parsing error
 *)

open Core.Std

let parse filename =
  try
    let main_ast = (In_channel.with_file filename ~f:(
      fun chan ->
        let lexbuf = Lexing.from_channel chan in
        let _ = ErrorMsg.reset ()
        and _ = ParseState.setfile filename in
        let ast = C0Parser.program C0Lexer.initial lexbuf in
        let _ = if !ErrorMsg.anyErrors then raise ErrorMsg.Error else () in
        ast)) in
     let header_ast = (In_channel.with_file filename ~f:(
      fun chan ->
        let lexbuf = Lexing.from_channel chan in
        let _ = ErrorMsg.reset ()
        and _ = ParseState.setfile filename in
        let ast = C0Parser.program C0Lexer.initial lexbuf in
        let _ = if !ErrorMsg.anyErrors then raise ErrorMsg.Error else () in
        ast))
      in (main_ast, header_ast)
  with
  | Parsing.Parse_error ->
    ErrorMsg.error None "Parse error"; raise ErrorMsg.Error
  | Sys_error s -> ErrorMsg.error None s; raise ErrorMsg.Error
