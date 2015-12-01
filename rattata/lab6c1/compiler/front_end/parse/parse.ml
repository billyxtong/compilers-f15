(* L4 Compiler
 * Parser
 * Authors: Ben Plaut, William Tong
 * Gluing together the pieces produced by ocamllex and ocamlyacc
 *)

(* parse filename = ast
 * will raise ErrorMsg.Error in case of lexing or parsing error
 *)

let parse_file filename = 
    Core.Std.In_channel.with_file filename ~f:(
      fun chan ->
        let lexbuf = Lexing.from_channel chan in
        let _ = ErrorMsg.reset ()
        and _ = ParseState.setfile filename in
        let ast = C0Parser.program C0Lexer.initial lexbuf in
        let _ = if !ErrorMsg.anyErrors then raise ErrorMsg.Error else () in
        ast)

let parse main_filename header_filename =
  try
     let header_ast =
       (match header_filename with
           None -> []
         | Some filename -> parse_file filename) in
     let main_ast = parse_file main_filename
      in (main_ast, header_ast)
  with
  | Parsing.Parse_error ->
    ErrorMsg.error "Parse error"; raise ErrorMsg.Error
  | Sys_error s -> ErrorMsg.error s; raise ErrorMsg.Error
