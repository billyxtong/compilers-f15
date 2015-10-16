{
(* L2 Compiler
 * Lexer
 * Authors: Ben Plaut and William Tong
 * 
 * Lexes the necessary keywords and other tokens
 * in order to make the grammar forward compatible with C0.
 *)

open Core.Std

module A = Ast
module P = C0Parser

let start = Lexing.lexeme_start
let l_end = Lexing.lexeme_end
let text = Lexing.lexeme

let commentLevel = ref 0
let commentPos = ref 0

let enterComment lexbuf =
  commentLevel := !commentLevel + 1 ;
  commentPos := start lexbuf

let exitComment () =
  commentLevel := !commentLevel - 1 ;
  !commentLevel = 0

let decnumber s lexbuf =
  try
    P.DECCONST (Int32.of_string (if s = "2147483648" then "0x80000000"
                                 else s))
  with Failure _ ->
    ErrorMsg.error
      ("cannot parse integral constant `" ^ text lexbuf ^ "'");
    P.DECCONST Int32.zero

let hexnumber s lexbuf =
  try
    P.HEXCONST (Int32.of_string s)
  with Failure _ ->
    ErrorMsg.error
      ("cannot parse integral constant `" ^ text lexbuf ^ "'");
    P.HEXCONST Int32.zero

let eof () =
  (if !commentLevel > 0 then
    ErrorMsg.error 
      "unterminated comment");
  P.EOF

}

let id = ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let decnum = ("0" | ['1'-'9'](['0'-'9']*))
let hexnum = "0"['x' 'X']['0'-'9' 'a'-'f' 'A'-'F']+

let ws = [' ' '\t' '\r' '\011' '\012']

rule initial =
  parse
    ws+         { initial lexbuf }
  | '\n'        { ParseState.newline (start lexbuf); initial lexbuf }

  | '{'         { P.LBRACE }
  | '}'         { P.RBRACE }
  | '('         { P.LPAREN }
  | ')'         { P.RPAREN }

  | ';'         { P.SEMI }
  | ','         { P.COMMA }

  | '='         { P.ASSIGN }
  | "+="        { P.PLUSEQ }
  | "-="        { P.MINUSEQ }
  | "*="        { P.STAREQ }
  | "/="        { P.SLASHEQ }
  | "%="        { P.PERCENTEQ }

  | '+'         { P.PLUS }
  | '-'         { P.MINUS }
  | '*'         { P.STAR }
  | '/'         { P.SLASH }
  | '%'         { P.PERCENT }

  | "!"         { P.LOG_NOT }
  | "&&"        { P.LOG_AND }
  | "||"        { P.LOG_OR }
      
  | "!="        { P.NEQ }
  | "=="        { P.DOUBLE_EQ }
  | "<"         { P.LT }
  | "<="        { P.LEQ }
  | ">"         { P.GT }
  | ">="        { P.GEQ }

  | "~"         { P.BIT_NOT }
  | "&"         { P.BIT_AND }
  | "|"         { P.BIT_OR }
  | "^"         { P.XOR }

  | "&="        { P.AND_EQ }
  | "|="        { P.OR_EQ }
  | "^="        { P.XOR_EQ }    

  | "<<="       { P.LSHIFT_EQ }
  | ">>="       { P.RSHIFT_EQ }
  | "<<"        { P.LSHIFT }
  | ">>"        { P.RSHIFT }

  | ":"         { P.COLON }
  | "?"         { P.QUESMARK }

  | "--"          { P.MINUSMINUS }
  | "++"          { P.PLUSPLUS }
      
  | "true"        { P.TRUE }
  | "false"       { P.FALSE }
  | "bool"        { P.BOOL }
  | "void"        { P.VOID }

  | "if"          { P.IF }
  | "else"        { P.ELSE }
  | "while"       { P.WHILE }
  | "for"         { P.FOR }

  | "typedef"     { P.TYPEDEF }
  | "assert"      { P.ASSERT }

  | "break"       { assert false }

  | "struct"      { assert false }
  | "continue"    { assert false }
  | "NULL"        { assert false }
  | "alloc"       { assert false }
  | "alloc_array" { assert false }
  | "char"        { assert false }
  | "string"      { assert false }

  | "return"    { P.RETURN }
  | "int"       { P.INT }

(* don't think main should be a separate token anymore *)
(*   | "main"      { P.MAIN } *)


  | decnum as n { decnumber n lexbuf }
  | hexnum as n { hexnumber n lexbuf }

  | id as name  { P.IDENT name }

  | "/*"        { enterComment lexbuf; comment lexbuf }
  | "*/"        { ErrorMsg.error 
                    "unbalanced comments"; initial lexbuf }

  | "//"        { comment_line lexbuf }
  | '#'         { assert false }
  | eof         { eof () }
  | _           { ErrorMsg.error 
                    ("illegal character: \"" ^ text lexbuf ^ "\"");
                  initial lexbuf }

and comment =
  parse
    "/*"       { enterComment lexbuf; comment lexbuf }
  | "*/"       { (if exitComment () then initial else comment) lexbuf }
  | '\n'       { ParseState.newline (start lexbuf); comment lexbuf }
  | _          { comment lexbuf }

and comment_line =
  parse
    '\n'       { ParseState.newline (start lexbuf); initial lexbuf }
  | _          { comment_line lexbuf }

{}
