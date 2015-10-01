%{
(* L1 Compiler
 * L1 grammar
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 *
 * Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
 * Now conforms to the L1 fragment of C0
 *
 * Modified: Maxime Serrano <mserrano@andrew.cmu.edu> Fall 2014
 * Should be more up-to-date with 2014 spec
 *
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

open Core.Std

module A = Ast
module D = Datatypesv1
(* let ploc (left, right) = *)
(*   Parsing.rhs_start left, Parsing.rhs_end right *)
(* let mark e (left, right) = *)
(*   A.Marked (Mark.mark' (e, ParseState.ext (ploc (left, right)))) *)
(* let marks e (left, right) = *)
(*   A.Markeds (Mark.mark' (e, ParseState.ext (ploc (left, right)))) *)

(* expand_asnop (id, "op=", exp) region = "id = id op exps"
 * or = "id = exp" if asnop is "="
 * syntactically expands a compound assignment operator
 *)
(* let expand_asnop a = *)
(*   match a with *)
(*     (id, None, exp) -> *)
(*       A.Assign(id, exp) *)
(*   | (id, Some oper, exp) -> *)
(*       A.Assign(id, mark (A.OpExp (oper, [A.Var(id); exp])) (left, right)) *)

let expand_asnop id op e =
    match op with
       A.EQ -> A.SimpAssign (id, A.EQ, e)
     | A.PLUSEQ -> A.SimpAssign (id, A.EQ,
     	     A.PreElabBinop(A.IdentExpr id, D.TmpBinop D.ADD, e))
     | A.SUBEQ -> A.SimpAssign (id, A.EQ,
     	     A.PreElabBinop(A.IdentExpr id, D.TmpBinop D.SUB, e))
     | A.MULEQ -> A.SimpAssign (id, A.EQ,
             A.PreElabBinop(A.IdentExpr id, D.TmpBinop D.MUL, e))
     | A.DIVEQ -> A.SimpAssign (id, A.EQ,
             A.PreElabBinop(A.IdentExpr id, D.TmpBinop D.FAKEDIV, e))
     | A.MODEQ -> A.SimpAssign (id, A.EQ,
             A.PreElabBinop(A.IdentExpr id, D.TmpBinop D.FAKEMOD, e))
     | _ -> assert(false)		 
%}

%token EOF
%token STRUCT TYPEDEF IF ELSE WHILE FOR CONTINUE BREAK
%token ASSERT TRUE FALSE NULL ALLOC ALLOCARRY
%token BOOL VOID CHAR STRING
%token SEMI
%token <Int32.t> DECCONST
%token <Int32.t> HEXCONST
%token <string> IDENT
%token RETURN
%token INT
%token MAIN
%token PLUS MINUS STAR SLASH PERCENT
%token ASSIGN PLUSEQ MINUSEQ STAREQ SLASHEQ PERCENTEQ
%token LBRACE RBRACE
%token LPAREN RPAREN
%token UNARY ASNOP
%token MINUSMINUS PLUSPLUS
%token LOG_NOT LOG_AND LOG_OR
%token NEQ EQ LT LEQ GT GEQ
%token BIT_NOT BIT_AND BIT_OR XOR
%token AND_EQ OR_EQ XOR_EQ
%token LSHIFT RSHIFT LSHIFT_EQ RSHIFT_EQ
%token COLON QUESMARK       
/* UNARY and ASNOP are dummy terminals.
 * We need dummy terminals if we wish to assign a precedence
 * to a rule that does not correspond to the precedence of
 * the rightmost terminal in that rule.
 * Implicit in this is that precedence can only be infered
 * terminals. Therefore, don't try to assign precedence to "rules"
 *
 * MINUSMINUS is a dummy terminal to parse fail on.
 */

%type <Ast.preElabAST> program

%left PLUS MINUS
%left STAR SLASH PERCENT
%right UNARY
%left LPAREN

%start program

%%

program :
  INT MAIN LPAREN RPAREN LBRACE stmts RBRACE EOF { $6 }
  ;

stmts :
  /* empty */                   { [] }
 | stmt stmts                   { $1::$2 }
 ;

stmt :
   decl SEMI                     { A.PreElabDecl $1 }
 | simp SEMI                     { $1  }
 | RETURN exp SEMI               { A.PreElabReturn $2 }
 ;

decl :
   INT IDENT                     { A.NewVar ($2, A.INT)}
 | INT IDENT ASSIGN exp         { A.Init ($2, A.INT, $4) }
 | INT MAIN                     { A.NewVar ("main", A.INT) }
 | INT MAIN ASSIGN exp          { A.Init ("main", A.INT, $4) }
 ;

simp :
  lvalue asnop exp %prec ASNOP   { expand_asnop $1 $2 $3 }
  ;

lvalue :
   IDENT                        { $1 }
 | MAIN                         { "main" }
 | LPAREN lvalue RPAREN         { $2 }
 ;

exp :
  LPAREN exp RPAREN              { $2 }
 | intconst                      { $1 }
 | MAIN                          { A.IdentExpr "main" }
 | IDENT                         { A.IdentExpr $1 }
 | exp PLUS exp                  { A.PreElabBinop
				     ($1, D.TmpBinop D.ADD, $3) }
 | exp MINUS exp                  { A.PreElabBinop
				     ($1, D.TmpBinop D.SUB, $3) }
 | exp STAR exp                  { A.PreElabBinop
				     ($1, D.TmpBinop D.MUL, $3) }
 | exp SLASH exp                  { A.PreElabBinop
				     ($1, D.TmpBinop D.FAKEDIV, $3) }
 | exp PERCENT exp                  { A.PreElabBinop
				     ($1, D.TmpBinop D.FAKEMOD, $3) }
 | MINUS exp %prec UNARY         { A.UnaryMinus $2 }
 ;

intconst :
  DECCONST           { A.PreElabConstExpr (match Int32.to_int $1 with
			     Some x -> x
			   | None ->
			       failwith "could not convert 32bit int")
		     }
 | HEXCONST          { A.PreElabConstExpr (match Int32.to_int $1 with
			     Some x -> x
			   | None ->
			       failwith "could not convert 32bit int")
		     }
 ;

asnop :
  ASSIGN                        { A.EQ }
 | PLUSEQ                       { A.PLUSEQ }
 | MINUSEQ                      { A.SUBEQ }
 | STAREQ                       { A.MULEQ }
 | SLASHEQ                      { A.DIVEQ }
 | PERCENTEQ                    { A.MODEQ }
 ;

%%
