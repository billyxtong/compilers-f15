%{
(* L2 Compiler
 * L2 grammar
 * Authors: Ben Plaut, William Tong
 *)

open Core.Std

module A = Ast
module D = Datatypesv1

let expand_asnop id op e =
    match op with
       A.EQ -> A.SimpAssign (id, e)
     | A.PLUSEQ -> A.SimpAssign (id,
     	     A.PreElabBinop(A.PreElabIdentExpr id, A.IntBinop D.ADD, e))
     | A.SUBEQ -> A.SimpAssign (id,
     	     A.PreElabBinop(A.PreElabIdentExpr id, A.IntBinop D.SUB, e))
     | A.MULEQ -> A.SimpAssign (id,
             A.PreElabBinop(A.PreElabIdentExpr id, A.IntBinop D.MUL, e))
     | A.DIVEQ -> A.SimpAssign (id,
             A.PreElabBinop(A.PreElabIdentExpr id, A.IntBinop D.FAKEDIV, e))
     | A.MODEQ -> A.SimpAssign (id,
             A.PreElabBinop(A.PreElabIdentExpr id, A.IntBinop D.FAKEMOD, e))
     | A.AND_EQ -> A.SimpAssign (id,
             A.PreElabBinop(A.PreElabIdentExpr id, A.IntBinop D.BIT_AND, e))
     | A.OR_EQ -> A.SimpAssign (id,
             A.PreElabBinop(A.PreElabIdentExpr id, A.IntBinop D.BIT_OR, e))
     | A.XOR_EQ -> A.SimpAssign (id,
             A.PreElabBinop(A.PreElabIdentExpr id, A.IntBinop D.BIT_XOR, e))
     | A.LSHIFT_EQ -> A.SimpAssign (id,
             A.PreElabBinop(A.PreElabIdentExpr id, A.IntBinop D.LSHIFT, e))
     | A.RSHIFT_EQ -> A.SimpAssign (id,
             A.PreElabBinop(A.PreElabIdentExpr id, A.IntBinop D.RSHIFT, e))

let expand_postop id op =
    match op with
       A.PLUSPLUS -> expand_asnop id A.PLUSEQ
	      (A.PreElabConstExpr (1, D.INT))
     | A.MINUSMINUS -> expand_asnop id A.SUBEQ
	      (A.PreElabConstExpr (1, D.INT))

let rec expand_log_binop e1 op e2 =
    match op with
       A.GT -> A.PreElabBinop (e1, A.GT, e2)
     | A.DOUBLE_EQ -> A.PreElabBinop (e1, A.DOUBLE_EQ, e2)
     | A.NEQ -> A.PreElabNot (expand_log_binop e1 A.DOUBLE_EQ e2)
     | A.LEQ -> A.PreElabNot (expand_log_binop e1 A.GT e2)
     | A.LOG_AND -> A.PreElabBinop (e1, A.LOG_AND, e2)
     | A.LOG_OR -> A.PreElabNot
          (A.PreElabBinop (A.PreElabNot e1, A.LOG_AND, A.PreElabNot e2))
     | A.GEQ -> A.PreElabNot (expand_log_binop e1 A.LT e2)
     | A.LT -> A.PreElabBinop (e1, A.LT, e2)
     | _ -> failwith "this should only be called with logical binops"
				    
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
%token UNARY ASNOP LOG_BINOP
%token MINUSMINUS PLUSPLUS
%token LOG_NOT LOG_AND LOG_OR
%token NEQ DOUBLE_EQ LT LEQ GT GEQ
%token BIT_NOT BIT_AND BIT_OR XOR
%token AND_EQ OR_EQ XOR_EQ
%token LSHIFT RSHIFT LSHIFT_EQ RSHIFT_EQ
%token COLON QUESMARK       
/* UNARY and ASNOP and LOG_BINOP are dummy terminals.
 * We need dummy terminals if we wish to assign a precedence
 * to a rule that does not correspond to the precedence of
 * the rightmost terminal in that rule.
 * Implicit in this is that precedence can only be infered
 * terminals. Therefore, don't try to assign precedence to "rules"
 *
 */

%type <Ast.preElabAST> program
/* Highest precendence at bottom */
%right QUESMARK COLON       
%left LOG_OR
%left LOG_AND
%left BIT_OR
%left XOR      
%left BIT_AND
%left NEQ DOUBLE_EQ
%left LT LEQ GT GEQ
%left LSHIFT RSHIFT      
%left PLUS MINUS
%left STAR SLASH PERCENT
%right UNARY
%left LPAREN

%start program

%%

program :
  INT MAIN LPAREN RPAREN block EOF { $5 }
  ;

block :
  LBRACE stmts RBRACE            { $2 }
    
stmts :
  /* empty */                   { [] }
 | stmt stmts                   { $1::$2 }
 ;

stmt :
 | simp SEMI                     { A.SimpStmt $1 }
 | control                       { A.Control $1 }
/* Basically, turn things of the form "{int y = 0;}" into
   "if (true) int y = 0; else {int y = 0;}". This handles scope issues */
 | block                         { A.Block $1 }

simp :
   decl                     { A.PreElabDecl $1 }
 | simpNoDecl               { $1 }  
   
simpNoDecl :
   lvalue asnop exp %prec ASNOP  { expand_asnop $1 $2 $3 }
 | exp                           { A.SimpStmtExpr $1 }
 | lvalue postop		 { expand_postop $1 $2 }
  ;

postop :
   PLUSPLUS                      { A.PLUSPLUS }
 | MINUSMINUS 	                 { A.MINUSMINUS }
    
c0type :
   INT                           { D.INT }
 | BOOL 		         { D.BOOL }

simpoptNoDecl :
   /* empty */                   { A.EmptySimp }
 | simpNoDecl		         { A.HasSimpStmt $1 }

simpopt :
   /* empty */                   { A.EmptySimp }
 | simp				{ A.HasSimpStmt $1 }

elseopt :
   /* empty */                   { A.EmptyElse }
 | ELSE stmt		        { A.PreElabElse $2 }


  
control :
   IF LPAREN exp RPAREN stmt elseopt { A.PreElabIf ($3, $5, $6) }
 | WHILE LPAREN exp RPAREN stmt { A.PreElabWhile ($3, $5) }
 | FOR LPAREN simpopt SEMI exp SEMI simpoptNoDecl RPAREN stmt
       { A.PreElabFor ($3, $5, $7, $9) }
 | RETURN exp SEMI               { A.PreElabReturn $2 }
	  
decl :
   c0type IDENT                     { A.NewVar ($2, $1)}
 | c0type IDENT ASSIGN exp         { A.Init ($2, $1, $4) }
 | c0type MAIN                     { A.NewVar ("main", $1) }
 | c0type MAIN ASSIGN exp          { A.Init ("main", $1, $4) }
 ;

lvalue :
   IDENT                        { $1 }
 | MAIN                         { "main" }
 | LPAREN lvalue RPAREN         { $2 }
 ;

/* There's a shift/reduce conflict for something like
   (lvalue | ) because do you reduce the lvalue into an exp first
   and then do LPAREN exp RPAREN or do you shift RPAREN first and
   and reduce via LPAREN lvalue RPAREN. But this should be ok,
   because both ways lead to a correct parse. */
exp :
  LPAREN exp RPAREN              { $2 }
 | intconst                      { $1 }
 | boolconst                     { $1 } 
 | lvalue 			 { A.PreElabIdentExpr $1 }
 /* Int arithmetic */				 
 | exp PLUS exp                  { A.PreElabBinop
				     ($1, A.IntBinop D.ADD, $3) }
 | exp MINUS exp                  { A.PreElabBinop
				     ($1, A.IntBinop D.SUB, $3) }
 | exp STAR exp                  { A.PreElabBinop
				     ($1, A.IntBinop D.MUL, $3) }
 | exp SLASH exp                  { A.PreElabBinop
				     ($1, A.IntBinop D.FAKEDIV, $3) }
 | exp PERCENT exp                  { A.PreElabBinop
				     ($1, A.IntBinop D.FAKEMOD, $3) }
/* Bitwise ops */       
 | exp BIT_OR exp                  { A.PreElabBinop
				     ($1, A.IntBinop D.BIT_OR, $3) }
 | exp BIT_AND exp                  { A.PreElabBinop
				     ($1, A.IntBinop D.BIT_AND, $3) }
 | exp XOR exp                  { A.PreElabBinop
				     ($1, A.IntBinop D.BIT_XOR, $3) }
/* Shifts */       
 | exp RSHIFT exp                  { A.PreElabBinop
				     ($1, A.IntBinop D.RSHIFT, $3) }
 | exp LSHIFT exp                  { A.PreElabBinop
				     ($1, A.IntBinop D.LSHIFT, $3) }
/* Unary */       
 | MINUS exp %prec UNARY      { A.PreElabBinop
				(A.PreElabConstExpr (0, D.INT),
				  A.IntBinop D.SUB, $2 ) }
 | BIT_NOT exp %prec UNARY      { A.PreElabBinop
				(A.PreElabConstExpr (-1, D.INT),
				  A.IntBinop D.BIT_XOR, $2 ) }
/* Comparison/Logical */
 | exp GT exp                  { expand_log_binop $1 A.GT $3 }
 | exp DOUBLE_EQ exp           { expand_log_binop $1 A.DOUBLE_EQ $3 }
 | exp NEQ exp                 { expand_log_binop $1 A.NEQ $3 }
 | exp LT exp 		       { expand_log_binop $1 A.LT $3 }
 | exp GEQ exp 		       { expand_log_binop $1 A.GEQ $3 }
 | exp LEQ exp 		       { expand_log_binop $1 A.LEQ $3 }
 | exp LOG_AND exp 	       { expand_log_binop $1 A.LOG_AND $3 }
 | exp LOG_OR exp              { expand_log_binop $1 A.LOG_OR $3 }
 | LOG_NOT exp %prec UNARY    { A.PreElabNot $2 }
 | exp QUESMARK exp COLON exp  { A.PreElabTernary ($1, $3, $5) }
 ;
   
boolconst :
   TRUE               { A.PreElabConstExpr(1, D.BOOL) }
 | FALSE              { A.PreElabConstExpr(0, D.BOOL) }		     
   
intconst :
  DECCONST           { match Int32.to_int $1 with
			   Some x -> A.PreElabConstExpr (x, D.INT)
			 | None ->
			       failwith "could not convert 32bit int"
		     }
 | HEXCONST         { match Int32.to_int $1 with
			   Some x -> A.PreElabConstExpr (x, D.INT)
			 | None ->
			       failwith "could not convert 32bit int"
		     }

asnop :
  ASSIGN                        { A.EQ }
 | PLUSEQ                       { A.PLUSEQ }
 | MINUSEQ                      { A.SUBEQ }
 | STAREQ                       { A.MULEQ }
 | SLASHEQ                      { A.DIVEQ }
 | PERCENTEQ                    { A.MODEQ }
 | AND_EQ                       { A.AND_EQ }
 | OR_EQ                        { A.OR_EQ }
 | XOR_EQ                       { A.XOR_EQ }
 | LSHIFT_EQ                    { A.LSHIFT_EQ }
 | RSHIFT_EQ                    { A.RSHIFT_EQ }
 ;

%%
