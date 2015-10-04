(* L1 Compiler
 * Abstract Syntax Trees
 * Author: Alex Vaynberg
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 *
 * Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *
 * Forward compatible fragment of C0
 *)

open Datatypesv1

(* Typed Post-Elab AST
   A restriced grammar from the Pre-Elab AST. See the elaboration
   file (which I have not yet written) for more info. *)
type ident = string
type intExpr = IntConst of const | IntIdent of ident
             | ASTBinop of intExpr * intBinop * intExpr
type boolExpr = BoolConst of const | BoolIdent of ident
              | GreaterThan of intExpr * intExpr
              | IntEquals of intExpr * intExpr
              | BoolEquals of boolExpr * boolExpr
              | LogNot of boolExpr
              | LogAnd of boolExpr * boolExpr
type expr = IntExpr of intExpr | BoolExpr of boolExpr               
type assignStmt = ident * expr 
type postElabStmt = Decl of ident * c0type
                  | AssignStmt of assignStmt
                  | If of boolExpr * postElabAST * postElabAST
                  | While of boolExpr * postElabAST
                  | Return of intExpr
 and postElabAST = stmt list

(* Untyped Post-Elab AST
   A restriced grammar from the Pre-Elab AST. See the elaboration
   file (which I have not yet written) for more info. *)
type generalBinop = IntBinop of intBinop | DOUBLE_EQ | GT | LOG_AND 
type untypedPostElabExpr = UntypedPostElabConstExpr of const * c0type
                         | UntypedPostElabIdentExpr of ident
                         | UntypedPostElabBinop of untypedPostElabExpr * 
                                                   generalBinop * 
                                                   untypedPostElabExpr
                         | UntypedPostElabNot of untypedPostElabExpr
type untypedPostElabStmt = Decl of ident * c0type
                         | AssignStmt of ident * expr
                         | If of boolExpr * untypedPostElabAST * 
                                 untypedPostElabAST
                         | While of boolExpr * untypedPostElabAST
                         | Return of intExpr
                         | JumpUncond of label
(* I'm sure I had to add jumps to postElabAST Billy,
             since postElabAST really shouldn't have jumps...
             I need it for toInfAddr :(
             Ignore this though; don't write print functions
             for it or anything *)

and untypedPostElabAST = untypedPostElabStmt list

(* Pre-Elab AST
   Unfortunately, we have to wrap everything in different
   constructors here, in order to keep in separate from
   Post-Elab AST *)
(* assignOp is only used in parsing; is not actually used in the
   resulting preElabAST *)
   
type postOp = PLUSPLUS | MINUSMINUS    
type assignOp = EQ | PLUSEQ | SUBEQ | MULEQ | DIVEQ | MODEQ
              | AND_EQ | OR_EQ | XOR_EQ | LSHIFT_EQ | RSHIFT_EQ
type preElabExpr = PreElabConstExpr of const * c0type
                 | PreElabIdentExpr of ident
                 | PreElabBinop of preElabExpr * generalBinop * preElabExpr
                 | PreElabNot of preElabExpr
type preElabDecl = NewVar of ident * c0type
                 | Init of (ident * c0type * preElabExpr)
type simpStmt = PreElabDecl of preElabDecl                        
              | SimpAssign of ident * preElabExpr
              | SimpStmtExpr of preElabExpr
type simpOpt = EmptySimp | HasSimpStmt of simpStmt
type elseOpt = EmptyElse | PreElabElse of preElabStmt
 and control = PreElabIf of preElabExpr * preElabStmt * elseOpt
             | PreElabWhile of preElabExpr * preElabStmt
             | PreElabFor of simpOpt * preElabExpr * simpOpt *
                             preElabStmt
             | PreElabReturn of preElabExpr
 and preElabStmt = SimpStmt of simpStmt
                 | Control of control
                 | Block of block
 and block = preElabStmt list                      
type preElabAST = block
