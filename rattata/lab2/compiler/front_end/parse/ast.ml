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

(* Post-Elab AST
   A restriced grammar from the Pre-Elab AST. See the elaboration
   file (which I have not yet written) for more info. *)
type ident = string
type expr = ConstExpr of const | Ident of ident
          | ASTBinop of expr * tmpBinop * expr
and assignStmt = ident * expr 
type stmt = Decl of ident | AssignStmt of assignStmt | Return of expr
type elabAST = stmt list

(* Pre-Elab AST
   Unfortunately, we have to wrap everything in different
   constructors here, in order to keep in separate from
   Post-Elab AST *)
type postOp = PLUSPLUS | MINUSMINUS
type assignOp = EQ | PLUSEQ | SUBEQ | MULEQ | DIVEQ | MODEQ
type preElabExpr = PreElabConstExpr of const
                 | IdentExpr of ident
                 | PreElabBinop of preElabExpr * tmpBinop * preElabExpr
type preElabDecl = NewVar of ident * c0type
                 | Init of (ident * c0type * preElabExpr)
type simpStmt = PreElabDecl of preElabDecl                        
              | SimpAssign of ident * assignOp * preElabExpr
              | SimpStmtExpr of preElabExpr
type elseOpt = EmptyElse | PreElabElse of preElabStmt
type simpOpt = EmptySimp | HasSimpStmt of simpStmt
type control = PreElabIf of preElabExpr * preElabStmt * elseOpt
             | PreElabWhile of preElabExpr * preElabStmt
             | PreElabFor of simpOpt * preElabExpr * simpOpt *
                             preElabStmt
             | PreElabReturn of preElabExpr
type preElabStmt = SimpStmt of simpStmt
                 | Control of control
                 | Block of block
and block = preElabStmt list                      
type preElabAST = block
