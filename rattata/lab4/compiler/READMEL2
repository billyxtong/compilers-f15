L2 Compiler by Ben Plaut (bplaut)
and William Tong (wxt)

Project overview:

We implement a complete compiler for language L2, which extends language L1
by adding booleans, control flow, and some operators. This entailed adding
steps for lexing, parsing, AST elaboration, and typechecking, as well as
necessary updates to infinite-address, three-address, and two-address codegen,
described below.

External libraries:
1. Built-in OCaml libraries: List, Array, Hashtbl, String, 
2. Jane Street's Core.Std

Design:

Significant updates were made to provided front end files. Specifically, we 
changed the starter code files from L1 to be able to lex and parse the new 
tokens added in L2, and changed them to be compatabile with our own datatypes 
(updated in util/datatypesv1.ml). We also created datatypes for 3 different 
ASTs in order to keep elaboration and typechecking strictly separate (see 
front_end/parse/ast.ml). We retain modularity of datatypes from L1 (no 
assembly datatypes in non-assembly, x-address code) in infinite-address code,
and finally implement liveness analysis to (hopefully) prevent stack overflow
in C0 programs with high numbers of variables.

Compiler flow:
1. Lexing: 
2. Parsing:

3. Elaboration: We take the output of the parsing step, which is a 
pre-elaboration AST, and turn it into an UNTYPED post-elaboration AST. In 
this step, we split declaration/initialization statements, turn
for loops into while loops, and assign random expressions in code (e.g. 
a line that says "5 + 4 + 3;") to "fake" variables because they could 
throw div-by-zero exceptions or errors.  

4. Typechecking: We take the output of the elaborating step, which is an 
untyped post-elaboration AST, and turn it into a TYPED post-elaboration AST.
In this step, we check that each untyped statement actually is well-typed by
the constraints of the L2 grammar, and throw errors if it is not. Examples
include, but are not limited to,
 - if(e,s1,s2), e must be a boolean expression
 - boolean variables cannot be assign int expressions and vice versa
 - variables with the same name cannot be redeclared in the same scope
 - return(e), e must be an int expression

5. Convert to infinite address code: 
6. Convert to 3 address code: updated to conform to new datatypes 
7. Convert to 2 address code:
8. Register allocation and liveness analysis:
9. Assembly: 

