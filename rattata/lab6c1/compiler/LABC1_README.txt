Ben Plaut (bplaut) and William Tong (wxt)
L6 Compiler: "C0 and Beyond"

Project overview:

In L6, we chose to write a compiler for extensions to the L4 language.
We chose the "C0 and Beyond" project, which includes the L5 language,
as well as another interesting extension to the language that we chose
ourselves. Our L6 therefore includes implementations for small types
"char", "string", function pointers, and a moderately strong form of 
type inference.

Design:

We decided to represent chars as ints due to the ease with which we can
convert between characters and their ASCII values. For the same reason,
we decide to represent string literals as int lists. 

Regarding function pointers, we add new constructors to our ASTs for 
the following: the new use of "typedef", dereferencing function pointers,
and the address-of operator "&". 

All new relevant datatypes can be viewed in util/datatypesv1.ml and 
front_end/parse/ast.ml.

Compiler flow:

Steps 1-5 are in the directory "front_end/", and 6-9 are in "back_end/".

1. Lexing:
2. Parsing:

3. Elaboration: Elaboration remained mostly the same from L4, with new
transformations needed for chars and strings. We handled escape sequences
for both datatypes in this step. Function pointers are handled very simply
in this step, transformed from constructor to analogous constructor.

4. Typechecking: We take the output of the elaboration step, which is an
untyped AST, and typecheck it via the rules of the L5 language.

5. Convert to infinite address code:
6. Convert to 3 address code:
7. Convert to 2 address code:
8. Register allocation and liveness analysis:
9. Assembly:

