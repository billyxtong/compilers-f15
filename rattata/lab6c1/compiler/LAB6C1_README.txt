Ben Plaut (bplaut) and William Tong (wxt)
L6 Compiler: "C0 and Beyond"

=================== PROJECT OVERVIEW =======================

In L6, we chose to write a compiler for extensions to the L4 language.
We chose the "C0 and Beyond" project, which includes the L5 language,
as well as another interesting extension to the language that we chose
ourselves. Our L6 therefore includes implementations for small types
"char", "string", function pointers, and a moderately strong form of 
type inference.

====================== DESIGN ==========================

We decided to represent chars as ints due to the ease with which we can
convert between characters and their ASCII values. For the same reason,
we decide to represent string literals as int lists. 

Regarding function pointers, we add new constructors to our ASTs for 
the following: the new use of "typedef", dereferencing function pointers,
and the address-of operator "&". 

Finally, for type inference, we had to modify some datatypes as well as 
create a new version of the typechecker for the new type inference rules.
These are included in the directory "compiler/typeInference". In order to
turn type inference on and off, we included a new flag "--typeInference" that
we can apply to our compiler.

New relevant datatypes for L5 can be viewed in util/datatypesv1.ml and 
front_end/parse/ast.ml, and type inference files in the directory 
mentioned above.

Compiler flow:

Steps 1-5 are in the directory "front_end/", and 6-9 are in "back_end/".

1. Lexing: The file c0Lexer.mll in front_end/parse had to change a bit for
strings. We handled escape sequences explicitly, and we defined the regexp
for a string to be (double quotes) followed by (anything that is not double
quotes, unless it's \") followed by (double quotes). Lexing function
pointers was straightforward.

2. Parsing: Parsing remained largely the same, since strings were handled
in lexing. For type inference, we had to change function declarations
to not require types beforehand. The other type of type inference (variable
declarations without types) can simply be treated as simple assignments,
and so did not require changes the parser.

3. Elaboration: Elaboration remained mostly the same from L4, with new
transformations needed for chars and strings. We handled escape sequences
for both datatypes in this step. Function pointers are handled very simply
in this step, transformed from constructor to analogous constructor.

Elaboration remained the same for type inference.

4. Typechecking: We take the output of the elaboration step, which is an
untyped AST, and typecheck it via the rules of the L5 language.

In type inference, typechecking changed significantly. While we did still
take the output of the elaboration step and perform transformations, we
applied new rules. Once finished, we transformed the 
typeInfAst.typedPostElabAST back into a Ast.typedPostElabAST (so top.ml
would work).

Other changes include the need to accept the AlphaExpr datatype in basically
every expression, and the need to propagate type information forward AND
backward. However, this last feature does not quite work.

5. Convert to infinite address code: Strings are represented at runtime
as an array of characters created by allocating dynamic memory. Although
this can be more costly, it allowed us to reduce it to a problem that
we already solved.

The rest of the steps remained largely the same. Function pointer calls
are handled very similarly to normal function calls, requiring only
small adjustments to the code.
6. Convert to 3 address code
7. Convert to 2 address code
8. Register allocation and liveness analysis
9. Assembly

