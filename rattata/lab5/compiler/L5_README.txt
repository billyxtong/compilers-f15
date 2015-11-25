L5 Compiler by Ben Plaut (bplaut) and William Tong (wxt)

L5 keeps the same language as L4, so there are no changes made to 
the back or front end in the lexer, parser, typechecker, or codegen
mechanisms (infinite address, 3-address, 2-address, assembly). We 
instead optimize our compiler as well as its generated assembly code
so the correct programs we already compile can run even faster. We
implement five optimizations:

1. Dead code elimination
2. Constant folding and propagation
3. Register allocator optimizations (such as precoloring)
4. Inlining
5. Limiting push/pop instructions

as well as an "unsafe" mode that forgoes checks that are necessary
to prove memory safety (array access bounds and NULL dereferences).

======================== Design Decisions =========================

With the exception of the register allocator optimizations, all files
pertaining to optimizations are in a new directory "optimizations" that
is nested under the L5 "compiler" directory. In order to be able to 
turn our optimizations off and on, we create a file "optimizeFlags.ml"
in the "optimizations" directory that contains boolean values representing
whether we want to turn on an optimization. Depending on the flags passed
into our compiler as arguments, "top.ml" will turn on an optimization and
perform it on the corresponding intermediate language.

================= Explanation of Optimizations ======================

1) Dead code elimination is performed via a transformation on our 
two-address code and implemented as follows. First, we make a set of
all temps in the program. Then, we find the lines that they're needed
on. Finally, we propagate this neededness using a backward dataflow
analysis. We decided to do this on two-address code because no additional
dead code can be generated from transforming two-address code to assembly.

2) Constant folding and propgation is implemented weakly (without reaching 
definitions or SSA) on our three-address code. In order to avoid propagating 
across jumps, we only propagate for temps defined exactly once. Using a 
forward dataflow analysis, we find all temps, and if they are mapped 
to a constant in a move, replace all subsequent instances of this temp in 
the program with that constant. We then remove the move instruction. Finally, 
if we ever reach an instruction where after constant propagation we have a 
binop on two constant arguments, we simply fold them and map the temp to 
the new constant, creating another propagable temp. Three-address code was
the natural choice for this optimization because we want to transform code
of the form "x = y + z".

3) The calling conventions specify six registers to be used for passing 
arguments to function. Up until L5, we reserved those registers for use 
only as arguments. After reserving two registers for handling 
memory-to-memory operations, using RBP as the base pointer, and reserving 
EAX to hold return values, this left us with only five registers to be 
allocated for arbitrary temps. To handle this, we implemented the ability 
to precolor temps. This allowed us to require that certain temps be 
allocated certain registers, without losing the ability to allocate that 
register to other temps. This provided us with four more registers to 
allocate (we still reserved ECX and EDX for shifts and division, respectively).

4) We implement standard function inlining. We inline functions that are fewer than 50 line (in three-address code), and are not recursive.

5) Previously, each function would push all of the registers it used both upon entrance, and before each function call: effectively twice as much as necessary. This optimization solves that, and is implemented in back_end/regAlloc/allocForFun.ml.




