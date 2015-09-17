open Datatypesv1

(* val toTmpAssemArg: Tree.exp -> tmpAssemArg *)
(* This function is only necessary because we're converting from *)
(* their datatype to ours here.       *)
let toTmpAssemArg = function
    Tree.CONST x -> AssemArg(Const(Int32.to_int x, INT))
  | Tree.TEMP t -> TmpAssemLoc(Tmp(t))
    
(* val binopTo2Addr: Tree.exp -> tmp2AddrInstr list
   Tree.exp must be a binop *)
(* MAKE SURE YOU GET THE ORDER RIGHT FOR NON-COMMUTATIVE OPS *)
let binopTo2Addr = function
    Tree.BINOP(b, e1, e2) ->
       let arg1 = toTmpAssemArg e1 in
       let arg2 = toTmpAssemArg e2 in
       let t = Tmp(Temp.create()) in
       let op = match b with
           ADD -> Tmp2AddrBinop(Tmp2AddrAdd(arg2, t))
                    in
       [Tmp2AddrMov(t, arg1), op]

(* val instrTo2Addr: Tree.stm -> tmp2AddrInstr list *)
(* A lot of redundant code at the moment but oh well.
   it will be better when I'm not converting from their
   datatype *)
let instrTo2Addr = function
    Tree.RETURN Tree.CONST x ->
      [Tmp2AddrReturn toTmpAssemArg (Tree.CONST x)]
  | Tree.RETURN Tree.TEMP t ->
      [Tmp2AddrReturn toTmpAssemArg (Tree.TEMP t)]
  | Tree.MOVE (Tree.exp dest, Tree.CONST x) ->
      let our_dest = toTmpAssemArg dest in
      let our_src = toTmpAssemArg src in 
      [Tmp2AddrMov (our_src, our_dest)] 
  | Tree.MOVE (Tree.exp dest, Tree.TEMP t) ->
      let our_dest = toTmpAssemArg dest in
      let our_src = toTmpAssemArg src in 
      [Tmp2AddrMov (our_src, our_dest)]
  | Tree.MOVE (Tree.exp dest, Tree.TEMP t) ->
      let our_dest = toTmpAssemArg dest in
      let our_src = toTmpAssemArg src in 
      [Tmp2AddrMov (our_src, our_dest)]
      
         
           

(* val to2Addr: Tree.stm list -> tmp2AddrProg *)
let rec to2Addr = function
   [] -> []
 | instr::instrs -> instrTo2Addr instr @ to2Addr instrs
