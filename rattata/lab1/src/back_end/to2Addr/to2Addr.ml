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
let binopTo2Addr dest = function
    (b, e1, e2) ->
       let arg1 = toTmpAssemArg e1 in
       let arg2 = toTmpAssemArg e2 in
       let t = Tmp(Temp.create()) in
       let op = match b with
           Tree.ADD -> Tmp2AddrBinop(Tmp2AddrAdd(arg2, t))
        |  Tree.MUL -> Tmp2AddrBinop(Tmp2AddrMul(arg2, t))
                    in
       Tmp2AddrMov(arg1, t):: op::
       [Tmp2AddrMov(TmpAssemLoc t, dest)]

(* val instrTo2Addr: Tree.stm -> tmp2AddrInstr list *)
(* A lot of redundant code at the moment but oh well.
   it will be better when I'm not converting from their
   datatype *)
let instrTo2Addr = function
    Tree.RETURN arg ->
       [Tmp2AddrReturn (toTmpAssemArg arg)]
  | Tree.RETURN Tree.BINOP (b, e1, e2) ->
      let t = Tmp(Temp.create()) in
      binopTo2Addr t (b, e1, e2) @ [Tmp2AddrReturn
                                      (TmpAssemLoc t)]
  | Tree.MOVE (dest, Tree.BINOP (b, e1, e2)) ->
      let TmpAssemLoc our_dest = toTmpAssemArg dest in
      binopTo2Addr our_dest (b, e1, e2)
  | Tree.MOVE (dest, src) ->
      let TmpAssemLoc our_dest = toTmpAssemArg dest in
      [Tmp2AddrMov (toTmpAssemArg src, our_dest)]
         
(* val to2Addr: Tree.stm list -> tmp2AddrProg *)
let rec to2Addr = function
   [] -> []
 | instr::instrs -> instrTo2Addr instr @ to2Addr instrs
