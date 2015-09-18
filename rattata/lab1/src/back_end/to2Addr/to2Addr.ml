open Datatypesv1

(* val toTmpAssemArg: Tree.exp -> tmpAssemArg *)
(* This function is only necessary because we're converting from *)
(* their datatype to ours here.       *)
let toTmpArg = function
    Tree.CONST x -> TmpConst (Int32.to_int x)
  | Tree.TEMP t -> TmpLoc (Tmp t)
  | _ -> failwith "Can only convert consts and tmps to tmpArg\n"
    
(* val binopTo2Addr: Tree.exp -> tmp2AddrInstr list
   Tree.exp must be a binop *)
(* MAKE SURE YOU GET THE ORDER RIGHT FOR NON-COMMUTATIVE OPS *)
let binopTo2Addr dest = function
    (b, e1, e2) ->
       let arg1 = toTmpArg e1 in
       let arg2 = toTmpArg e2 in
       let t = Tmp(Temp.create()) in
       let op = match b with
           Tree.ADD -> Tmp2AddrBinop(Tmp2AddrAdd(arg2, t))
        |  Tree.SUB -> Tmp2AddrBinop(Tmp2AddrSub(arg2, t))
        |  Tree.MUL -> Tmp2AddrBinop(Tmp2AddrMul(arg2, t))
        |  Tree.DIV -> Tmp2AddrBinop(Tmp2AddrDiv(arg2, t))
        |  Tree.MOD -> Tmp2AddrBinop(Tmp2AddrMod(arg2, t))
                    in
       Tmp2AddrMov(arg1, t)::op::
       [Tmp2AddrMov(TmpLoc t, dest)]

(* val instrTo2Addr: Tree.stm -> tmp2AddrInstr list *)
(* A lot of redundant code at the moment but oh well.
   it will be better when I'm not converting from their
   datatype *)
(* Tree.MOVE is destination-source, which is annoying >:( *)       
let instrTo2Addr = function
  Tree.RETURN Tree.BINOP (b, e1, e2) ->
     let t = Tmp(Temp.create()) in
     binopTo2Addr t (b, e1, e2) @ [Tmp2AddrReturn
                                     (TmpLoc t)]
  | Tree.RETURN arg ->
       [Tmp2AddrReturn (toTmpArg arg)]
  | Tree.MOVE (Tree.TEMP dest, Tree.BINOP (b, e1, e2)) ->
      let our_dest = Tmp dest in
      binopTo2Addr our_dest (b, e1, e2)
  | Tree.MOVE (Tree.TEMP dest, src) ->
      let our_dest = Tmp dest in
      [Tmp2AddrMov (toTmpArg src, our_dest)]
  | Tree.MOVE _ ->
      failwith "Constants and binops cannot be desinations\n"
         
(* val to2Addr: Tree.stm list -> tmp2AddrProg *)
let rec to2Addr = function
   [] -> []
 | instr::instrs -> instrTo2Addr instr @ to2Addr instrs
