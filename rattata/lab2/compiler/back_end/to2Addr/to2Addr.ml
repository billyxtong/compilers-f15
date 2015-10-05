open Datatypesv1

(* val binopTo2Addr: exp -> tmp2AddrInstr list
   exp must be a binop *)
(* MAKE SURE YOU GET THE ORDER RIGHT FOR NON-COMMUTATIVE OPS *)
let binopTo2Addr dest = function
    (op, arg1, arg2) ->
       let t = Tmp(Temp.create()) in
       (* The order of where arg1 and arg2 go here is kind of
          weird: remember it's Tmp2AddrBinop(op, dest, src) *)
       let op_instr = Tmp2AddrBinop(op, arg2, t) in
       Tmp2AddrMov(arg1, t)::op_instr::
       [Tmp2AddrMov(TmpLoc t, dest)]

(* val instrTo2Addr: stm -> tmp2AddrInstr list *)
(* A lot of redundant code at the moment but oh well.
   it will be better when I'm not converting from their
   datatype *)
(* MOVE is destination-source, which is annoying >:( *)       
let instrTo2Addr = function
    Tmp3AddrMov (dest, src) -> [Tmp2AddrMov (dest, src)]
  | Tmp3AddrReturn arg -> [Tmp2AddrReturn arg]
  | Tmp3AddrBinop (op, arg1, arg2, dest) ->
     let t = Tmp(Temp.create()) in
     binopTo2Addr t (op, arg1, arg2) @ [Tmp2AddrReturn
                                     (TmpLoc t)]
  | Tmp3AddrJump j -> Tmp2AddrJump j::[]
  | Tmp3AddrLabel jumpLabel -> Tmp2AddrLabel jumpLabel::[]
  | Tmp3AddrBoolInstr instr -> Tmp2AddrBoolInstr instr::[]

let rec to2Addr = function
   [] -> []
 | instr::instrs -> instrTo2Addr instr @ to2Addr instrs
