open Datatypesv1

(* Currently just never use EAX and EDX for anything else. *)
let instrToWonky = function
    Tmp2AddrBinop(Tmp2AddrDiv(divisor, dest)) ->
    (* AssemLocForTmp is different from TmpAssemLoc.
       Check out datatypesv1.ml *)
       Tmp2AddrInstr (Tmp2AddrMov(TmpAssemLoc dest,
                                 AssemLocForTmp (Reg EAX)))::
       TmpCDQ::TmpIDIV(divisor)::
       Tmp2AddrInstr (Tmp2AddrMov(AssemArg (AssemLoc (Reg EAX)),
                                  dest))::[]
  | instr -> [Tmp2AddrInstr instr]

let rec toWonky2Addr = function
    [] -> []
  | instr::instrs -> instrToWonky instr @ toWonky2Addr instrs
