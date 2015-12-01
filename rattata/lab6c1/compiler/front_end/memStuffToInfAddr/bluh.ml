let largestPowerOfTwo (x : int) (y : int) =
  if y = 1 then 2**x
  else largestPowerOfTwo (x + 1) (y / 2)


let helper (multiplesOfTwo : int) (s : int) (index_final : tmpIntExpr) =
  if s = 4 then TmpInfAddrBinopExpr(LSHIFT, index_final, TmpIntArg(TmpConst 2))
  else if s = 8 then TmpInfAddrBinopExpr(LSHIFT, index_final, TmpIntArg(TmpConst 3))
  else
    let x = s mod 2 in
    if x = 0 then helper (multiplesOfTwo + 1) (s / 2) (index_final)
    else
      
