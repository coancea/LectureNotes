------------------------------------------
-- Utility functions
------------------------------------------

-- 1. sgmSumInt on integers, i.e., sgmIncScan (+) 0
fun sgmSumInt (flg : [n]int) (arr : [n]int) : [n]int =
  let flgs_vals = 
    scan ( fn (f1, x1) (f2,x2) => 
            let f = f1 | f2 in
            if f2 > 0 then (f, x2)
            else (f, x1 + x2) )
         (0,0) (zip flg arr)
  let (_, vals) = unzip flgs_vals
  in vals

-- 2. sgmSumF32 on floats, i.e., sgmIncScan (+) 0.0
fun sgmSumF32 (flg : [n]int) (arr : [n]f32) : [n]f32 =
  let flgs_vals = 
    scan ( fn (f1, x1) (f2,x2) => 
            let f = f1 | f2 in
            if f2 > 0 then (f, x2)
            else (f, x1 + x2) )
         (0,0.0f32) (zip flg arr)
  let (_, vals) = unzip flgs_vals
  in vals

