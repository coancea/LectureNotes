------------------------------------------
-- Utility functions
------------------------------------------

-- 1. Sobol random number generation --
let sobolDirVcts(): [30]i32 =
  [ 536870912, 268435456, 134217728, 67108864, 33554432, 16777216, 8388608, 4194304, 2097152, 1048576,
    524288,    262144,    131072,    65536,    32768,    16384,    8192,    4096,    2048,    1024,
    512,       256,       128,       64,       32,       16,       8,       4,       2,       1      ]

let sobolInd (dirVct:  [30]i32) (n: i32) : i32 =
  let n_gray = (n >> 1) ^ n
  let res = loop res = 0
    for i < 30 do
      let t = 1 << i
      in if (n_gray & t) == t
         then res ^ dirVct[i]
         else res
  in res

let mkRandArri32 (n: i32) : [n]i32 =
  let sobvcts = sobolDirVcts()
  in map (sobolInd sobvcts) (map (+1) (iota n))

let mkRandArrf32(n : i32) : [n]f32 =
  let divisor = (2.0f32 ** f32(30))
  let arr = mkRandArri32(n)
  in  map (\ x -> f32(x) / divisor) arr

-- 2. sgmSumInt on integers, i.e., sgmIncScan (+) 0
let sgmSumInt [n] (flg : [n]i32) (arr : [n]i32) : [n]i32 =
  let flgs_vals = 
    scan ( \ (f1, x1) (f2,x2) -> 
            let f = f1 | f2 in
            if f2 > 0 then (f, x2)
            else (f, x1 + x2) )
         (0,0) (zip flg arr)
  let (_, vals) = unzip flgs_vals
  in vals

let sgmSumF32 [n] (flg : [n]i32) (arr : [n]f32) : [n]f32 =
  let flgs_vals = 
    scan ( \ (f1, x1) (f2,x2) -> 
            let f = f1 | f2 in
            if f2 > 0 then (f, x2)
            else (f, x1 + x2) )
         (0,0.0f32) (zip flg arr)
  let (_, vals) = unzip flgs_vals
  in vals

