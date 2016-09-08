------------------------------------------
-- Utility functions
------------------------------------------

-- 1. Sobol random number generation --
fun sobolDirVcts(): [30]int =
  [ 536870912, 268435456, 134217728, 67108864, 33554432, 16777216, 8388608, 4194304, 2097152, 1048576,
    524288,    262144,    131072,    65536,    32768,    16384,    8192,    4096,    2048,    1024,
    512,       256,       128,       64,       32,       16,       8,       4,       2,       1      ]

fun sobolInd (dirVct:  [30]int) (n: int) : int =
  let n_gray = (n >> 1) ^ n
  let res = 0
  loop (res) =
    for i < 30 do
      let t = 1 << i
      in if (n_gray & t) == t
         then res ^ dirVct[i]
         else res
  in res

fun mkRandArrInt (n: int) : [n]int =
  let sobvcts = sobolDirVcts()
  in map (sobolInd sobvcts) (map (+1) (iota n))

fun mkRandArrf32(n : int) : [n]f32 =
  let divisor = (2.0f32 ** f32(30))
  let arr = mkRandArrInt(n)
  in  map (fn x => f32(x) / divisor) arr

-- 2. sgmSumInt on integers, i.e., sgmIncScan (+) 0
fun sgmSumInt (flg : [n]int) (arr : [n]int) : [n]int =
  let flgs_vals = 
    scan ( fn (f1, x1) (f2,x2) => 
            let f = f1 | f2 in
            if f2 > 0 then (f, x2)
            else (f, x1 + x2) )
         (0,0) (zip flg arr)
  let (_, vals) = unzip flgs_vals
  in vals

fun sgmSumF32 (flg : [n]int) (arr : [n]f32) : [n]f32 =
  let flgs_vals = 
    scan ( fn (f1, x1) (f2,x2) => 
            let f = f1 | f2 in
            if f2 > 0 then (f, x2)
            else (f, x1 + x2) )
         (0,0.0f32) (zip flg arr)
  let (_, vals) = unzip flgs_vals
  in vals

