default(f32)
default(i32)

type int = i32

-- 1. Sobol random number generation --
let sobolDirVcts(): [30]int =
  [ 536870912, 268435456, 134217728, 67108864, 33554432, 16777216, 8388608, 4194304, 2097152, 1048576,
    524288,    262144,    131072,    65536,    32768,    16384,    8192,    4096,    2048,    1024,
    512,       256,       128,       64,       32,       16,       8,       4,       2,       1      ]

let sobolInd (dirVct:  [30]int) (n: int) : int =
  let n_gray = (n >> 1) ^ n
  let res = 0
  let res = loop (res) for i < 30 do
      let t = 1 << i
      in if (n_gray & t) == t
         then res ^ dirVct[i]
         else res
  in res

let mkRandArrInt (n: int) : [n]int =
  let sobvcts = sobolDirVcts()
  in map (sobolInd sobvcts) (map (+1) (iota n))

let mkRandArrf32(n : int) : [n]f32 =
  let divisor = (2.0 ** f32(30))
  let arr = mkRandArrInt(n)
  in  map (\x -> f32(x) / divisor) arr

-----------------------
-----------------------
let main(n: int, amplitude : int): [n]int =
    let rands = mkRandArrInt (n)
    in  map (\x -> (x % amplitude) - (amplitude/2) ) rands

