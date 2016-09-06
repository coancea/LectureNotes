default(f32)

------------------------------------------
-- Util: Sobol random number generation --
------------------------------------------
fun sobolDirVcts(): [30]int =
  [ 536870912, 268435456, 134217728, 67108864, 33554432, 16777216, 8388608, 4194304, 2097152, 1048576,
    524288,    262144,    131072,    65536,    32768,    16384,    8192,    4096,    2048,    1024,
    512,       256,       128,       64,       32,       16,       8,       4,       2,       1      ]

fun sobolInd(dirVct:  [30]int, n: int ): int =
  let n_gray = (n >> 1) ^ n
  let res = 0
  loop (res) =
    for i < 30 do
      let t = 1 << i
      in if (n_gray & t) == t
         then res ^ dirVct[i]
         else res
  in res

-----------------------
-----------------------
fun main(n: int): [n]int =
    let sobvcts = sobolDirVcts()
    in map (fn  (ind: int): int  =>
                sobolInd(sobvcts, ind+1)
           ) (iota n)

