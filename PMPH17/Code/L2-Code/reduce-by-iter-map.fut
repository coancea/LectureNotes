-- Naive Reduce
--
-- ==
-- input { [1i32,3i32,5i32,7i32] }
-- }
-- output {
--   16i32
-- }

import "/futlib/array"

let lg (n : i32) : i32 =
  let (_,r) =
    loop (n',r) = (n,0) while n' > 0 do
        (n' >> 1, r + 1)
  in r-1

let main [n] (a : [n]i32) : i32 =
  let k = lg n
  let b = 
    loop b = a for h < k do
        let n' = n >> (h+1)
        in  map (\i -> unsafe (b[2*i]+b[2*i+1]) ) (iota n')
  in b[0]
