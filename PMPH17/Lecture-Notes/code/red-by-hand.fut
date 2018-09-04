-- Reduction by hand in Futhark
-- ==
-- compiled input {
--    [1.0f32, -2.0, -2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 3.0, 4.0, -6.0, 1.0, 2.0, -3.0, 7.0, 2.0]
-- }
-- output {
--    7.0f32
-- }
-- compiled input @ data/f32-arr-16777216.in
-- output { 3091.746094f32 }

-- For simplicity, assumes that n is 2^k
let main [n] (a : [n]f32) : f32 =
  let k = t32 <| f32.log2 <| r32 n
  let b = 
    loop b = a for h < k do
        let n' = n >> (h+1)
        in  map (\i -> unsafe (b[2*i]+b[2*i+1]) ) (iota n')
  in b[0]
