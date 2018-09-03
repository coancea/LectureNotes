-- Native reduction in Futhark
-- ==
-- compiled input {
--    [1.0f32, -2.0, -2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 3.0, 4.0, -6.0, 1.0, 2.0, -3.0, 7.0, 2.0]
-- }
-- output {
--    7.0f32
-- }
-- compiled input @ data/f32-arr-16777216.in
-- output { 3091.746094f32 }

let main [n] (a : [n]f32) : f32 =
    reduce (+) 0.0f32 a
