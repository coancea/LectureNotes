-- Native Reduce
--
-- ==
-- input { [1i32,3i32,5i32,7i32] }
-- }
-- output {
--   16i32
-- }

-- run with: $ futhark-dataset --i32-bounds=-100:100 -g [8388608]i32 | ./reduce-native -t /dev/stderr

import "/futlib/array"

let main [n] (a : [n]i32) : i32 = reduce_comm (+) 0i32 a

