-- Segmented scan
--
-- ==
-- input { [2i32,0i32,2i32,0i32]
--         [1i32,2i32,3i32,4i32]
-- }
-- output {
--   [1i32,3i32,3i32,7i32] 
-- }

-- run with: $ futhark-dataset --i32-bounds=-100:100 -g [8388608]i32 | ./sgm-scan -t /dev/stderr

import "/futlib/array"
import "/futlib/monoid"

module segmented_scan(M: monoid): {
  val segmented_scan : []i32 -> []M.t -> []M.t
} = {
  let segmented_scan [n] (flags: [n]i32) (as: [n]M.t): [n]M.t =
    #2 (unzip (scan (\(x_flag,x) (y_flag,y) ->
                     if y_flag > 0
                     then (x_flag | y_flag, y)
                     else (x_flag | y_flag, M.op x y))
                    (0i32, M.ne)
                    (zip flags as)))
}

module SS = segmented_scan { type t = i32
                             let ne = 0i32
 		                     let op (x:i32) (y:i32) = x i32.+ y
                           }

let main [n] (flags : [n]i32) (vals : [n]i32) : [n]i32 =
  SS.segmented_scan flags vals
