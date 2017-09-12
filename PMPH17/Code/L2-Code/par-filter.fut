-- Two-Way discriminator
--
-- ==
-- input { [1i32,2i32,3i32,4i32,5i32,6i32,7i32]
-- }
-- output {
--   [2i32,4i32,6i32,1i32,3i32,5i32,7i32] 
--   [3,0,0,4,0,0,0]
-- }

-- run with: $ futhark-dataset --i32-bounds=-100:100 -g [8388608]i32 | ./par-filter -t /dev/stderr

import "/futlib/array"

let cond (x : i32) : bool = (x & 1) == 0

let parFilter2 [n] (X : [n]i32) : 
                   ([n]i32, [n]i32) =
 let cs = map cond X
 let tfs= map (\ f->if f then 1 
                         else 0) cs
 let isT= scan (+) 0 tfs
 let i  = isT[n-1]

 let ffs= map (\f->if f then 0 
                        else 1) cs
 let isF= map (+i) (scan (+) 0 ffs)
 let inds=map (\(c,iT,iF) -> 
                  if c then iT-1 
                       else iF-1
              ) (zip cs isT isF)
 let flags = scatter (replicate n 0) 
                     [0,i] [i,n-i]
 in  (scatter (copy X) inds X, flags)

let main [n] (a : [n]i32) : ([n]i32,[n]i32) = 
  parFilter2 a


