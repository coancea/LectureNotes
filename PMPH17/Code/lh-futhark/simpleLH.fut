-- Trivial Examples in Map-Reduce (MR) Form
-- ==
-- compiled input @ data/simpleLH.in
-- output @ data/simpleLH.out

--- Utilities ---
import "/futlib/math"
default(f32)
default(i32)

type int = i32
let max (x:int) (y:int) = i32.max x y

--------------------
-- len            --
--------------------
let len (a : []int) : int =
  -- ( reduce (+) 0 ) . ( map (\_ -> 1) )
  let x = map (\ _ -> 1) a
  in  reduce (+) 0 x

--------------------
-- sum            --
--------------------
let sum (a : []f32) : f32let main (a : []int) : (int, f32,bool,int) =
  let l = len   a
  let p = alln  a
  let m = maxEl a

  let b = map (\x -> f32(x) / f32(m)) a
  let s = sum b
  in (l,s,p,m)

