-- Trivial Examples in Map-Reduce (MR) Form
-- ==
-- compiled input @ data/egLH-small.in
-- output @ data/egLH-small.out

--- Utilities ---
fun max (x: int) (y: int) : int =
  if x > y then x else y

--------------------
-- len            --
--------------------
fun len (a : [n]int) : int =
  -- ( reduce (+) 0 ) . ( map (\_ -> 1) )
  let x = map (fn el => 1) a
  in  reduce (+) 0 x

--------------------
-- sum            --
--------------------
fun sum (a : [n]f32) : f32 =
  -- (reduce (+) 0) . (map id)
  let x = map (fn el => el) a
  in  reduce (+) 0.0f32 x

--------------------
-- flatten        --
--------------------
fun flatten(a : [n][]int) : []int =
  -- (reduce (++) []) . (map id)
  let x = map (fn el => el) a
  in  reduce (fn (y : []int) (z : []int) => concat y z) (replicate 0 0) x 
       -- ++ in Futhark is named concat

-----------
-- all p --
-----------
fun alln(a : [n]int) : bool =
  -- (reduce (&&) True) . (map p)
  let x = map pred1 a
  in  reduce (&&) True x

fun pred1 ( x : int) : bool = (x % 2) == 0

-------------
-- maxList --
-------------
fun maxEl (a : [n]int) : int =
  -- (reduce (max) (-1000000)) . (map id)
  let x = map (fn el => el) a
  in  reduce max (-1000000) x

----------------------------------------
--- MAIN                             ---
----------------------------------------
fun main (a : [n]int) : (int, f32,bool,int) =
  let l = len   a
  let f = flatten (replicate 4 a) 
  let p = alln  a
  let m = maxEl a

  let b = map (fn x => f32(x) / f32(m)) a
  let s = sum b
  in (l,s,p,m)

