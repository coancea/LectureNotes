-- Trivial Examples in List-Hom (LH) form
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
  if      n == 0 then 0
  else if n == 1 then 1
  else let (x,y) = split (n/2) a
       in  (len x) + (len y)

--------------------
-- sum            --
--------------------
fun sum (a : [n]f32) : f32 =
  if      n == 0 then 0.0f32
  else if n == 1 then a[0]
  else let (x,y) = split (n/2) a
       in  (sum x) + (sum y)

--------------------
-- flatten        --
--------------------
fun flatten(a : [n][]int) : []int =
  if      n == 0 then replicate 0 1
  else if n == 1 then a[0]
  else let (x,y) = split (n/2) a
       in  concat (flatten x) (flatten y)
       -- ++ in Futhark is named concat

-----------
-- all p --
-----------
fun alln(a : [n]int) : bool =
  if      n == 0 then True
  else if n == 1 then pred1 a[0]
  else let (x,y) = split (n/2) a
       in  (alln x) && (alln y)

fun pred1 ( x : int) : bool = (x % 2) == 0

-------------
-- maxList --
-------------
fun maxEl (a : [n]int) : int = 
  if      n == 0 then -1000000
  else if n == 1 then a[0]
  else let (x,y) = split (n/2) a
       in  max (maxEl x) (maxEl y)

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

