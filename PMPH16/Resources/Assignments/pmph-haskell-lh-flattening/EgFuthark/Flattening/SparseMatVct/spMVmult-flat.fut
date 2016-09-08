-- ASSIGNMENT 2: Flat-Parallel implementation of Sparse Matrix-Vector Multiplication
-- ==
-- compiled input @ data/small.in
-- output @ data/small.out

include util

-- please implement this function, currently dummy.
-- note that the shp array contains the sizes of each row of the matrix;
-- and we start by creating the flags of the flat matrix; please continue.
-----------------------------------------------------
---               Matrix:                         ---
---              [ 2.0, -1.0,  0.0, 0.0]          ---
---              [-1.0,  2.0, -1.0, 0.0]          ---
---              [ 0.0, -1.0,  2.0,-1.0]          ---
---              [ 0.0,  0.0, -1.0, 2.0]          ---
---                                               ---
---              IS REPRESENTED AS a list of lists---
---              [ [(0,2.0),  (1,-1.0)],          ---
---                [(0,-1.0), (1, 2.0), (2,-1.0)],---
---                [(1,-1.0), (2, 2.0), (3,-1.0)],---
---                [(2,-1.0), (3, 2.0)]           ---
---              ]                                ---
---
---              mat is the flattened form of the ---
---              above, while shp holds the sizes ---
---              of each row, i.e., [2,3,3,2]     ---
-----------------------------------------------------
fun spMatVctMult (mat : [n](f32,int)) (vct : [vct_len]f32) (shp : [num_rows]int) : [num_rows]f32 =
  let shp_sc   = scan (+) 0 shp
  let shp_inds = map (fn i => if i > 0 then unsafe shp_sc[i-1] else 0) (iota num_rows)
  let flags = write shp_inds (replicate num_rows 1) (replicate n 0)
  -- ... continue here ...
  in  replicate num_rows 0.0f32


fun main(num_rows : int, num_cols : int, vct_len : int) : [num_rows]f32 =
  -- make random vector and sparse matrix
  let vct = mkRandArrf32 vct_len
  let mat = mkRandArrf32 (num_rows*num_cols)
  let tmp = mkRandArrInt (num_rows*num_cols)
  let inds= map (fn x => x % vct_len) tmp
  let shp = replicate num_rows num_cols
  
  -- now perform matrix-vctor multiplication
  in  spMatVctMult (zip mat inds) vct shp
