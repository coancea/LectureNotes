-- ASSIGNMENT 2: Flat-Parallel implementation of Sparse Matrix-Vector Multiplication
-- ==
-- compiled input { 
--   [0,   1,     0,   1,    2,    1,   2,    3,    2,   3,   3  ]
--   [2.0, -1.0, -1.0, 2.0, -1.0, -1.0, 2.0, -1.0, -1.0, 2.0, 3.0]
--   [2, 3, 3, 2, 1]
--   [2.0, 1.0, 0.0, 3.0]
-- } 
-- output { [3.0f32, 0.0f32, -4.0f32, 6.0f32, 9.0f32] }

import "/futlib/segmented"


-----------------------------------------------------
-- Please implement the function below, currently dummy.
-- note that the shp array contains the sizes of each row of the matrix;
-- and we start by creating the flags of the flat matrix; please continue.
-----------------------------------------------------
---               Matrix:                         ---
---              [ 2.0, -1.0,  0.0, 0.0]          ---
---              [-1.0,  2.0, -1.0, 0.0]          ---
---              [ 0.0, -1.0,  2.0,-1.0]          ---
---              [ 0.0,  0.0, -1.0, 2.0]          ---
---              [ 0.0,  0.0,  0.0, 3.0]          ---
---                                               ---
---              IS REPRESENTED AS a list of lists---
---              [ [(0,2.0),  (1,-1.0)],          ---
---                [(0,-1.0), (1, 2.0), (2,-1.0)],---
---                [(1,-1.0), (2, 2.0), (3,-1.0)],---
---                [(2,-1.0), (3, 2.0)],          ---
---                [(3,3.0)]
---              ]                                ---
---
---              mat is the flattened form of the ---
---              above, while shp holds the sizes ---
---              of each row, i.e., [2,3,3,2,1]   ---
---                                                  
---              The vector is full and matches   ---
---               the matrix number of columns,   ---
---               e.g., x = [2.0, 1.0, 0.0, 3.0]  ---
---                     (transposed)              ---
-----------------------------------------------------
let spMatVctMult [num_elms] [vct_len] [num_rows] 
                 (mat : [num_elms](i32,f32))
                 (shp : [num_rows]i32)
                 (vct : [vct_len]f32) : [num_rows]f32 =

  let shp_sc   = scan (+) 0 shp
  let shp_inds = map (\ i -> if i > 0 then unsafe shp_sc[i-1] else 0) (iota num_rows)
  let flags    = scatter (replicate num_elms false) shp_inds (replicate num_rows true)
  let v_mul_xis = map (\(i,v) -> unsafe (v * vct[i]) ) mat
  let segm_mat  = segmented_scan (+) 0f32 flags v_mul_xis -- sgmSumF32 flags v_mul_xis
  in  map (\i -> unsafe (segm_mat[i-1])) shp_sc

let main [n] [m] 
         (mat_inds : [n]i32) (mat_vals : [n]f32) 
         (shp : [m]i32) (vct : []f32) : [m]f32 =
  spMatVctMult (zip mat_inds mat_vals) shp vct

-- One may run with for example:
-- $ futhark-dataset --i32-bounds=0:9999 -g [1000000]i32 --f32-bounds=-7.0:7.0 -g [1000000]f32 --i32-bounds=100:100 -g [10000]i32 --f32-bounds=-10.0:10.0 -g [10000]f32 | ./spMVmult-seq -t /dev/stderr > /dev/null

