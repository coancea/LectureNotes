-- Sequential implementation of Sparse Matrix-Vector Multiplication
-- ==
-- compiled input { 
--   [0,   1,     0,   1,    2,    1,   2,    3,    2,   3,   3  ]
--   [2.0, -1.0, -1.0, 2.0, -1.0, -1.0, 2.0, -1.0, -1.0, 2.0, 3.0]
--   [2, 3, 3, 2, 1]
--   [2.0, 1.0, 0.0, 3.0]
-- } 
-- output { [3.0f32, 0.0f32, -4.0f32, 6.0f32, 9.0f32] }

default(i32)
default(f32)

let spMatVctMult [vct_len] [num_rows] 
                 (mat : [](i32,f32))
                 (shp : [num_rows]i32)
                 (vct : [vct_len]f32) : [num_rows]f32 =
  let (inds, vals) = unzip mat
  let res = replicate num_rows 0.0f32
  let offset = 0i32
  -- semantically this is a map over rows
  let (res,_) =
  loop(res,offset) for i < num_rows do
    let row_len = shp[i]
    let sum = 
    loop sum=0.0f32 for j < row_len do
        unsafe sum + vals[offset+j]*vct[inds[offset+j]]
    let res[i] = sum
    let offset = offset + row_len
    in  (res,offset)
  in res

-- One may run, for example, with:
-- $ futhark-dataset --i32-bounds=0:9999 -g [1000000]i32 --f32-bounds=-7.0:7.0 -g [1000000]f32 --i32-bounds=100:100 -g [10000]i32 --f32-bounds=-10.0:10.0 -g [10000]f32 | ./spMVmult-seq -t /dev/stderr > /dev/null
let main [n] [m] 
         (mat_inds : [n]i32) (mat_vals : [n]f32) 
         (shp : [m]i32) (vct : []f32) : [m]f32 =
  spMatVctMult (zip mat_inds mat_vals) shp vct
