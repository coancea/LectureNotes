-- Sequential implementation of Sparse Matrix-Vector Multiplication
-- ==
-- compiled input @ data/small.in
-- output @ data/small.out

include util

fun spMatVctMult (mat : [](f32,int)) (vct : [vct_len]f32) (shp : [num_rows]int) : [num_rows]f32 =
  let (vals,inds) = unzip mat
  let res = replicate num_rows 0.0f32
  let offset = 0
  -- semantically this is a map over rows
  loop((res,offset)) = for i < num_rows do
    let row_len = shp[i]
    loop(sum=0.0f32) = for j < row_len do
        unsafe sum + vals[offset+j]*vct[inds[offset+j]]
    let res[i] = sum
    let offset = offset + row_len
    in (res,offset)
  in res

fun main(num_rows : int, num_cols : int, vct_len : int) : [num_rows]f32 =
  -- make random vector and sparse matrix
  let vct = mkRandArrf32 vct_len
  let mat = mkRandArrf32 (num_rows*num_cols)
  let tmp = mkRandArrInt (num_rows*num_cols)
  let inds= map (fn x => x % vct_len) tmp
  let shp = replicate num_rows num_cols
  
  -- now perform matrix-vctor multiplication
  in  spMatVctMult (zip mat inds) vct shp
