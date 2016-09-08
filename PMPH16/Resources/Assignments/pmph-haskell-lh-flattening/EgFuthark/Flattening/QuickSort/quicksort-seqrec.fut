-- Sequential implementation of:
-- 1. filter2:    reorders the elements of an array such that the ones
--                that succeed under a predicate comes before the ones
--                that fail (the predicate), but the relative order inside
--                the two classes is preserved
-- 2. sgmfilter2: is the segmented version of filter2, i.e., it semantically
--                operates on an array of arrays and applies filter2 on each
--                subarray (segment).
-- 3. quicksort:  is the divide-and-conquer, sequential implementation of the
--                quicksort algorithm.
-- ==
-- compiled input @ data/small.in
-- output @ data/small.out

include util

-----------------------
--- Sequential Filter ---
-----------------------
fun filter2 (inp : [n]f32) (conds : [n]bool) : ([]int, []f32) =
  -- count how many elements succeed:
  loop(numT=0) = for i < n do
    if conds[i] then numT + 1 else numT
  
  -- now replace elements by maintaining two counters,
  -- for the elements that succeed and those that fail.
  --let (res, curT, curF) = (replicate n 0.0f32, 0, 0)
  loop((curT,curF,res)=(0, 0, replicate n 0.0f32)) = for i < n do
    let (ind, curTT, curFF) = 
        if conds[i]
        then (curT, curT+1, curF)
        else (numT+curF, curT, curF+1)
    let res[ind] = inp[i]
    in  (curTT,curFF,res)
  
  let flags = write ([0,numT]) ([numT,n-numT]) (replicate n 0)
  in  (flags, res)

---------------------------------
--- Sequential Segmented Filter ---
---------------------------------
fun sgmfilter2 (inp_sizes : [n]int) (inp_arr : [n]f32) (conds : [n]bool) : ([]int, []f32) =
  let res_sizes  = replicate n 0
  let res_arr    = replicate n 0.0f32
  let k = 0
  -- mainly applies the code for filter2 to process each segment
  loop((res_sizes,res_arr,k)) = while (k < n) do
    let seg_len = inp_sizes[k]
    -- count how many elements succeed:
    loop(numT=0) = for i < seg_len do
        if conds[k+i] then numT + 1 else numT
  
    -- now replace elements by maintaining two counters,
    -- for the elements that succeed and those that fail.
    let (curT, curF) = (0,0)
    loop((res_arr,curT,curF)) = 
      for i < seg_len do
        let (ind, curT, curF) = 
            if conds[k+i]
            then (curT, curT+1, curF)
            else (numT+curF, curT, curF+1)
        let res_arr[k+ind] = inp_arr[k+i]
        in  (res_arr,curT,curF)
    let res_sizes[k]      = numT
    let res_sizes[k+numT] = seg_len - numT
    in  (res_sizes,res_arr,k+seg_len)
  in (res_sizes,res_arr)

-----------------------
--- Nested Quicksort
-----------------------
fun randomInd ( (lb,ub) : (int,int) ) (count : int) : int =
  (count % (ub - lb + 1)) + lb

fun quicksort (step : int) (arr : [n]f32) : [n]f32 =  
    if n <= 1 then arr
    else let i = randomInd (0,n-1) step
         let p = arr[i]
         let conds = map (fn x => x < p) arr
         let (flgs, res) = filter2 arr conds
         let (s1,s2) = split (flgs[0]) res
         let rs1 = quicksort (step+1) s1 
         let rs2 = quicksort (step+1) s2
         in concat rs1 rs2

-----------------------
---   test program  ---
-----------------------

fun main(n : int) : ([n]int, [n]f32, [n]int, [n]f32, [n]f32, bool) =
  -- make array of random single-precision floats
  let arr = mkRandArrf32 n

  -- test filter2
  let conds = map (fn x => x > 0.5f32) arr
  let (rf_flgs, rf_vals) = filter2 arr conds

  -- test sgmfilter2
  let flags = write ([0, n/2]) ([n/2, n - n/2]) (replicate n 0)
  let (rsf_flgs, rsf_vals) = sgmfilter2 flags arr conds

  -- test nested quicksort
  let sq_arr = quicksort 0 arr
  let quicksort_success = reduce (&&) True (map (fn i => if i == 0 then True else unsafe sq_arr[i-1]<=sq_arr[i]) (iota n))

  in  (rf_flgs, rf_vals, rsf_flgs, rsf_vals, sq_arr, quicksort_success)

