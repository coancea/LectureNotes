-- Flat implementation of:
-- 1. filter2:    reorders the elements of an array such that the ones
--                that succeed under a predicate comes before the ones
--                that fail (the predicate), but the relative order inside
--                the two classes is preserved
-- 2. sgmfilter2: is the segmented version of filter2, i.e., it semantically
--                operates on an array of arrays and applies filter2 on each
--                subarray (segment).
-- 3. quicksort:  is the flat-parallel version of quicksort algorithm.
--                quicksort implementation uses sgmfilter2.
-- ==
-- compiled input { [29,5,7,11,2,3,13,23,17,19] } output { [2,3,5,7,11,13,17,19,23,29] }

import "/futlib/array"

import "util"

-----------------------
--- Parallel Filter ---
-----------------------
let filter2 [n] (arr : [n]f32) (conds : [n]bool) : ([]i32, []f32) = 
  let tflgs = map (\ b -> if b then 1 else 0) conds
  let fflgs = map (\ b -> 1 - b) tflgs

  let indsT = scan (+) 0 tflgs
  let tmp   = scan (+) 0 fflgs
  let lst   = indsT[n-1]
  let indsF = map (+lst) tmp

  let inds  = map (\ c indT indF -> if c then indT-1 else indF-1) conds indsT indsF

  let fltarr= scatter (copy arr) inds arr 
  let flags = scatter (replicate n 0) ([0,lst]) ([lst,n-lst])
  in  (flags, fltarr)

---------------------------------
--- Parallel Segmented Filter ---
---------------------------------
let sgmfilter2 [n] (sizes : [n]i32) (arr : [n]f32) (conds : [n]bool) : ([]i32, []f32) = 
  let si  = scan (+) 0 sizes
  let ssi = sgmSumInt sizes sizes

  let tfs = map (\ f -> if f then 1 else 0) conds
  let isT = sgmSumInt sizes tfs
  
  let lis = map (\ s -> unsafe isT[s-1]) si
  let ffs = map (\ f -> if f then 0 else 1) conds
  let tmp = sgmSumInt sizes ffs
  let isF = map (\ li ff -> li + ff) lis tmp

  let diff= map (-) si ssi
  let inds= map (\ c iT iF d -> 
                        if c then iT-1+d else iF-1+d ) 
                conds isT isF diff

  let iotas = sgmSumInt sizes (replicate n 1)
  let rsizes= map (\ f i s li -> 
                        if f > 0 
                        then if li > 0 then li else f
                        else if (i-1) == li
                        then s - li else 0 ) 
                  sizes iotas ssi lis
  let rarr= scatter (copy arr) inds arr
  in  (rsizes, rarr)

-----------------------
--- Flat Quicksort
-----------------------
let randomInd ( (lb,ub) : (i32,i32) ) (count : i32) : i32 =
  (count % (ub - lb + 1)) + lb

let quicksort [n] (arr : [n]f32) : [n]f32 = 
  let stop  = false
  let count = 0 
  let sizes = scatter (replicate n 0) ([0]) ([n])
  let (_,arr,_,_) =
    loop(sizes,arr,stop,count) while (!stop) do
      let si = scan (+) 0 sizes
      let r_inds= map (\ i s ->
                        if s<1 then 0.0f32
                        else let u = si[i]
                             let l = if i > 0 then unsafe si[i-1] else 0
                             let rind = randomInd (l,u-1) count
                             in  unsafe arr[rind] )
                      (iota n) sizes
      let rands = sgmSumF32 sizes r_inds
      let conds = map (>) rands arr

      let (rsizes, rarr) = sgmfilter2 sizes arr conds

      -- let stop1 = reduce (&&) true (map (\ s -> s<2) rsizes)
      let stop = reduce (&&) true (map (\i-> unsafe (rarr[i] <= rarr[i+1])) (iota (n-1)))
      let count= count + 1
      in (rsizes, rarr, stop, count)
  in arr

-----------------------
---   test program  ---
-----------------------

let main [n] (arr : [n]f32) : [n]f32 = quicksort arr

--let main(n : i32) : ([n]i32, [n]f32, [n]i32, [n]f32, [n]f32, bool) =
--  -- make array of random single-precision floats
--  let arr = mkRandArrf32 n
--
--  -- test filter2
--  let conds = map (\ x -> x > 0.5f32) arr
--  let (rf_flgs, rf_vals) = filter2 arr conds
--
--  -- test sgmfilter2
--  let flags = scatter (replicate n 0) ([0, n/2]) ([n/2, n - n/2])
--  let (rsf_flgs, rsf_vals) = sgmfilter2 flags arr conds
--
--  -- test flat quicksort
--  let sq_arr = quicksort arr
--  let quicksort_success = reduce (&&) true (map (\ i -> if i == 0 then true else unsafe sq_arr[i-1]<=sq_arr[i]) (iota n))
--
--  in  (rf_flgs, rf_vals, rsf_flgs, rsf_vals, sq_arr, quicksort_success)

