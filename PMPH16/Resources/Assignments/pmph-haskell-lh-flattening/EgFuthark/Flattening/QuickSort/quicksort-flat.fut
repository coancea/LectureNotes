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
-- compiled input @ data/small.in
-- output @ data/small.out


include util

-----------------------
--- Parallel Filter ---
-----------------------
fun filter2 (arr : [n]f32) (conds : [n]bool) : ([]int, []f32) = 
  let tflgs = map (fn b => if b then 1 else 0) conds
  let fflgs = map (fn b => 1 - b) tflgs

  let indsT = scan (+) 0 tflgs
  let tmp   = scan (+) 0 fflgs
  let lst   = indsT[n-1]
  let indsF = map (+lst) tmp

  let inds  = zipWith (fn c indT indF => if c then indT-1 else indF-1) conds indsT indsF

  let fltarr= write inds arr (copy arr)
  let flags = write ([0,lst]) ([lst,n-lst]) (replicate n 0)
  in  (flags, fltarr)

---------------------------------
--- Parallel Segmented Filter ---
---------------------------------
fun sgmfilter2 (sizes : [n]int) (arr : [n]f32) (conds : [n]bool) : ([]int, []f32) = 
  let si  = scan (+) 0 sizes
  let ssi = sgmSumInt sizes sizes

  let tfs = map (fn f => if f then 1 else 0) conds
  let isT = sgmSumInt sizes tfs
  
  let lis = map (fn s => unsafe isT[s-1]) si
  let ffs = map (fn f => if f then 0 else 1) conds
  let tmp = sgmSumInt sizes ffs
  let isF = zipWith (fn li ff => li + ff) lis tmp

  let diff= zipWith (-) si ssi
  let inds= zipWith ( fn c iT iF d => 
                        if c then iT-1+d else iF-1+d ) 
                    conds isT isF diff

  let iotas = sgmSumInt sizes (replicate n 1)
  let rsizes= zipWith ( fn f i s li => 
                              if f > 0 
                              then if li > 0 then li else f
                              else if (i-1) == li
                                   then s - li else 0 ) 
                      sizes iotas ssi lis
  let rarr= write inds arr (copy arr)
  in  (rsizes, rarr)

-----------------------
--- Flat Quicksort
-----------------------
fun randomInd ( (lb,ub) : (int,int) ) (count : int) : int =
  (count % (ub - lb + 1)) + lb

fun quicksort (arr : [n]f32) : [n]f32 = 
  let stop  = False
  let count = 0 
  let sizes = write ([0]) ([n]) (replicate n 0)
  loop((sizes,arr,stop,count)) = while (!stop) do
    let si = scan (+) 0 sizes
    let r_inds= zipWith ( fn i s =>
                            if s<1 then 0.0f32
                            else 
                                let u = si[i]
                                let l = if i > 0 then unsafe si[i-1] else 0
                                let rind = randomInd (l,u-1) count
                                in  unsafe arr[rind] )
                        (iota n) sizes
    let rands = sgmSumF32 sizes r_inds
    let conds = zipWith (>) rands arr

    let (rsizes, rarr) = sgmfilter2 sizes arr conds

    let stop = reduce (&&) True (map (fn s => s<2) rsizes)
    let count= count + 1
    in (rsizes, rarr, stop, count)
  in arr

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

  -- test flat quicksort
  let sq_arr = quicksort arr
  let quicksort_success = reduce (&&) True (map (fn i => if i == 0 then True else unsafe sq_arr[i-1]<=sq_arr[i]) (iota n))

  in  (rf_flgs, rf_vals, rsf_flgs, rsf_vals, sq_arr, quicksort_success)

