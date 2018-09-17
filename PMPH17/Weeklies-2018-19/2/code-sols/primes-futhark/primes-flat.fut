-- Primes: Flat-Parallel Version
-- ==
-- compiled input { 30 } output { [2,3,5,7,11,13,17,19,23,29] }

import "/futlib/math"

let primesFlat (n : i32) : []i32 =
  let sq_primes   = [2,3,5,7]
  let len  = 8
  let (sq_primes, _) =
    loop (sq_primes, len) while len < n do
      -- this is "len = min n (len*len)" 
      -- but without running out of i32 bounds 
      let len = if n / len < len then n else len*len

      let num_primes= length sq_primes
      let mult_lens = map (\ p -> len / p - 1 ) sq_primes
      let mult_scan = scan (+) 0 mult_lens
      let mult_tot_len = last mult_scan
      let mult_scan = map (\ i -> if i > 0 then unsafe mult_scan[i-1] else 0) (iota num_primes)

      let zeros = replicate mult_tot_len (0,0)
      let (ps,flags) = unzip (scatter zeros mult_scan (zip sq_primes (replicate num_primes 1)) )
    
       -- let prime_vals= segmScanInc (+) 0 flags ps
       -- prime_inds= segmScanInc (+) 0 flags (replicate mult_tot_len 1)
       let (_, prime_vals, prime_inds) = unzip3 (
            scan (\ (f1,v1,i1) (f2,v2,i2) ->
                        let f = f1 | f2 
                        in  if f2 == 1
                            then (f, v2, i2) 
                            else (f, v1+v2, i1+i2)
                 ) (0,0,0) (zip3 flags ps (replicate mult_tot_len 1))
        )
       let not_primes = map2 (\ i v -> (i+1)*v) prime_inds prime_vals

       let mm = mult_tot_len
       let zero_array = replicate mm 0
       let mostly_ones= map (\ x -> if x > 1 then 1 else 0) (iota (len+1))
       let prime_flags= scatter mostly_ones not_primes zero_array
       let sq_primes = filter (\i -> unsafe prime_flags[i] != 0) (iota (len+1))
       in  (sq_primes, len)
  in sq_primes

      -- the current iteration known the primes <= 'len', 
      -- based on which it will compute the primes <= 'len*len' 
      -- Morally, the code should be the nested-parallel code above,
           -- composite = map (\ p -> let m = (n `div` p)
           --                      in  map (\ j -> j * p ) (drop 2 (iota (m+1)))
           --                 ) sq_primes
           -- not_primes = reduce (++) [] composite
      -- but since Futhark does not support irregular arrays
      -- we write it as a loop nest in which we precompute
      -- the total result size

let main (n : i32) : []i32 = primesFlat n
