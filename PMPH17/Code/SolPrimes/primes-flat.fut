-- Primes: Flat-Parallel Version
-- ==
-- compiled input { 30 } output { [2,3,5,7,11,13,17,19,23,29] }

default(i32)
default(f32)

import "/futlib/math"

let primesFlat (n : i32) : []i32 =
  if n <= 8 then [2,3,5,7]
  else let sq= i32( f32.sqrt( f32(n) ) )
       let (sq_primes, num_primes) = primesFlat sq
       let mult_lens = map (\ p -> n / p - 1 ) sq_primes
       let mult_scan = scan (+) 0 mult_lens
       let mult_tot_len = mult_scan[num_primes-1]
       let mult_scan = map (\ i -> if i > 0 then unsafe mult_scan[i-1] else 0) (iota num_primes)

       let zeros = replicate mult_tot_len (0,0)
       let (ps,flags) = unzip (write mult_scan (zip sq_primes (replicate num_primes 1)) zeros)
    
       -- let prime_vals= segmScanInc (+) 0 flags ps
       -- prime_inds= segmScanInc (+) 0 flags (replicate mult_tot_len 1)
       let (_, prime_vals, prime_inds) = unzip (
            scan (\ (f1,v1,i1) (f2,v2,i2) ->
                        let f = f1 | f2 
                        in  if f2 == 1
                            then (f, v2, i2) 
                            else (f, v1+v2, i1+i2)
                 ) (0,0,0) (zip flags ps (replicate mult_tot_len 1))
        )
       let not_primes = map (\ i v -> (i+1)*v) prime_inds prime_vals
       let zero_array = replicate mult_tot_len 0
       let mostly_ones= map (\ x -> if x > 1 then 1 else 0) (iota (n+1))
       let prime_flags= scatter mostly_ones not_primes zero_array
       in  filter (\i -> unsafe prime_flags[i] != 0) (iota (n+1))

let main (n : i32) : []i32 = primesFlat n
