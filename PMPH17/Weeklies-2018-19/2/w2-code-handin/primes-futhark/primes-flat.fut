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

      let mult_lens = map (\ p -> len / p - 1 ) sq_primes
      
      --------------------------------------------------------------
      -- The current iteration knowns the primes <= 'len', 
      --  based on which it will compute the primes <= 'len*len'
      -- ToDo: replace the dummy code below with the flat-parallel
      --  code that is equivalent with the nested-parallel one:
      --   let composite = map (\ p -> let m = (n `div` p)
      --                               in  map (\ j -> j * p ) (drop 2 (iota (m+1)))
      --                       ) sq_primes
      --   let not_primes = reduce (++) [] composite
      --
      let flat_size = reduce (+) 0 mult_lens
      let not_primes = replicate flat_size 0
      -- If not_primes is correctly computed, then the remaining
      -- code is correct and will do the job of computing the prime
      -- numbers up to n!
      --------------------------------------------------------------
      --------------------------------------------------------------

       let zero_array = replicate flat_size 0
       let mostly_ones= map (\ x -> if x > 1 then 1 else 0) (iota (len+1))
       let prime_flags= scatter mostly_ones not_primes zero_array
       let sq_primes = filter (\i-> i>1 && i<=n && unsafe prime_flags[i]>0i32) (0...len)

       in  (sq_primes, len)

  in sq_primes

-- RUN a big test with:
-- $ futhark-opencl primes-flat.fut
-- $ echo "10000000" | ./primes-flat -t /dev/stderr -r 10 > /dev/null
let main (n : i32) : []i32 = primesFlat n
