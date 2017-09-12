-- Primes: Nested-Par Version
-- Since Futhark does not support irregular array,
-- we pad to the maximal allowed dimension.
-- ==
-- compiled input { 30 } output { [2,3,5,7,11,13,17,19,23,29] }

let primesOpt (n : i32) : ([]i32, i32) =
  if n <= 4 then ([2,3],2)
  else let sq= i32( f64.sqrt( f64(n) ) )
       let (sq_primes, sq_sz) = primesOpt sq
           -- composite = map (\ p -> let m = (n `div` p)
           --                      in  map (\ j -> j * p ) (drop 2 (iota (m+1)))
           --                 ) sqrt_primes
           -- not_primes = reduce (++) [] composite
       -- Morally, it should be the nested-parallel code above,
       -- but since Futhark does not support irregular arrays
       -- we write it as a loop nest in which we precompute 
       -- the total result size 
       let sizes = map (\ p -> n / p - 1 ) sq_primes
       let flat_size = reduce (+) 0 sizes
       let not_primes = replicate flat_size 0
       let cur_ind = 0
       let (not_primes, cur_ind) =
         loop (not_primes, cur_ind) for i < sq_sz do
            let p = sq_primes[i]
            let s = sizes[i]
            let not_primes = 
                loop(not_primes) for j < s do
                  let not_primes[cur_ind+j] = (j+2)*p
                  in  not_primes
            in  (not_primes, cur_ind + s)
       let zero_array = replicate flat_size 0
       let prime_flags = write not_primes zero_array (replicate (n+1) 1)
       let prime_flags[0] = 0
       let prime_flags[1] = 0
       in  filter (\i->unsafe prime_flags[i]!=0) (iota (n+1))
       
let main (n : i32) : []i32 = let (res,_) = primesOpt n in res

