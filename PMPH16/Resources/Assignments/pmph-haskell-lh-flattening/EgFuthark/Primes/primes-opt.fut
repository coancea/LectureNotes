-- Primes: Nested-Par Version
-- Since Futhark does not support irregular array,
-- we pad to the maximal allowed dimension.
-- ==
-- compiled input @ data/primes-small.in
-- output @ data/primes-small.out


fun i32_filter (flags: [k]int) (ns: *[k]int): (*[]int, int) =
  let is0 = scan (+) 0 flags
  let filter_size = is0[k - 1]
  let is1 = map (fn (i: int, f: int): int  => i * f) (zip is0 flags)
  let is2 = map (fn (i: int): int  => i - 1) is1
  in (write is2 ns (replicate filter_size 0), filter_size)


fun primesOpt (n : int) : ([]int, int) =
  if n <= 4 then ([2,3],2)
  else let sq= int( sqrt32( f32(n) ) )
       let (sq_primes, sq_sz) = primesOpt sq
           -- composite = map (\ p -> let m = (n `div` p)
           --                      in  map (\ j -> j * p ) (drop 2 (iota (m+1)))
           --                 ) sqrt_primes
           -- not_primes = reduce (++) [] composite
       -- Morally, it should be the nested-parallel code above,
       -- but since Futhark does not support irregular arrays
       -- we write it as a loop nest in which we precompute 
       -- the total result size 
       let sizes = map (fn p => n / p - 1 ) sq_primes
       let flat_size = reduce (+) 0 sizes
       let not_primes = replicate flat_size 0
       let cur_ind = 0
       loop ((not_primes, cur_ind)) = for i < sq_sz do
            let p = sq_primes[i]
            let s = sizes[i]
            loop(not_primes) = for j < s do
                  let not_primes[cur_ind+j] = (j+2)*p
                  in  not_primes
            in  (not_primes, cur_ind + s)
       let zero_array = replicate flat_size 0
       let prime_flags = write not_primes zero_array (replicate (n+1) 1)
       let prime_flags[0] = 0
       let prime_flags[1] = 0
       in  i32_filter prime_flags (iota (n+1))
       
fun main (n : int) : []int = let (res,_) = primesOpt n in res

