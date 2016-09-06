-- Primes: Flat-Parallel Version
-- ==
-- compiled input @ data/primes-small.in
-- output @ data/primes-small.out


fun i32_filter (flags: [k]int) (ns: *[k]int): (*[]int, int) =
  let is0 = scan (+) 0 flags
  let filter_size = is0[k - 1]
  let is1 = map (fn (i: int, f: int): int  => i * f) (zip is0 flags)
  let is2 = map (fn (i: int): int  => i - 1) is1
  in (write is2 ns (replicate filter_size 0), filter_size)

fun primesFlat (n : int) : ([]int, int) =
  if n <= 8 then ([2,3,5,7],4)
  else let sq= int( sqrt32( f32(n) ) )
       let (sq_primes, num_primes) = primesFlat sq
       let mult_lens = map (fn p => n / p - 1 ) sq_primes
       let mult_scan = scan (+) 0 mult_lens
       let mult_tot_len = mult_scan[num_primes-1]
       let mult_scan = zipWith (fn i => if i > 0 then unsafe mult_scan[i-1] else 0) (iota num_primes)

       let zeros1 = replicate mult_tot_len 0
       let flags = write mult_scan (replicate num_primes 1) zeros1
       let zeros2 = copy(replicate mult_tot_len 0)
       let ps    = write mult_scan sq_primes                zeros2
    
       -- let prime_vals= segmScanInc (+) 0 flags ps
       -- prime_inds= segmScanInc (+) 0 flags (replicate mult_tot_len 1)
       let (_, prime_vals, prime_inds) = unzip (
            scan (fn (f1,v1,i1) (f2,v2,i2) =>
                        let f = f1 | f2 
                        in  if f2 == 1
                            then (f, v2, i2) 
                            else (f, v1+v2, i1+i2)
                 ) (0,0,0) (zip flags ps (replicate mult_tot_len 1))
        )
       let not_primes = zipWith (fn i v => (i+1)*v) prime_inds prime_vals
       let zero_array = replicate mult_tot_len 0
       let mostly_ones= map (fn x => if x > 1 then 1 else 0) (iota (n+1))
       let prime_flags= write not_primes zero_array mostly_ones
       in  i32_filter prime_flags (iota (n+1))

fun main (n : int) : []int = let (res,_) = primesFlat n in res
