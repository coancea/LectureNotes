-- For Assignment 1, Task 3, Please implement the Flat-Parallel Version 
-- of prime numbers computation (sieve).
-- ==
-- compiled input @ data/primes-small.in
-- output @ data/primes-small.out

include util

-- flags is semantically an array of booleans (values 0 and 1).
-- i32_filter filters out the elements of the array ns that  
-- correspond to a 0 flag.
fun i32_filter (flags: [k]int) (ns: *[k]int): (*[]int, int) =
  let is0 = scan (+) 0 flags
  let filter_size = is0[k - 1]
  let is1 = map (fn (i: int, f: int): int  => i * f) (zip is0 flags)
  let is2 = map (fn (i: int): int  => i - 1) is1
  in (write is2 ns (replicate filter_size 0), filter_size)

-- ASSIGNMENT 1, Task 3: implement below the flat
-- The current dummy implementation only recognizes
-- [2,3,5,7] as prime numbers.
-- Your implementation should recognize all prime numbers less than n.
fun primesFlat (n : int) : ([]int, int) =
  if n <= 8 then ([2,3,5,7],4)
  else let sq= int( sqrt32( f32(n) ) )
       let (sq_primes, num_primes) = primesFlat sq
       -- Insert your code here!
       in  (sq_primes, num_primes)

fun main (n : int) : []int = let (res,_) = primesFlat n in res
