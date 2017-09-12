-- For Assignment 1, Task 3, Please implement the Flat-Parallel Version 
-- of prime numbers computation (sieve).
-- ==
-- compiled input { 30 } output { [2,3,5,7,11,13,17,19,23,29] }

import "/futlib/array"

-- ASSIGNMENT 1, Task 3: implement below the flat
-- The current dummy implementation only recognizes
-- [2,3,5,7] as prime numbers.
-- Your implementation should recognize all prime numbers less than n.
let primesFlat (n : i32) : []i32 =
  if n <= 8 then [2,3,5,7]
  else let sq= i32( f64.sqrt (f64 n) )
       let sq_primes = [2,3,5,7] -- primesFlat sq
       ----------------------------------------
       -- Here insert your code that computes the
       -- not-primes array of length mm.
       -- The two lines below are dummy, replace them!
       -- ...
       let not_primes = sq_primes
       let mm = size (0) not_primes
       -- ...
       -- ... 
       -- This is how the implementation should end,
       -- i.e., the code remanining after flattening
       -- the nested maps.
       let zero_array = replicate mm 0
       let mostly_ones= map (\ x -> if x > 1 then 1 else 0) (iota (n+1))
       let prime_flags= scatter mostly_ones not_primes zero_array
       in  filter (\i -> unsafe prime_flags[i] != 0) (iota (n+1))

let main (n : i32) : []i32 = primesFlat n
