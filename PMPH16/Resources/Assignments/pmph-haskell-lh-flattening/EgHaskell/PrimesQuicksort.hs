import System.Environment -- access to arguments etc.
import Data.Bits
import Debug.Trace

import System.IO.Unsafe  -- be careful!
import System.Random

import qualified Data.List as DL

import Helpers

------------------------------------
--- Scan Inclusive and Exclusive ---
--- and Segmented Scan Inclusive ---
------------------------------------

scanInc :: (a->a->a) -> a -> [a] -> [a]
scanInc myop ne arr = tail $ scanl myop ne arr

scanExc :: (a->a->a) -> a -> [a] -> [a]
scanExc myop ne arr = let n = length arr
                          r = scanl myop ne arr
                      in  take n r

segmScanInc :: (a->a->a) -> a -> [Int] -> [a] -> [a]
segmScanInc myop ne flags arr =
    let fvs = zip flags arr
        (_,a) = unzip $
                scanl (\(f1,v1) (f2,v2) ->
                        let f = f1 .|. f2
                        in  if f2 == 0
                            then (f, v1 `myop` v2)
                            else (f, v2)
                      ) (0,ne) fvs
    in  tail a

segmScanExc :: (a->a->a) -> a -> [Int] -> [a] -> [a]
segmScanExc myop ne flags arr =
    let inds   = iota (length arr)
        adj_arr= zipWith (\i f -> if f>0 then ne
                                  else (arr !! (i-1)))
                         inds flags
    in  segmScanInc myop ne flags adj_arr


-----------------------------------
--- Filter and Segmented Filter ---
-----------------------------------
filter2 :: (a->Bool) -> [a] -> ([a], [Int])
filter2 cond arr =
    let n   = length arr
        cs  = map cond arr
        tfs = map (\f -> if f then 1
                              else 0) cs
        isT = scanInc (+) 0 tfs
        i   = last isT

        ffs = map (\f->if f then 0
                            else 1) cs
        isF = (map (+ i) . scanInc (+) 0) ffs

        inds= map (\(c,iT,iF) ->
                      if c then iT-1 else iF-1)
                  (zip3 cs isT isF)
        flags = write [0,i] [i,n-i] (replicate n 0)
    in  (permute inds arr, flags)

nestedSgmFilter2 :: (a->Bool) -> [[a]] -> [([a],[Int])]
nestedSgmFilter2 p = map (filter2 p) 

-----------------------------------------------------
---  Intuitive Semantics:                         ---
---   flatSgmFilter2 odd [2,0,2,0] [4,1,3,3]   ---
---        gives ([1,1,2,0],[1,4,3,3])            ---
---          [2,0,2,0] are the flags
---          [4,1,3,3] are the data
---        The first segment consists of elements ---
---          [4,1] and filtering with odd will    ---
---          break it into two segments, one of   ---
---          odd numbers, occuring first, and one ---
---          for even numbers. Hence the flag is  ---
---          modified to [1,1] and data is        ---
---          permuted to [1,4]!                   ---
---        The second segment consist of elements ---
---          [3,3], which are both odd, hence their--
---          flags and data remain as provided,   ---
---          i.e., [2,0] and [3,3], respectivelly.---
---
---        It follows the final result should be: ---
---          (flags,data): ([1,1,2,0], [1,4,3,3]) ---
---                                               ---
--- This is used by flatQuicksort!                ---
-----------------------------------------------------

flatSgmFilter2 :: (a->Bool) -> [Int] -> [a] -> ([Int],[a])
flatSgmFilter2 cond sizes arr = 
    let n   = length arr
        cs  = map cond arr
        ssi = segmScanInc (+) 0 sizes sizes
        si  = scanInc (+) 0 sizes 
      
        tfs = map (\f -> if f then 1 
                              else 0) cs
        isT = segmScanInc (+) 0 sizes tfs
        lis = map (\s->(isT !! (s-1))) si

        ffs = map (\f->if f then 0 
                            else 1) cs
        tmp = segmScanInc (+) 0 sizes ffs
        isF = zipWith (\ li ff -> li + ff) lis tmp

        diff= zipWith (-) si ssi
        inds= DL.zipWith4 (\ c iT iF d -> 
                            if c then iT-1+d else iF-1+d
                       ) cs isT isF diff

        iotas = segmScanInc (+) 0 sizes (replicate n 1)
        sizes'= DL.zipWith4 (\ f i s li -> 
                              if f > 0 
                              then if li > 0 then li else f
                              else if (i-1) == li
                                   then s - li else 0
                         ) sizes iotas ssi lis
--        inds' = trace (show inds ++ " cs:" ++ show cs ++ " isT: " ++ show isT ++ " isF" ++ show isF ++ "difs: " ++ show diff ++ " lis: "++show lis) inds
    in  (sizes', permute inds arr)


-----------------------------------
--- Computing the prime numbers ---
---   up to and including N     ---
-----------------------------------

primes :: Int -> [Int]
primes n =
    let a = map (\i -> if i==0 || i==1
                       then 0
                       else 1 ) [0..n]
        sqrtN = floor (sqrt (fromIntegral n))
--        sqrtN1  = trace (show sqrtN) sqrtN
    in  primesHelp 2 n sqrtN a
    where
        primesHelp :: Int -> Int -> Int -> [Int] -> [Int]
        primesHelp i n sqrtN a =
            if i > sqrtN
            then a
            else let m    = (n `div` i) - 1
                     inds = map (\k -> (k+2)*i) (iota m)
                     vals = replicate m 0
                     a'   = write inds vals a
--                     a''  = trace (show inds ++ " " ++ show vals ++ " " ++ show a') a'
                 in  primesHelp (i+1) n sqrtN a'

primesOpt :: Int -> [Int]
primesOpt n =
    if n <= 2 then [2]
    else let sqrtN = floor (sqrt (fromIntegral n))
             sqrt_primes = primesOpt sqrtN
             composite = map (\ p -> let m = (n `div` p)
                                     in  map (\ j -> j * p ) (drop 2 (iota (m+1)))
                             ) sqrt_primes
             not_primes = reduce (++) [] composite
             zero_array = replicate (length not_primes) False
             prime_flags= write not_primes zero_array (replicate (n+1) True)
             (primes,_)= ( unzip . filter (\(i,f) -> f) ) (zip (iota (n+1)) prime_flags)
             primes' = trace (show n ++ " " ++ show sqrt_primes ++ " " ++
                              show composite ++ " "++ show primes) primes
          in drop 2 primes


-----------------------------------------------------
--- ASSIGNMENT 1, Task 3, the flat-parallel version--
---    of prime-number computation (sieve).       ---
---    See L2-Flatenning lecture sliedes for help,---
---    for example, slide 66 out of 74.           ---
-----------------------------------------------------

-- Should result in a list containing all the prime numbers up to n!
-- The current dummy implementation just knows 2 and 3 as prime numbers,
-- please give a valid solution instead!
primesFlat :: Int -> [Int]
primesFlat n =
    if n <= 2 then [2]
    else [2,3] 
    

-----------------
--- QuickSort ---
-----------------

nestedQuicksort :: (Ord a, Show a) => [a] -> [a]
nestedQuicksort arr =
    if (length arr) <= 1 then arr
    else let i :: Int
             i = unsafePerformIO (getStdRandom (randomR (0, (length arr) - 1)))
             a = arr !! i
             s1 = filter (\x -> (x <  a)) arr
             s2 = filter (\x -> (x >= a)) arr
             rs = map nestedQuicksort [s1, s2]
             -- rs'= trace (show rs) rs
         in  (rs !! 0) ++ (rs !! 1)

-------------------------------------------------------
--- The flat version receives as input a
---    (semantically) two-dimensional array,
---    whose representation is a 1D array of
---    1. flags: [3,0,0,2,0,5,0,0,0,0,1] and
---    2. data : [9,8,7,5,6,4,3,2,1,0,9], meaning:
---    the first  row has 3 elements: [9,8,7]
---    the second row has 2 elements: [5,6]
---    the third  row has 5 elements: [4,3,2,1,0] and
---    the fourth row has 1 element : [9]
-------------------------------------------------------

flatQuicksort :: (Ord a, Show a, Num a) => a -> [Int] -> [a] -> [a]
flatQuicksort ne sizes arr =
    if reduce (&&) True $ map (\s->(s<2)) sizes then arr
    else let si = scanInc (+) 0 sizes
             r_inds= map (\(l,u,s)-> if s<1 then ne
                                else let i :: Int
                                         i = unsafePerformIO (getStdRandom (randomR (l,u-1)))
                                     in  (arr !! i)
                         )  (zip3 (0:si) si sizes)


             rands = segmScanInc (+) ne sizes r_inds

             (sizes', arr_rands) = flatSgmFilter2 (\(r,x) -> (x <  r)) sizes (zip rands arr)
             (_,arr') = unzip arr_rands

--             arr'' = trace (show arr ++ " " ++ show sizes ++ " " ++ " " ++ show rands ++ " " ++ show arr' ++ " " ++ show sizes') arr'

         in  flatQuicksort ne sizes' arr'


-----------------------------------------------------
--- ASSIGNMENT 2: implement sparse matrix-vector  ---
---    TASK I.3.  multiplication with nested      ---
---               parallelism \& test it!         ---
---               Matrix:                         ---
---              [ 2.0, -1.0,  0.0, 0.0]          ---
---              [-1.0,  2.0, -1.0, 0.0]          ---
---              [ 0.0, -1.0,  2.0,-1.0]          ---
---              [ 0.0,  0.0, -1.0, 2.0]          ---
---                                               ---
---              IS REPRESENTED AS a list of lists---
---                in which the outer list has the---
---                same number of elements as the ---
---                number of rows of the matrix.  ---
---                However, each row is represented--
---                as a sparse list that pairs up ---
---                each non-zero element with its ---
---                column index, as below:        ---
---                                               ---
---              [ [(0,2.0),  (1,-1.0)],          ---
---                [(0,-1.0), (1, 2.0), (2,-1.0)],---
---                [(1,-1.0), (2, 2.0), (3,-1.0)],---
---                [(2,-1.0), (3, 2.0)]           ---
---              ]                                ---
---               The vector is full and matches  ---
---               the matrix number of columns,   ---
---               e.g., x = [2.0, 1.0, 0.0, 3.0]  ---
---                     (transposed)              ---
---    ALSO LOOK WHERE IT IS FUNCTION IS CALLED   ---
-----------------------------------------------------

nestSparseMatVctMult :: [[(Int,Double)]] -> [Double] -> [Double]
nestSparseMatVctMult mat x =
    map (\row -> let (inds,vals) = unzip row
                     yi = 0.0 -- implement who yi should be!
                 in  yi
            ----------------------------------------------
            --- Pseudocode:                            ---
            ---   yi := 0;                             ---
            ---   for(j = 0; j < length inds; j++)     ---
            ---       yi := yi + vals[j] * x[ inds[j] ]---
            ----------------------------------------------
        ) mat


-----------------------------------------------------------
--- ASSIGNMENT 2: implement sparse matrix-vector        ---
---    TASK I.3   multiplication with flat parallelism! ---
---               Same matrix as before has a flat      ---
---               representation: flag vector (flags) & ---
---                               data vector (mat  )   ---
---    ALSO LOOK WHERE IT IS FUNCTION IS CALLED   ---
-----------------------------------------------------------

flatSparseMatVctMult :: [Int] -> [(Int,Double)] -> [Double] -> [Double]
flatSparseMatVctMult flags mat x =
    let tot_num_elems = length flags
        vct_len       = length x
        --------------------------------
        --- Implementation Here      ---
        --- Figure it out!           ---
        --------------------------------
    in  x

----------------------------------------
--- MAIN                             ---
----------------------------------------
-- runhaskell LHexamples.hs
main :: IO()
main = do args <- getArgs
          let inp  = if null args then [8,14,0,12,4,10,6,2] else read (head args)
              sizes= [2,0,1,4,0,0,0,1]
              pinp = permute [7, 6, 5, 4, 3, 2, 1, 0] inp
              vals :: [Int]
              vals = [33,33,33,33]
              inds = [0,2,4,6]
              winp = write inds vals inp
              matrix_nest = [ [(0,2.0),  (1,-1.0)],
                              [(0,-1.0), (1, 2.0), (2,-1.0)],
                              [(1,-1.0), (2, 2.0), (3,-1.0)],
                              [(2,-1.0), (3, 2.0)]
                            ]
              matrix_flag = [1,       0,        1,        0,        0,        1,        0,        0,        1,         0      ]
              matrix_flat = [(0,2.0), (1,-1.0), (0,-1.0), (1, 2.0), (2,-1.0), (1,-1.0), (2, 2.0), (3,-1.0), (2,-1.0), (3, 2.0)]
              x_vector    = [2.0, 1.0, 0.0, 3.0]

          putStrLn ("Input list: "++show inp)
          putStrLn ("Input flags:"++show sizes)
          putStrLn ("SegmScanIncl: "++ show (segmScanInc (+) 0 sizes inp))
          putStrLn ("SegmScanExcl: "++ show (segmScanExc (+) 0 sizes inp))
          putStrLn ("Permuted list: "++show pinp)
          putStrLn ("Written list: " ++show winp)
          putStrLn (" filter2Odd(a/2):"++show ( filter2 (\x->odd (x `div` 2)) inp))
          putStrLn ("SegmFilterOdd(a/2):"++show (flatSgmFilter2 (\x->odd (x `div` 2)) sizes inp))
          putStrLn ("Primes 32: " ++ show (primes 32))
          putStrLn ("PrimesOpt  49: " ++ show (primesOpt 49))
          putStrLn ("PrimesOpt  9: " ++ show (primesOpt 9))
          putStrLn ("PrimesFlat 49: " ++ show (primesFlat 49))
          putStrLn ("PrimesFlat 9: " ++ show (primesFlat 9))
          putStrLn ("NestQuicksort inp: " ++ show (nestedQuicksort inp))
          putStrLn ("FlatQuicksort inp: " ++ show (flatQuicksort 0 (8:(replicate 7 0)) inp))

          putStrLn ("Nested SparseMatrixMult: " ++ show (nestSparseMatVctMult matrix_nest x_vector))
          putStrLn ("Flat   SparseMatrixMult: " ++ show (flatSparseMatVctMult matrix_flag matrix_flat x_vector))
