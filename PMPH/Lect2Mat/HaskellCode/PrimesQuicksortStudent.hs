import System.Environment -- access to arguments etc.
import Data.List
import Data.Ord
import Data.Bits
import Debug.Trace

import System.IO.Unsafe  -- be careful!                                         
import System.Random

------------------------------------------------
-- 1. Install Haskell, then cabal:
--    $ sudo apt-get install cabal-install
-- 2. Install random package:
---   $ cabal install random
------------------------------------------------
reduce :: (a->a->a) -> a -> [a] -> a
reduce = foldl

---------------------------------------
--- Helpers: iota, write, permute   ---
--- sequential ugly implementations ---
---------------------------------------

iota :: Int -> [Int]
iota n = [0..n-1]

write :: [Int] -> [a] -> [a] -> [a]
write [] _ a        = a
write _ [] a        = a
write (i:is) (v:vs) a =
    let prol = take (i)   a
        epil = drop (i+1) a
    in  write is vs (prol ++ [v] ++ epil) 
    
permute :: [Int] -> [a] -> [a]
permute inds vals = 
    let ivs = zip inds vals
        sivs = sortBy sortGT ivs
    in  map (\(x1,x2)->x2) sivs
    where
        sortGT (i1, v1) (i2, v2)
            | i1 < i2  = LT
            | i1 >= i2 = GT

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
parFilter :: (a->Bool) -> [a] -> ([a], [Int])
parFilter cond arr = 
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

primesFlat :: Int -> [Int]
primesFlat n = 
    if n <= 2 then [2] 
    else let sqrtN = floor (sqrt (fromIntegral n))
             sqrt_primes = primesFlat sqrtN
             num_primes  = length sqrt_primes
             mult_lens   = map (\p -> (n `div` p) - 1) sqrt_primes
             mult_scan   = scanExc (+) 0 mult_lens 
             mult_tot_len= (last mult_scan) + (last mult_lens)
             
             flags = write mult_scan (replicate num_primes 1) (replicate mult_tot_len 0)
             ps    = write mult_scan sqrt_primes              (replicate mult_tot_len 0)
             prime_vals= segmScanInc (+) 0 flags ps
             prime_inds= segmScanInc (+) 0 flags (replicate mult_tot_len 1)
             not_primes= zipWith (\i v->(i+1)*v) prime_inds prime_vals

             zero_array = replicate (length not_primes) False 
             prime_flags= write not_primes zero_array (replicate (n+1) True)
             (primes,_)= ( unzip . filter (\(i,f) -> f) ) (zip (iota (n+1)) prime_flags)
             primes' = trace (show n ++ " " ++ show sqrt_primes ++ " " ++ 
                              show not_primes ++ " "++ show primes) primes
          in drop 2 primes


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

             (sizes', arr_rands) = segmSpecialFilter (\(r,x) -> (x <  r)) sizes (zip rands arr)
             (_,arr') = unzip arr_rands

--             arr'' = trace (show arr ++ " " ++ show sizes ++ " " ++ " " ++ show rands ++ " " ++ show arr' ++ " " ++ show sizes') arr'

         in  flatQuicksort ne sizes' arr'

-----------------------------------------------------
--- ASSIGNMENT 1: implement this function (below) ---
---    TASK 3.    (the current implementation is  ---
---                bogus, i.e., just to compile)  ---  
---                                               ---
---  Intuitive Semantics:                         ---
---   segmSpecialFilter odd [2,0,2,0] [4,1,3,3]   ---
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
--- IF YOU GET IT RIGHT flatQuicksort should WORK!---
-----------------------------------------------------
segmSpecialFilter :: (a->Bool) -> [Int] -> [a] -> ([Int],[a])
segmSpecialFilter cond sizes arr = 
    let bools = map cond arr
        flags = map (\s -> if s /= 0 then 1 else 0) sizes
        ts = map (\b -> if b then 1 else 0) bools
        fs = map (\t -> 1-t) ts
        isTs = segmScanInc (+) 0 flags ts
        sizesExtended = segmScanInc (+) 0 flags sizes
        offsets = zipWith (\sE offsInclFirst -> offsInclFirst - sE)
                  sizesExtended
                  $ tail $ scanl (+) 0 sizes
        numTrues = map (\(sE, offs) -> isTs !! (sE+offs-1))
                   $ zip sizesExtended offsets
        isFs = zipWith (+) numTrues $ segmScanInc (+) 0 flags fs
        inds = map (\(b,isT,isF,offs) ->
                        if b then (isT-1+offs) else (isF-1+offs))
               $ zip4 bools isTs isFs offsets
        newArr = permute inds arr
        
        tsAggr = segmScanInc (+) 0 flags ts
        fsAggr = segmScanInc (+) 0 flags fs
        shiftedFlags = tail flags ++ [head flags]
        tsAggrStripped = zipWith (*) shiftedFlags tsAggr
        fsAggrStripped = zipWith (*) shiftedFlags fsAggr
        indsForTs = zipWith (*) shiftedFlags $ map (+1) offsets
        indsForFs = zipWith3 (\indT t f -> if f > 0 then indT + t else 0) indsForTs tsAggrStripped fsAggrStripped
        newSizesTs = write indsForTs tsAggrStripped $ replicate (length arr + 1) 0 -- "junk" is written to the 0'th element.
        newSizes = tail $ write indsForFs fsAggrStripped newSizesTs -- "junk" is discarded (tail).
    in
      (newSizes, newArr)

-----------------------------------------------------
--- ASSIGNMENT 1: implement sparse matrix-vector  ---
---    TASK 4a.   multiplication with nested      ---
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
    map (\row ->
         ----------------------------------------------
         --- Pseudocode:                            ---
         ---   yi := 0;                             ---
         ---   for(j = 0; j < length inds; j++)     ---
         ---       yi := yi + vals[j] * x[ inds[j] ]---
         ----------------------------------------------
         let prods = map (\(i,v) -> v * (x !! i)) row
         in reduce (+) 0.0 prods
        ) mat


-----------------------------------------------------------
--- ASSIGNMENT 1: implement sparse matrix-vector        ---
---    TASK 4b.   multiplication with flat parallelism! ---
---               Same matrix as before has a flat      ---
---               representation: flag vector (flags) & ---
---                               data vector (mat  )   ---
---    ALSO LOOK WHERE IT IS FUNCTION IS CALLED   ---
-----------------------------------------------------------

flatSparseMatVctMult :: [Int] -> [(Int,Double)] -> [Double] -> [Double]
flatSparseMatVctMult flags mat x = 
--         --------------------------------
--         --- Implementation Here      ---
--         --- Figure it out!           ---
--         --------------------------------
    let prods = map (\(i,v) -> v * (x !! i)) mat
        reduced = segmScanInc (+) 0 flags prods
        shiftedFlags = tail flags ++ [head flags]
        accFlags = scanInc (+) 0 shiftedFlags
        inds = zipWith (*) shiftedFlags accFlags
    in tail $ write inds reduced $ replicate (last accFlags + 1) 0

----------------------------------------
--- MAIN                             ---
----------------------------------------
-- ghc -O2 -o test PrimesQuicksort.hs
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
          putStrLn (" ParFilterOdd(a/2):"++show ( parFilter (\x->odd (x `div` 2)) inp))
          putStrLn ("SegmFilterOdd(a/2):"++show (segmSpecialFilter (\x->odd (x `div` 2)) sizes inp))
          putStrLn ("Primes 32: " ++ show (primes 32))
          putStrLn ("PrimesOpt  49: " ++ show (primesOpt 49))
          putStrLn ("PrimesOpt  9: " ++ show (primesOpt 9))
          putStrLn ("PrimesFlat 49: " ++ show (primesFlat 49))
          putStrLn ("PrimesFlat 9: " ++ show (primesFlat 9))
          putStrLn ("NestQuicksort inp: " ++ show (nestedQuicksort inp))
          putStrLn ("FlatQuicksort inp: " ++ show (flatQuicksort 0 (8:(replicate 7 0)) inp))

          putStrLn ("Nested SparseMatrixMult: " ++ show (nestSparseMatVctMult matrix_nest x_vector))
          putStrLn ("Flat   SparseMatrixMult: " ++ show (flatSparseMatVctMult matrix_flag matrix_flat x_vector))



