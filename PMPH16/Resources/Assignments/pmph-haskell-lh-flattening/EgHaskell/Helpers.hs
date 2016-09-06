module Helpers
       ( split
       , reduce
       , iota
       , write
       , permute
       )
       where

import Data.Ord
import Data.List

-------------------------------
-- helper to simulate (x++y) --
-------------------------------
split :: [a] -> ([a], [a])
split []  = ([],[] )
split [x] = ([],[x])
split xs  = let mid = (length xs) `div` 2
            in (take mid xs, drop mid xs)

reduce :: (a -> a -> a) -> a -> [a] -> a
reduce = foldl

iota :: Int -> [Int]
iota n = [0..n-1]

write :: [Int] -> [a] -> [a] -> [a]
write [] _ a        = a
write _ [] a        = a
write (i:is) (v:vs) a =
    if i >= length a
    then write is vs a
    else
        let prol = take (i)   a
            epil = drop (i+1) a
        in  write is vs (prol ++ [v] ++ epil)

permute :: [Int] -> [a] -> [a]
permute inds vals =
    let ivs = zip inds vals
        sivs = sortBy (comparing fst) ivs
    in  map snd sivs
