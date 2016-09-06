import System.Environment -- access to arguments etc.

import Helpers

hoPred :: (a -> Bool) -> ((a,a) -> Bool) -> [a] -> Bool
hoPred p1 p2 []    = True
hoPred p1 p2 [x]   = p1 x
hoPred p1 p2 [x,y] = p2 (x,y)
hoPred p1 p2 (x : y : zs) = (p2 (x,y)) && (hoPred p1 p2 (y : zs))

zeroes :: Integral a =>  [a] -> Bool
zeroes = hoPred (\x -> x == 0) (\(x,y) -> x == 0 && y == 0)

same :: Eq a => [a] -> Bool
same =  hoPred (\x -> True) (\(x,y) -> x == y)

sorted :: Ord a => [a] -> Bool
sorted = hoPred (\x -> True) (\(x,y) -> x <= y)

allTrue :: Integral a =>  [a] -> Bool
-- allTrue = hoPred (\x -> (x > 0)) (\(x,y) -> (x+y > 0))
allTrue = hoPred (\x -> True) (\(x,y) -> True)

-------------------------
-- maximal segment sum --
-------------------------
emss :: Integral a => [a] -> (a,a,a,a)
emss xs =
    let mssop (mssx, misx, mcsx, tsx)
              (mssy, misy, mcsy, tsy) =
          ( mssx `max` mssy `max`  (mcsx + misy),
            misx `max` (tsx+misy),
            mcsy `max` (mcsx+tsy),
            tsx + tsy )
        f x = (x `max` 0, x `max` 0, x `max` 0, x)
    in ((reduce mssop (0,0,0,0)) . (map f)) xs

mss :: Integral a => [a] -> a
mss xs =  let first (x,_,_,_) = x
          in  first (emss xs)

--------------------------------------
-- Longest Satisfying Segment       --
-- ASSIGNMENT 1: fill in the blanks --
--       See lecture notes          --
--------------------------------------

elss :: Integral a => ([a] -> Bool) -> [a] -> (a,a,a,a,a,a,Bool)
elss p xs =
    let -- lssop :: Integral a => (a,a,a,a,a,a,Bool) -> (a,a,a,a,a,a,Bool) -> (a,a,a,a,a,a,Bool)
        lssop (lssx, lisx, lcsx, tlx, firstx, lastx, okx)
              (lssy, lisy, lcsy, tly, firsty, lasty, oky) =
          (newlss, newlis, newlcs, tlx+tly, firstx, lasty, newok)
                where
                  connect = False -- ... fill in the blanks (rewrite this line)
                  newlss  = 0     -- ... fill in the blanks (rewrite this line)
                  newlis  = 0     -- ... fill in the blanks (rewrite this line)
                  newlcs  = 0     -- ... fill in the blanks (rewrite this line)
                  newok   = False -- ... fill in the blanks (rewrite this line)
        f x = (xmatch, xmatch, xmatch, 1, x, x, p [x])
                  where xmatch = if (p [x]) then 1 else 0
    in ((reduce lssop (0,0,0,0,0,0,True)) . (map f)) xs

lss :: Integral a => ([a] -> Bool) -> [a] -> a
lss p xs = let first (x,_,_,_,_,_,_) = x
           in  first (elss p xs)


----------------------------------------
--- MAIN                             ---
----------------------------------------
-- runhaskell LongestSatSgm.hs
main :: IO()
main = do args <- getArgs
          let inp :: [Int]
              inp = if null args then [1,-2,3,4,-1,5,-6,1] else read (head args)
              resZeroes = lss zeroes  inp
              resSame   = lss same    inp
              resSorted = lss sorted  inp
              resMss    = mss         inp
          putStrLn ("Result zeroes: " ++ show resZeroes)
          putStrLn ("Result same  : " ++ show resSame  )
          putStrLn ("Result sorted: " ++ show resSorted)
          putStrLn ("Result mss   : " ++ show resMss)
