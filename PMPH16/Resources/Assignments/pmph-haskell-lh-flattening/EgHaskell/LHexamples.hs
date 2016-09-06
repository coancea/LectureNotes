import System.Environment -- access to arguments etc.

import Helpers

--------------------
-- len            --
--------------------
lenLH :: [a] -> Int
lenLH []  = 0
lenLH [_] = 1
lenLH z = let (x,y) = split z
          in  (lenLH x) + (lenLH y)

lenMR :: [a] -> Int
lenMR = ( reduce (+) 0 ) . ( map (\_ -> 1) )

--------------------
-- sum            --
--------------------
sumLH :: [Int] -> Int
sumLH []  = 0
sumLH [x] = x
sumLH z = let (x,y) = split z
          in  (sumLH x) + (sumLH y)

sumMR :: [Int] -> Int
sumMR = (reduce (+) 0) . (map id)

--------------------
-- flatten        --
--------------------
flattenLH :: [[a]] -> [a]
flattenLH []  = []
flattenLH [x] = x
flattenLH z = let (x,y) = split z
          in  (flattenLH x) ++ (flattenLH y)

flattenMR :: [[a]] -> [a]
flattenMR = (reduce (++) []) . (map id)

-----------
-- all p --
-----------
alln :: (a -> Bool) -> [a] -> Bool
alln _ []     = True
alln p [x]    = p x
alln p xs     =
        let (x, y) = split xs
        in  (alln p x) && (alln p y)

allMR :: (a -> Bool) -> [a] -> Bool
allMR p = (reduce (&&) True) . (map p)

-------------
-- maxList --
-------------
maxListLH :: [Int] -> Int
maxListLH []  = -1000000
maxListLH [x] = x
maxListLH z = let (x, y) = split z
              in  (maxListLH x) `max` (maxListLH y)

maxListMR :: [Int] -> Int
maxListMR = (reduce (max) (-1000000)) . (map id)

----------------------------------------
--- MAIN                             ---
----------------------------------------
-- runhaskell LHexamples.hs
main :: IO()
main = do args <- getArgs
          let inp = if null args then [8,14,0,12,4,10,6,2] else read (head args)
              p x = x `mod` 2 == 0
              (resall, resallmr)     = (alln p inp, allMR p inp)

              lofl = replicate 4 inp
              (flat_lst1, flat_lst2) = (flattenLH lofl, flattenMR lofl)

          putStrLn ("Sum of input list: "++show (sumLH inp)++" "++show (sumMR inp))
          putStrLn ("Len of input list: "++show (lenLH inp)++" "++show (lenMR inp))
          putStrLn ("For all p : " ++ show resall ++ " " ++ show resallmr)
          putStrLn ("For Flatten: " ++ show flat_lst1 ++ " " ++ show flat_lst2)
          putStrLn ("For maxList: " ++ show (maxListLH inp) ++ " " ++ show (maxListMR inp))
