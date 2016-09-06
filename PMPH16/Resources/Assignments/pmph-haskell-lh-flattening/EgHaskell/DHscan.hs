import System.Environment -- access to arguments etc.

import Data.Bits -- bit twiddling with shift and xor

import Helpers

-- log2
log2 1 = 0
log2 n = 1 + log2 (n `div` 2) -- the "`" makes div into infix notation

-- [.] = liftLst
liftLst :: a -> [a]
liftLst x = [x]

--------------------------------------------
--- Distributable Homomorphism Skeletons
--------------------------------------------

swap :: (a->a->a,a->a->a) -> [a] -> Int -> Int -> a
swap (plus, mul) x d l =
        let ind = l `xor` (1 `shiftL` (d-1))
        in if(l < ind) then plus (x !! l  ) (x !! ind)
           else             mul  (x !! ind) (x !! l  )

distr_homop :: (a->a->a) -> (a->a->a) -> [a] -> [a] -> [a]
distr_homop pl mul u v = (zipWith pl u v) ++ (zipWith mul u v)

distr_hom :: (a->a->a) -> (a->a->a) -> [a] -> [a]
distr_hom pl mul []  = []
distr_hom pl mul [c] = [c]
distr_hom pl mul z   =  let (x, y) = split z
                        in  distr_homop pl mul (distr_hom pl mul x) (distr_hom pl mul y)


distr_homMR :: (a->a->a) -> (a->a->a) -> [a] -> [a]
distr_homMR pl mul = (reduce (distr_homop pl mul) []) . (map liftLst)

distr_hyper :: (a->a->a) -> (a->a->a) -> Int -> [a] -> [a]
distr_hyper plus mul k xs =
        if (k == 1) then map (swap (plus, mul) xs 1) [0..length xs-1]
        else let new_xs = distr_hyper plus mul (k-1) xs
             in  map (swap (plus, mul) new_xs k) [0..length xs-1]

----------------------------------------------------------------
--- Implementing the ``distributable scan'' corresponds to
--- implementing the `plus' and `multiply' operations, i.e.,
--- filling in the blanks
----------------------------------------------------------------

distrib_scan :: Num a => (a->a->a) -> [a] -> [a]
distrib_scan op =
        let scan_pl  (s1, r1) (s2, r2) = (s1,    r1+r2)
            scan_mul (s1, r1) (s2, r2) = (s2+r1, r1+r2)
        in  (map (\(x,y) -> x)) . (distr_hom scan_pl scan_mul) . (map (\x -> (x,x)))

hyper_scan :: Num a => (a->a->a) -> [a] -> [a]
hyper_scan op xs =
        let scan_pl  (s1, r1) (s2, r2) = (s1,    r1+r2)
            scan_mul (s1, r1) (s2, r2) = (s2+r1, r1+r2)
            scan_hlp = (map (\(x,y) -> x)) . (distr_hyper scan_pl scan_mul k) . (map (\x -> (x,x)))
            k = log2 (length xs)
        in  scan_hlp xs

----------------------------------------
---    Other scan implementations    ---
----------------------------------------
-- leftwards
lw_scan :: (a -> a -> a) -> [a] -> [a]
lw_scan op []       = []
lw_scan op [c]      = [c]
lw_scan op (c:xs)   = c : map (op c) (lw_scan op xs)

-- typical list-homomorphims scan
hom_scan :: (a -> a -> a) -> [a] -> [a]
hom_scan op []      = []
hom_scan op [c]     = [c]
hom_scan op z       =
        let (x,y) = split z
            homop u v = u ++ map (op (last u)) v
        in  (hom_scan op x) `homop` (hom_scan op y)


----------------------------------------
--- MAIN                             ---
----------------------------------------
-- runhaskell DHscan.hs

main :: IO()
main = do args <- getArgs
          let inp = if null args then [1,2,3,4,5,6,7,8] else read (head args)
              xxx = lw_scan      (+) inp
              ppp = hom_scan     (+) inp
              ttt = distrib_scan (+) inp
              www = hyper_scan   (+) inp
          putStrLn ("Leftwards    Scan: " ++ show xxx)
          putStrLn ("Homomorphism Scan: " ++ show ppp)
          putStrLn ("DistrHomomor Scan: " ++ show ttt)
          putStrLn ("Hyper        Scan: " ++ show www)
