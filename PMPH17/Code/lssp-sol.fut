-- Parallel longest satisfying segment
-- ==
-- compiled input { 
--    [1, -2, -2, 0, 0, 0, 0, 0, 3, 4, -6, 1]
-- }  
-- output { 
--    9
-- } 

type int = i32

let max(x: int, y: int): int = if x > y then x else y

let pred1 (k : int, x: int) : bool =
  if      k == 1 then x == 0 -- zeroes
  else if k == 2 then true   -- sorted  
  else if k == 3 then true   -- same
  else true                  -- default

let pred2(k : int, x: int, y: int): bool =
  if      k == 1 then (x == 0) && (y == 0) -- zeroes
  else if k == 2 then x <= y               -- sorted  
  else if k == 3 then x == y               -- same
  else true                                -- default

let redOp (pind : int) 
          (x: (int,int,int,int,int,int)) 
          (y: (int,int,int,int,int,int)) 
        : (int,int,int,int,int,int) =
  let (lssx, lisx, lcsx, tlx, firstx, lastx) = x
  let (lssy, lisy, lcsy, tly, firsty, lasty) = y

  let connect = pred2(pind, lastx, firsty)
  let newlss = if connect then max(lcsx + lisy, max(lssx, lssy))
                          else max(lssx, lssy)
  let newlis = if lisx == tlx && connect then lisx + lisy else lisx
  let newlcs = if lcsy == tly && connect then lcsy + lcsx else lcsy
  let first = if tlx == 0 then firsty else firstx
  let last  = if tly == 0 then lastx else lasty in

  (newlss, newlis, newlcs, tlx+tly, first, last)

let mapOp (pind : int) (x: int) : (int,int,int,int,int,int) =
  let xmatch = if pred1(pind, x) then 1 else 0 in
  (xmatch, xmatch, xmatch, 1, x, x)

let lssp (pind : int) (xs : []int) : int =
  let (x,_,_,_,_,_) =
    reduce (redOp pind) (0,0,0,0,0,0) (map (mapOp pind) xs) in
  x

let main [n] (xso: *[n]int): int =
  -- let xs = write ([n/2, n/2+1, n/2+2]) ([0,0,0]) xso
  lssp 2 xso -- computes sorted, you may also try with zeroes (1) and same (3)
