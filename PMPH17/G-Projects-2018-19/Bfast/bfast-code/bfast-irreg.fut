-- BFAST-irregular: version handling obscured observations (e.g., clouds)
-- ==
-- compiled input @ data/sahara.in.gz
-- output @ data/sahara.out.gz

let logplus (x: f32) : f32 =
  if x > (f32.exp 1)
  then f32.log x else 1

-- | builds the X matrices; first result dimensions of size 2*k+2
let mkX (k2p2: i32) (N: i32) (f: f32) : [k2p2][N]f32 =
  [ replicate N 1           -- first   row
  , map r32 (1 ... N)       -- second  row
  ] ++
  ( map (\ i ->           -- sin/cos rows
          map (\j -> let i' = r32 (i / 2)
                     let j' = r32 j
                     let angle = 2 * f32.pi * i' * j' / f
                     in  if i % 2 == 0 then f32.sin angle else f32.cos angle
              ) (1 ... N)
        ) (2 ... k2p2-1)
  )

---------------------------------------------------
-- Matrix inversion by Gauss Jordan              --
---------------------------------------------------

  let gauss_jordan [nm] (n:i32) (A: *[nm]f32): [nm]f32 =
    let m = nm / n in
    loop A for i < n do
      let v1 = A[i]

      let A' = map (\ind -> let (k, j) = (ind / m, ind % m)
                            let x = unsafe (A[j] / v1) in
                                if k < n-1  -- Ap case
                                then unsafe ( A[(k+1)*m+j] - A[(k+1)*m+i] * x )
                                else x      -- irow case
                   ) (iota (n*m))
      in  scatter A (iota (n*m)) A'

  let mat_inv [n] (A: [n][n]f32): [n][n]f32 =
    let m = 2*n
    -- Pad the matrix with the identity matrix.
    let Ap = map (\ind -> let (i, j) = (ind / m, ind % m)
                          in  if j < n then unsafe ( A[i,j] )
                                       else if j == n+i
                                            then 1.0
                                            else 0.0
                 ) (iota (n*m))
    let Ap' = unflatten n m (gauss_jordan n Ap)

    -- Drop the identity matrix at the front.
    in Ap'[0:n,n:n * 2]

---------------------------------------------------
-- Dot-Product, Matrix-Vector and Matrix-Matrix  --
-- multiplication in dense and filtered form     --
---------------------------------------------------

let dotprod [n] (xs: [n]f32) (ys: [n]f32): f32 =
    reduce (+) 0.0 (map2 (*) xs ys)

let matvecmul_row [n][m] (xss: [n][m]f32) (ys: [m]f32) =
    map (dotprod ys) xss

let dotprod_filt [n] (flgs: [n]bool) (xs: [n]f32) (ys: [n]f32) : f32 =
    f32.sum (map3 (\flg x y -> if flg then x*y else 0) flgs xs ys)

let matvecmul_row_filt [n][m] (flgs: [m]bool) (xss: [n][m]f32) (ys: [m]f32) =
    map (dotprod_filt flgs ys) xss

let matsqr_filt [m] [n] (flgs: [n]bool) (xss: [m][n]f32) : *[m][m]f32 =
    map (\xs -> map (\ys -> dotprod_filt flgs xs ys) xss) xss

---------------------------------------------------

-- | The core of the alg: the computation for a time series
--   for one pixel.
let bfast [N] (f: f32) (k: i32) (n: i32)
              (hfrac: f32) (lam: f32)
              (y: [N]f32) :
              []f32 =   -- result has lengths N-n
  -- it's ok, don't panick: whatever is invariant to the
  -- outer map is gonna be hoisted out!
  let X = mkX (2*k+2) N f

  let m    = n
  let flgs = map (\v -> !(f32.isnan v)) y

  let flgsh = unsafe (flgs[:m])
  let yh    = unsafe (y[:m])
  let Xh    = unsafe (X[:,:m])

  -- line 2, beta-hat computation
  -- fit linear regression model:
  let Xsqr = matsqr_filt flgsh Xh               -- Xsqr  shape: [2k+2][2k+2]
  let Xinv = mat_inv    Xsqr                    -- Xinv  shape: [2k+2][2k+2]
  let beta0= matvecmul_row_filt flgsh Xh yh     -- beta0 shape:       [2k+2]
  let beta = matvecmul_row Xinv beta0           -- beta  shape:       [2k+2]

  -- Here starts the monitoring/testing set

  -- line 3, y-hat computation: get predictions and residuals.
  -- the entries in y_pred corresponding to indices `i` such
  -- that `flgs[i]==false` are invalid.
  let y_pred = matvecmul_row (transpose X) beta -- [N]

  -- line 4: error
  let y_error_all = map3 (\flg ye yep -> if flg then (ye-yep) else 0) flgs y y_pred -- [N]

  let num_nans = map (\flg -> if flg then 0 else 1) flgs |> scan (+) 0
  let ns = n - unsafe num_nans[n-1]
  let Ns = N - unsafe num_nans[N-1]
  let ms = m - unsafe num_nans[m-1]

  let (_,y_error,val_inds)  =
      unzip3 <| filter (\(flg,_,_) -> flg) (zip3 flgs y_error_all (iota N))
  
  -- moving sums:
  let h = t32 ( (r32 m) * hfrac )
  let MO_fst = reduce (+) 0 ( unsafe (y_error[ns-h+1 : ns+1]) )
  let MO_ini = map (\j ->
                      if j == 0 then MO_fst
                      else  unsafe (-y_error[ns-h+j] + y_error[ns+j])
                   ) (0 ... Ns-ns-1)
  let MO = scan (+) 0 MO_ini

  -- line 5: sigma
  let tmp = map (\ a -> a*a ) (unsafe (y_error[:ms]))

  let sigma = reduce (+) 0 tmp
  let sigma = f32.sqrt ( sigma / (r32 (ns-2*k-2)) )

  let MO = map (\mo -> mo / (sigma * (f32.sqrt (r32 ns))) ) MO

  let val_inds' = drop ns val_inds |> map (\x -> x-n)
  let full_MO = scatter (replicate (N-n) f32.nan) val_inds' MO
  
  -- line 10: BOUND computation (will be hoisted)
  let BOUND = map (\q -> let t   = n+1+q
                         let tmp = logplus ((r32 t) / (r32 n))
                         in  lam * (f32.sqrt tmp)
                  ) (0 ... N-n-1)

  let breaks = map2 (\m b -> if (f32.isnan m) || (f32.isnan b)
                             then 0.0 else (f32.abs m) - b
                    ) full_MO BOUND
  in  breaks

-- | entry point
entry main [m][N] (k: i32) (n: i32) (freq: f32)
                  (hfrac: f32) (lam: f32)
                  (images : [m][N]f32) :
                  ([m][]f32) =
  let res = map (bfast freq k n hfrac lam) images
  in  res

