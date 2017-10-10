-- Generate dataset of specified size for test of Trinomial Option Pricing
-- ==
-- compiled input @ data/dataset.in
-- output @ data/dataset.out

import "/futlib/math"

------------------------------------------
-- Floating point numbers precision
------------------------------------------
--default(f64)
--import "header64"
--let i2r    (i : i32 ) : real = f64 i

default(f32)
import "header32"
let i2r    (i : i32 ) : real = f32 i

-----------------
-- Entry point
-----------------
let main (options_n : i32) :    ( [options_n]real
                                , [options_n]real
                                , [options_n]i32
                                , [options_n]real
                                , [options_n]real)  = 
  let k = 0.0002
  let optionData = map (\i ->
      ( (i2r i)*k
      , 9.0
      , 108
      , 0.1000
      , 0.0100
      ) 
  ) (iota options_n) 
  in unzip optionData

