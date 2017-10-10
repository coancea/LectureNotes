import "/futlib/math"

type real = f32

let i2r    (i : i32 ) : real = f32 i
let r_exp  (a : real) : real = f32.exp  a
let r_sqrt (a : real) : real = f32.sqrt a
let r_abs  (a : real) : real = f32.abs  a
let r_log  (a : real) : real = f32.log  a
let r_max  (a : real, b : real) : real = f32.max a b
