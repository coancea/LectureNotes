-- Primes: Naive Version
-- ==
-- compiled input @ data/primes-small.in
-- output @ data/primes-small.out


fun i32_filter (flags: [k]int) (ns: *[k]int): *[]int =
  let is0 = scan (+) 0 flags
  let filter_size = is0[k - 1]
  let is1 = map (fn (i: int, f: int): int  => i * f) (zip is0 flags)
  let is2 = map (fn (i: int): int  => i - 1) is1
  in write is2 ns (replicate filter_size 0)

fun primesHelp (n : int) (sq : int) (a : *[]int) : []int =
  loop(a) = for 2 <= i < (sq+1) do 
        let m    = (n / i) - 1
        let inds = map (fn k => (k+2)*i) (iota m)
        let vals = replicate m 0
        let res  = write inds vals a
        in  res
  in a

fun main (n : int) : []int = 
  let a = map (fn i => if i==0 || i==1 then 0 else 1) (iota (n+1))
  let sq= int( sqrt32( f32(n) ) )
  let fl= primesHelp n sq a
  in  i32_filter fl (iota (n+1))

