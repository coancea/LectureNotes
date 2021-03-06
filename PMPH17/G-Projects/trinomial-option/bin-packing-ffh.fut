-- parallel binpacking
-- ==
-- compiled input @ data/binpack.in
-- output @ data/binpack.out


import "/futlib/math"
import "/futlib/array"

let sgmPrefSum [n] (flags: [n]i32) (data: [n]i32) : [n]i32 =
    #2 (unzip (scan (\(x_flag,x) (y_flag,y) ->
                     let flag = x_flag | y_flag in
                     if y_flag != 0
                     then (flag, y)
                     else (flag, x + y))
                    (0, 0)
                    (zip flags data)))

let bin_packing_ffh [q] 
            (w: i32)              -- the bin size
            (all_perm  : *[q]i32) -- the current permutation of elems [0..q-1]
            (all_data0 :  [q]i32) -- the data (weights/lengths) corresponding to elements in perm
          : ([]i32, [q]i32)       -- resulting shape + resulting permutation

=
    let all_data = scatter (replicate q 0) all_perm all_data0
    let len   = q
    let cur_shape = replicate 0 0
    let goOn  = true
    let count = 0
    let (_,all_perm,_,cur_shape, _,_) = 
    loop ((len,all_perm,all_data,cur_shape, goOn,count)) while goOn && count < 100 do
        let (data, _) = split (len) all_data
        let (perm, _) = split (len) all_perm

        -- 1. initial attempt by first fit heuristic
        let scan_data = scan (+) 0 data
        let ini_sgms  = map (/w) scan_data

        let num_sgms  = (last ini_sgms) + 1  -- OK
        let flags = map (\i -> if i == 0 then 1
                               else if unsafe ini_sgms[i-1] == ini_sgms[i]
                                    then 0 else 1
                        ) (iota len)
        let ones  = replicate len 1
        let tmp   = sgmPrefSum flags ones
        let (inds1,inds2,vals) = unzip (
            map (\ i -> if (i == len-1) || (unsafe flags[i+1] == 1) 
                             -- end of segment
                             then (i+1-tmp[i], ini_sgms[i], tmp[i])
                             else (-1,-1,0)
                ) (iota len)
          )
        let flags = scatter (replicate len 0) inds1 vals
        let shapes= scatter (replicate num_sgms 0) inds2 vals

        -- 2. try validate: whatever does not fit move it as a first segment
        let scan_data = sgmPrefSum flags data
        let ini_sgms  = scan (+) 0 (map (\x -> if x > 0 then 1 else 0) flags) 
                        -- map (/w) scan_data
        let moves =
            map (\ i ->
                    let sgm_len = flags[i]
                    in
                    if sgm_len > 0
                    then if unsafe scan_data[i+sgm_len-1] > w
                         then 1 -- this start of segment should be moved
                         else 0
                    else 0
                ) (iota len)

        let num_moves = reduce (+) 0 moves
        in
        -- if true
        -- then (num_moves, flags, all_data, concat shapes cur_shape, false)
        if num_moves == 0
        then (num_moves, all_perm, all_data, concat shapes cur_shape, false, count)
        else -- reorder perm, data, and shape arrays
            let scan_moves = scan (+) 0 moves
            let (inds_s, lens, inds_v) = unzip (
                map (\ i -> let offset = scan_moves[i]
                            let (ind_s, ll) = 
                                if i > 0 && flags[i] == 0 && unsafe moves[i-1] > 0
                                 -- new start of segment
                                then (unsafe ini_sgms[i-1]-1, unsafe flags[i-1]-1)
                                else (-1, 0)
                            let ind_v = if moves[i] == 0 then (num_moves-offset+i) else offset-1  -- ???
                            in  (ind_s, ll, ind_v)
                    ) (iota len)
              )
            let shapes'  = scatter shapes inds_s lens
            let cur_shape= concat shapes' cur_shape

            -- THE TWO COMMENTED LINES BELOW GENERATE A BUG!!!
            -- actually Opencl complains about overlapping buffers
            -- I did the 4-line workaround instead.
--          let all_perm = scatter (copy all_perm) inds_v perm
--          let all_data = scatter (copy all_data) inds_v data
            let perm' = scatter (replicate len 0) inds_v perm
            let data' = scatter (replicate len 0) inds_v data
            let all_perm[0:len] = perm'
            let all_data[0:len] = data'

            -- in  (num_moves, all_perm, inds_v, cur_shape, false)
            in  (num_moves, all_perm, all_data, cur_shape, true, count+1)
      in  (cur_shape, all_perm)


let main [arr_len] (arr : [arr_len]i32) : ([]i32, []i32, i32, i32, f32, bool) = 
  -- let arr = [10, 33, 2, 67, 101, 8, 10, 80, 70, 60, 9, 10, 2, 104, 55, 5, 6, 25, 122, 105, 31, 196] -- 22
  -- let arr_len = (shape arr)[0]

  let w   = let m = reduce_comm i32.max 0 arr
            in  if m < 256 then 2*m
                else m

  let (binpack_shape, perm) = 
        bin_packing_ffh w (iota arr_len) arr

  let num_sgms = length binpack_shape
  let (ind,waste,ok) = (0,0,true)

  let (_, waste, ok) =
  loop ((ind, waste, ok)) for k < num_sgms do
    let sum_sgm =
    loop sum_sgm=0 for i < binpack_shape[k] do
        sum_sgm + arr[perm[ind+i]]
    in ( ind+binpack_shape[k], waste + w - sum_sgm, ok && (sum_sgm <= w) )

  let check_perm = replicate arr_len 0
  let (_,ok) = 
  loop ((check_perm,ok)) for i < arr_len do
      let perm_ind = perm[i]
      let perm_elm = check_perm[perm_ind]
      let ok = if perm_elm == 0 then ok else false
      let check_perm[perm_ind] = 1
      in  (check_perm,ok)

  in  (perm, binpack_shape, reduce (+) 0 binpack_shape, w, f32(waste)/f32(num_sgms*w), ok)

-- futhark-dataset --i32-bounds=1:255 -g [10000]i32 | futhark -i bin-packing-ffh.fut
