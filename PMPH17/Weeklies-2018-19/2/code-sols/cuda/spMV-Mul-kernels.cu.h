#ifndef SP_MV_MUL_KERS
#define SP_MV_MUL_KERS

__global__ void
replicate0(int tot_size, int* flags_d) {
    const unsigned int gid = blockIdx.x*blockDim.x + threadIdx.x;
    if(gid < tot_size) {
        flags_d[gid] = 0; 
    }
}

__global__ void
mkFlags(int mat_rows, int* mat_shp_sc_d, int* flags_d) {
    const unsigned int gid = blockIdx.x*blockDim.x + threadIdx.x;
    if(gid < mat_rows) {
        if (gid == 0)
            flags_d[ 0 ] = 1;
        else
            flags_d[ mat_shp_sc_d[gid-1] ] = 1; 
    }
}

__global__ void 
mult_pairs(int* mat_inds, float* mat_vals, float* vct, int tot_size, float* tmp_pairs) {
    const unsigned int gid = blockIdx.x*blockDim.x + threadIdx.x;
    if(gid < tot_size) {
        tmp_pairs[gid] = mat_vals[gid] * vct[ mat_inds[gid] ];
    }
}

__global__ void
select_last_in_sgm(int mat_rows, int* mat_shp_sc_d, float* tmp_scan, float* res_vct_d) {
    const unsigned int gid = blockIdx.x*blockDim.x + threadIdx.x;
    if (gid < mat_rows) {
        res_vct_d[ gid ] = tmp_scan[ mat_shp_sc_d[gid] - 1 ];
    }
}

#endif
