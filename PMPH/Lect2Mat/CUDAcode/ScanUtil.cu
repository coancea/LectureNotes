#include <cuda_runtime.h>

template<class T>
class Add {
  public:
    static __device__ T identity()        { return (T)0;    }
    static __device__ T apply(T t1, T t2) { return t1 + t2; }
};


/****************************************/
/*** Scan Exclusive Helpers & Kernel  ***/
/****************************************/

template<class OP, class T>
__device__ T 
scanExcWarp(volatile T* ptr, const unsigned int idx) {
    const unsigned int lane = idx & 31;

    if (lane >= 1)  ptr[idx] = OP::apply(ptr[idx-1],  ptr[idx]);
    if (lane >= 2)  ptr[idx] = OP::apply(ptr[idx-2],  ptr[idx]);
    if (lane >= 4)  ptr[idx] = OP::apply(ptr[idx-4],  ptr[idx]);
    if (lane >= 8)  ptr[idx] = OP::apply(ptr[idx-8],  ptr[idx]);
    if (lane >= 16) ptr[idx] = OP::apply(ptr[idx-16], ptr[idx]);

    return (lane > 0) ? ptr[idx-1] : OP::identity();
}

template<class OP, class T>
__device__ T 
scanExcBlock(volatile T* ptr, const unsigned int idx) {
    const unsigned int lane   = idx &  31;
    const unsigned int warpid = idx >> 5;

    T val = scanExcWarp<OP,T>(ptr,idx);
    __syncthreads();

    if (lane == 31) ptr[warpid] = ptr[idx]; 
    __syncthreads();

    if (warpid == 0) scanExcWarp<OP,T>(ptr, idx);
    __syncthreads();

    if (warpid > 0) {
        val = OP::apply(ptr[warpid-1], val);
    }

    return val;
}

__global__ void 
scanExcKernel(float* d_in, float* d_out, unsigned int d_size) {
    extern __shared__ float sh_mem[];
    const unsigned int tid = threadIdx.x;
    const unsigned int gid = blockIdx.x*blockDim.x + tid;
    float el    = (gid < d_size) ? d_in[gid] : 0.0;
    sh_mem[tid] = el;
    __syncthreads();
    float res   = scanExcBlock < Add<float>, float >(sh_mem, tid);
    if (gid < d_size) d_out [gid] = res; 
}

/***************************************/
/*** Scan Inclusive Helpers & Kernel ***/
/***************************************/
template<class OP, class T>
__device__ T 
scanIncWarp(volatile T* ptr, const unsigned int idx) {
    const unsigned int lane = idx & 31;

    if (lane >= 1)  ptr[idx] = OP::apply(ptr[idx-1],  ptr[idx]);
    if (lane >= 2)  ptr[idx] = OP::apply(ptr[idx-2],  ptr[idx]);
    if (lane >= 4)  ptr[idx] = OP::apply(ptr[idx-4],  ptr[idx]);
    if (lane >= 8)  ptr[idx] = OP::apply(ptr[idx-8],  ptr[idx]);
    if (lane >= 16) ptr[idx] = OP::apply(ptr[idx-16], ptr[idx]);

    return ptr[idx];
}

template<class OP, class T>
__device__ T 
scanIncBlock(volatile T* ptr, const unsigned int idx) {
    const unsigned int lane   = idx &  31;
    const unsigned int warpid = idx >> 5;

    T val = scanIncWarp<OP,T>(ptr,idx);
    __syncthreads();

    if (lane == 31) ptr[warpid] = ptr[idx]; 
    __syncthreads();

    if (warpid == 0) scanIncWarp<OP,T>(ptr, idx);
    __syncthreads();

    if (warpid > 0) {
        val = OP::apply(ptr[warpid-1], val);
    }

    return val;
}

__global__ void 
scanIncKernel(float* d_in, float* d_out, unsigned int d_size) {
    extern __shared__ float sh_mem[];
    const unsigned int tid = threadIdx.x;
    const unsigned int gid = blockIdx.x*blockDim.x + tid;
    float el    = (gid < d_size) ? d_in[gid] : 0.0;
    sh_mem[tid] = el;
    __syncthreads();
    float res   = scanIncBlock < Add<float>, float >(sh_mem, tid);
    if (gid < d_size) d_out [gid] = res; 
}

__global__ void 
scanIncKernelAddInt(int* d_in, int* d_out, unsigned int d_size) {
    extern __shared__ int sh_mem1[];
    const unsigned int tid = threadIdx.x;
    const unsigned int gid = blockIdx.x*blockDim.x + tid;
    int el    = (gid < d_size) ? d_in[gid] : 0.0;
    sh_mem1[tid] = el;
    __syncthreads();
    int res   = scanIncBlock < Add<int>, int >(sh_mem1, tid);
    if (gid < d_size) d_out [gid] = res; 
}


/***********************************************************/
/*** Kernels to copy/distribute the end of block results ***/
/***********************************************************/

__global__ void 
copyEndOfBlockKernel(float* d_in, float* d_out, unsigned int d_out_size) {
    const unsigned int gid = blockIdx.x*blockDim.x + threadIdx.x;
    
    if(gid < d_out_size)
        d_out[gid] = d_in[ blockDim.x*(gid+1) - 1];
}

__global__ void 
distributeEndBlock(float* d_in, float* d_out, unsigned int d_size) {
    const unsigned int gid = blockIdx.x*blockDim.x + threadIdx.x;
    
    if(gid < d_size && blockIdx.x > 0)
        d_out[gid] += d_in[blockIdx.x-1];
}

__global__ void 
shiftRightByOne(float* d_in, float* d_out, float ne, unsigned int d_size) {
    const unsigned int gid = blockIdx.x*blockDim.x + threadIdx.x;
    
    if      (gid == 0)      d_out[gid] = ne;
    else if (gid < d_size)  d_out[gid] = d_in[gid-1];
}


/*************************************************/
/*** Segmented Inclusive Scan Helpers & Kernel ***/
/*************************************************/
template<class OP, class T, class F>
__device__ T 
sgmScanIncWarp(volatile T* ptr, volatile F* flg, const unsigned int idx) {
    const unsigned int lane = idx & 31;

    if (lane >= 1)  {
        ptr[idx] = (flg[idx] != 0) ? ptr[idx] : OP::apply(ptr[idx-1],  ptr[idx]);
        flg[idx] = flg[idx-1] | flg[idx];
    }
    if (lane >= 2)  {
        ptr[idx] = (flg[idx] != 0) ? ptr[idx] : OP::apply(ptr[idx-2],  ptr[idx]);
        flg[idx] = flg[idx-2] | flg[idx];
    }
    if (lane >= 4)  {
        ptr[idx] = (flg[idx] != 0) ? ptr[idx] : OP::apply(ptr[idx-4],  ptr[idx]);
        flg[idx] = flg[idx-4] | flg[idx];
    }
    if (lane >= 8)  {
        ptr[idx] = (flg[idx] != 0) ? ptr[idx] : OP::apply(ptr[idx-8],  ptr[idx]);
        flg[idx] = flg[idx-8] | flg[idx];
    }
    if (lane >= 16)  {
        ptr[idx] = (flg[idx] != 0) ? ptr[idx] : OP::apply(ptr[idx-16],  ptr[idx]);
        flg[idx] = flg[idx-16] | flg[idx];
    }

    return ptr[idx];
}

template<class OP, class T, class F>
__device__ T 
sgmScanIncBlock(volatile T* ptr, volatile F* flg, const unsigned int idx) {
    const unsigned int lane   = idx &  31;
    const unsigned int warpid = idx >> 5;
    const unsigned int warplst= (warpid<<5) + 31;

    // 1a: record whether this warp begins with an ``open'' segment.
    bool warp_is_open = (flg[(warpid << 5)] == 0);
    __syncthreads();

    // 1b: intra-warp segmented scan for each warp
    T val = sgmScanIncWarp<OP,T>(ptr,flg,idx);

    // 2a: the last value is the correct partial result
    T warp_total = ptr[warplst];
    
    // 2b: warp_flag is the OR-reduction of the flags 
    //     in a warp, and is computed indirectly from
    //     the mindex in hd[]
    bool warp_flag = flg[warplst]!=0 || !warp_is_open;
    bool will_accum= warp_is_open && (flg[idx] == 0);

    __syncthreads();

    // 2c: the last thread in a warp writes partial results
    if (lane == 31) {
        ptr[warpid] = warp_total; //ptr[idx]; 
        flg[warpid] = warp_flag;
    }
    __syncthreads();

    if (warpid == 0) sgmScanIncWarp<OP,T>(ptr, flg, idx);
    __syncthreads();

    if (warpid > 0 && will_accum) {
        val = OP::apply(ptr[warpid-1], val);
    }
    return val;
}


__global__ void 
sgmScanIncKernel(float* d_in, int* flags, float* d_out, 
                              int* f_rec, float* d_rec, unsigned int d_size) {
    extern __shared__ float sh_mem[];
    volatile float* vals_sh = sh_mem;
    volatile int*   flag_sh = (int*) (sh_mem + blockDim.x);
    //__shared__ float sh_mem[256];
    const unsigned int tid = threadIdx.x;
    const unsigned int gid = blockIdx.x*blockDim.x + tid;
    float el;
    int   fl;    
    if (gid < d_size) { el = d_in[gid]; fl = flags[gid]; }
    else              { el = 0.0;       fl = 0;          }
    vals_sh[tid] = el; flag_sh[tid] = fl;
    __syncthreads();
    float res   = sgmScanIncBlock < Add<float>, float >(vals_sh, flag_sh, tid);
    if (gid < d_size) d_out [gid] = res; 

    // set the flags and data for the recursive step!
    if(tid == 0)  { f_rec[blockIdx.x] = 0; }
    __syncthreads();
    if(fl  >  0)  { f_rec[blockIdx.x] = 1; }
    if(tid == (blockDim.x - 1)) { d_rec[blockIdx.x] = res; }
}


__global__ void 
sgmDistributeEndBlock(float* d_rec_in, float* d_out, int* f_inds, unsigned int d_size) {
    const unsigned int gid = blockIdx.x*blockDim.x + threadIdx.x;
    
    if(gid < d_size && blockIdx.x > 0) {
        if(f_inds[gid] == 0)
            d_out[gid] += d_rec_in[blockIdx.x-1];
    }
}


