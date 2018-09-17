#ifndef SCAN_KERS
#define SCAN_KERS

#include <cuda_runtime.h>

template<class T>
class TupleInt {
  public:
    T x; int y;
    __device__ __host__ inline TupleInt() {
        x = 0; y = 0;
    }
    __device__ __host__ inline TupleInt(const T& a, const int& b) {
        x = a; y = b;
    }
    __device__ __host__ inline TupleInt(const TupleInt<T>& i2) { 
        x = i2.x; y = i2.y; 
    }
    volatile __device__ __host__ inline TupleInt<T>& operator=(const TupleInt<T>& i2) volatile {
        x = i2.x; y = i2.y;
        return *this;
    }
};

template<class T>
class SgmOp {
  public:
    typedef typename T::ElmTp Tel; 
    typedef TupleInt<Tel> ElmTp;
    static __device__ __host__ inline ElmTp identity() { return TupleInt<Tel>(); }
    static __device__ __host__ inline ElmTp apply( volatile ElmTp& t1
                                                 , volatile ElmTp& t2 ) {
        const int fr = t1.y | t2.y;
        const Tel el = (t2.y != 0) ? t2.x : T::apply(t1.x, t2.x);
        return TupleInt<Tel>(el, fr);
    }
};


template<class T>
class Add {
  public:
    typedef T ElmTp;
    static __device__ __host__ inline T identity()                    { return (T)0;    }
    static __device__ __host__ inline T apply(const T t1, const T t2) { return t1 + t2; }
};

template<class T>
class Mul {
  public:
    typedef T ElmTp;
    static __device__ inline T identity()                      { return (T)1;    }
    static __device__ inline T apply(const T& t1, const T& t2) { return t1 * t2; }
};

/***************************************/
/*** Scan Inclusive Helpers & Kernel ***/
/***************************************/
template<class OP>
__device__ inline
typename OP::ElmTp 
scanIncWarp( volatile typename OP::ElmTp* ptr, const unsigned int idx ) {
    typedef typename OP::ElmTp T;
    const unsigned int lane = idx & 31;

    // no synchronization needed inside a WARP,
    //   i.e., SIMD execution
    if (lane >= 1)  ptr[idx] = OP::apply(ptr[idx-1],  ptr[idx]); 
    if (lane >= 2)  ptr[idx] = OP::apply(ptr[idx-2],  ptr[idx]);
    if (lane >= 4)  ptr[idx] = OP::apply(ptr[idx-4],  ptr[idx]);
    if (lane >= 8)  ptr[idx] = OP::apply(ptr[idx-8],  ptr[idx]);
    if (lane >= 16) ptr[idx] = OP::apply(ptr[idx-16], ptr[idx]);

    return const_cast<T&>(ptr[idx]);
}

template<class OP>
__device__ inline
typename OP::ElmTp 
scanIncBlock(volatile typename OP::ElmTp* ptr
            , const unsigned int idx) {
    typedef typename OP::ElmTp T;
    const unsigned int lane   = idx &  31;
    const unsigned int warpid = idx >> 5;

    T val = scanIncWarp<OP>(ptr,idx);
    __syncthreads();

    // place the end-of-warp results in
    //   the first warp. This works because
    //   warp size = 32, and 
    //   max block size = 32^2 = 1024
    if (lane == 31) { ptr[warpid] = const_cast<T&>(ptr[idx]); } 
    __syncthreads();

    //
    if (warpid == 0) scanIncWarp<OP>(ptr, idx);
    __syncthreads();

    if (warpid > 0) {
        val = OP::apply(ptr[warpid-1], val);
    }

    return val;
}

template<class OP>
__global__ void 
scanIncKernel( typename OP::ElmTp* d_in
             , typename OP::ElmTp* d_out
             , unsigned int d_size ) {
    typedef typename OP::ElmTp T;
    extern __shared__ char sh_mem1[];
    volatile T* sh_memT = (volatile T*)sh_mem1;
    const unsigned int tid = threadIdx.x;
    const unsigned int gid = blockIdx.x*blockDim.x + tid;
    T el    = (gid < d_size) ? d_in[gid] : OP::identity();
    sh_memT[tid] = el;
    __syncthreads();
    T res   = scanIncBlock<OP>(sh_memT, tid);
    if (gid < d_size) d_out [gid] = res; 
}


/***********************************************************/
/*** Kernels to copy/distribute the end of block results ***/
/***********************************************************/

template<class T>
__global__ void 
copyEndOfBlockKernel(T* d_in, T* d_out, unsigned int d_out_size) {
    const unsigned int gid = blockIdx.x*blockDim.x + threadIdx.x;
    
    if(gid < d_out_size)
        d_out[gid] = d_in[ blockDim.x*(gid+1) - 1];
}

template<class OP>
__global__ void 
distributeEndBlock( typename OP::ElmTp* d_in
                  , typename OP::ElmTp* d_out
                  , unsigned int d_size) {
    const unsigned int gid = blockIdx.x*blockDim.x + threadIdx.x;
    
    if(gid < d_size && blockIdx.x > 0)
        d_out[gid] = OP::apply(d_in[blockIdx.x-1],d_out[gid]);
}

template<class T>
__global__ void 
shiftRightByOne(T* d_in, T* d_out, T ne, unsigned int d_size) {
    const unsigned int gid = blockIdx.x*blockDim.x + threadIdx.x;
    
    if      (gid == 0)      d_out[gid] = ne;
    else if (gid < d_size)  d_out[gid] = d_in[gid-1];
}

////////////////////////////////////////
////////////////////////////////////////

template<class T>
__global__ void 
sgmShiftRightByOne(T* d_in, int*flags, T* d_out, T ne, unsigned int d_size) {
    const unsigned int gid = blockIdx.x*blockDim.x + threadIdx.x;
    if(gid < d_size) {
        if      (flags[gid]!=0)  d_out[gid] = ne;
        else                     d_out[gid] = d_in[gid-1];
    }
}

template<class T>
__global__ void
zipFlags(TupleInt<T>* res, int* d_flg, T* d_in, unsigned int d_size) {
    const unsigned int gid = blockIdx.x*blockDim.x + threadIdx.x;
    if(gid < d_size) {
        T   el   = d_in [gid];
        int flg  = d_flg[gid];
        res[gid] = TupleInt<T>(el, flg);
    }
}

template<class T>
__global__ void
remFlags(T* res, TupleInt<T>* d_in, unsigned int d_size) {
    const unsigned int gid = blockIdx.x*blockDim.x + threadIdx.x;
    if(gid < d_size) {
        res[gid] = d_in[gid].x;
    }
}

#endif //SCAN_KERS

