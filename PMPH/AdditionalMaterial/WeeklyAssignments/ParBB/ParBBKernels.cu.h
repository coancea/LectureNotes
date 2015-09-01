#ifndef PAR_BB_KERS
#define PAR_BB_KERS

#include <cuda_runtime.h>

template<class T>
class Add {
  public:
    typedef T BaseType;
    static __device__ __host__ inline T identity()                    { return (T)0;    }
    static __device__ __host__ inline T apply(const T t1, const T t2) { return t1 + t2; }
};

template<class T>
class Mul {
  public:
    typedef T BaseType;
    static __device__ inline T identity()                      { return (T)1;    }
    static __device__ inline T apply(const T& t1, const T& t2) { return t1 * t2; }
};

class MyInt4 {
  public:
    int x; int y; int z; int w;

    __device__ __host__ inline MyInt4() {
        x = 0; y = 0; z = 0; w = 0; 
    }
    __device__ __host__ inline MyInt4(const int& a, const int& b, const int& c, const int& d) {
        x = a; y = b; z = c; w = d; 
    }
    __device__ __host__ inline MyInt4(const MyInt4& i4) { 
        x = i4.x; y = i4.y; z = i4.z; w = i4.w; 
    }
    volatile __device__ __host__ inline MyInt4& operator=(const MyInt4& i4) volatile {
        x = i4.x; y = i4.y; z = i4.z; w = i4.w; 
        return *this;
    }
    __device__ __host__ inline int selInc(int i) {
        return (i == 0) ? ++x :
               (i == 1) ? ++y :
               (i == 2) ? ++z : ++w;
    }
    __device__ __host__ inline void zeroOut() {
        x = y = z = w = 0; 
    }
    __device__ __host__ inline void set(const volatile MyInt4& i4) {
        x = i4.x; y = i4.y; z = i4.z; w = i4.w; 
    }
    __host__ inline void selSub(int i, int val) {
        if     (i==0) { x -= val; }
        else if(i==1) { y -= val; }
        else if(i==2) { z -= val; }
        else          { w -= val; } 
    }
};

class MyInt8 {
  public:
    int a; int b; int c; int d;
    int x; int y; int z; int w;

    __device__ __host__ inline MyInt8() {
        a = b = c = d = x = y = z = w = 0; 
    }
    __device__ __host__ inline MyInt8(const int& v1, const int& v2, const int& v3, const int& v4,
                                      const int& v5, const int& v6, const int& v7, const int& v8) {
        a = v1; b = v2; c = v3; d = v4; x = v5; y = v6; z = v7; w = v8; 
    }
    __device__ __host__ inline MyInt8(const MyInt8& i8) { 
        a = i8.a; b = i8.b; c = i8.c; d = i8.d; x = i8.x; y = i8.y; z = i8.z; w = i8.w; 
    }
    volatile __device__ __host__ inline MyInt8& operator=(const MyInt8& i8) volatile {
        a = i8.a; b = i8.b; c = i8.c; d = i8.d; x = i8.x; y = i8.y; z = i8.z; w = i8.w; 
        return *this;
    }
    __device__ __host__ inline int selInc(int i) {
        return (i == 0) ? ++a : (i == 1) ? ++b : (i == 2) ? ++c : (i == 3) ? ++d :
               (i == 4) ? ++x : (i == 5) ? ++y : (i == 6) ? ++z : ++w;
    }
    __device__ __host__ inline void zeroOut() {
        a = b = c = d = x = y = z = w = 0; 
    }
    __device__ __host__ inline void set(const volatile MyInt8& i8) {
        a = i8.a; b = i8.b; c = i8.c; d = i8.d; x = i8.x; y = i8.y; z = i8.z; w = i8.w; 
    }
    __host__ inline void selSub(int i, int val) {
        if     (i==0) { a -= val; }
        else if(i==1) { b -= val; }
        else if(i==2) { c -= val; }
        else if(i==3) { d -= val; }
        else if(i==4) { x -= val; }
        else if(i==5) { y -= val; }
        else if(i==6) { z -= val; }
        else          { w -= val; } 
    }
};

class MsspOp {
  public:
    typedef MyInt4 BaseType;
    static __device__ inline MyInt4 identity() { return MyInt4(0,0,0,0); }  
    static __device__ inline MyInt4 apply(volatile MyInt4& t1, volatile MyInt4& t2) { 
        int mss = max(t1.x, max(t2.x,t1.z+t2.y));
        int mis = max(t1.y, t1.w+t2.y);
        int mcs = max(t2.z, t1.z+t2.w);
        int t   = t1.w + t2.w;
        return MyInt4(mss, mis, mcs, t); 
    }
};

class AddMyInt4 {
  public:
    static __device__ inline MyInt4 identity() { return MyInt4(0,0,0,0); }  
    static __device__ inline MyInt4 apply(volatile MyInt4& t1, volatile MyInt4& t2) { 
        return MyInt4(t1.x+t2.x, t1.y+t2.y, t1.z+t2.z, t1.w+t2.w); 
    }
};

class AddMyInt8 {
  public:
    static __device__ inline MyInt8 identity() { return MyInt8(0,0,0,0,0,0,0,0); }  
    static __device__ inline MyInt8 apply(volatile MyInt8& t1, volatile MyInt8& t2) { 
        return MyInt8(  t1.a+t2.a, t1.b+t2.b, t1.c+t2.c, t1.d+t2.d,
                        t1.x+t2.x, t1.y+t2.y, t1.z+t2.z, t1.w+t2.w); 
    }
};


//template<float rep>
class LessThan {
  public:
    typedef float InType;
    typedef int   OutType;
    static const float padelm = 0.99;
    static __host__ __device__ inline float identity() { return 0.0; }  
    static __host__ __device__ inline int   apply(volatile float t) {
        return (t < 0.5) ? 1 : 0;
    }
};    

class Mod4 {
  public:
    typedef int           InType;
    typedef int          OutType;
    typedef MyInt4       ExpType;
    typedef AddMyInt4 AddExpType;
    static const int cardinal = 4;
    static const int padelm   = 3;
    static __host__ __device__ inline int identity() { return 0; }
    static __host__ __device__ inline int   apply(volatile int t) {
        return t & 3;
    }
    static __device__ inline void incWithAccumDiff( MyInt4& acc,
                                                    int*    arrtmp, 
                                                    MyInt4* blk_beg, 
                                                    MyInt4* blk_prv, 
                                                    MyInt4* blk_end 
    ) {
        int beg_blk, tmp_diff = 0;

        beg_blk   = (blockIdx.x > 0) * blk_beg->x;
        acc.x    += tmp_diff + (threadIdx.x > 0) * (blk_prv->x - beg_blk);
        arrtmp[0] = blk_end->x - beg_blk;
        tmp_diff += arrtmp[0];

        beg_blk   = (blockIdx.x > 0) * blk_beg->y;
        acc.y    += tmp_diff + (threadIdx.x > 0) * (blk_prv->y - beg_blk);
        arrtmp[1] = blk_end->y - beg_blk;
        tmp_diff += arrtmp[1];

        beg_blk   = (blockIdx.x > 0) * blk_beg->z;
        acc.z    += tmp_diff + (threadIdx.x > 0) * (blk_prv->z - beg_blk);
        arrtmp[2] = blk_end->z - beg_blk;
        tmp_diff += arrtmp[2];        

        beg_blk   = (blockIdx.x > 0) * blk_beg->w;
        acc.w    += tmp_diff + (threadIdx.x > 0) * (blk_prv->w - beg_blk);
        arrtmp[3] = blk_end->w - beg_blk;
    }
};

class Mod8 {
  public:
    typedef int           InType;
    typedef int          OutType;
    typedef MyInt8       ExpType;
    typedef AddMyInt8 AddExpType;
    static const int cardinal = 8;
    static const int padelm   = 7;
    static __host__ __device__ inline int identity() { return 0; }
    static __host__ __device__ inline int   apply(volatile int t) {
        return t & 7;
    }
    static __device__ inline void incWithAccumDiff( MyInt8& acc,
                                                    int*    arrtmp, 
                                                    MyInt8* blk_beg, 
                                                    MyInt8* blk_prv, 
                                                    MyInt8* blk_end 
    ) {
        int beg_blk, tmp_diff = 0;

        beg_blk   = (blockIdx.x > 0) * blk_beg->a;
        acc.a    += tmp_diff + (threadIdx.x > 0) * (blk_prv->a - beg_blk);
        arrtmp[0] = blk_end->a - beg_blk;
        tmp_diff += arrtmp[0];

        beg_blk   = (blockIdx.x > 0) * blk_beg->b;
        acc.b    += tmp_diff + (threadIdx.x > 0) * (blk_prv->b - beg_blk);
        arrtmp[1] = blk_end->b - beg_blk;
        tmp_diff += arrtmp[1];

        beg_blk   = (blockIdx.x > 0) * blk_beg->c;
        acc.c    += tmp_diff + (threadIdx.x > 0) * (blk_prv->c - beg_blk);
        arrtmp[2] = blk_end->c - beg_blk;
        tmp_diff += arrtmp[2];        

        beg_blk   = (blockIdx.x > 0) * blk_beg->d;
        acc.d    += tmp_diff + (threadIdx.x > 0) * (blk_prv->d - beg_blk);
        arrtmp[3] = blk_end->d - beg_blk;
        tmp_diff += arrtmp[3];

        beg_blk   = (blockIdx.x > 0) * blk_beg->x;
        acc.x    += tmp_diff + (threadIdx.x > 0) * (blk_prv->x - beg_blk);
        arrtmp[4] = blk_end->x - beg_blk;
        tmp_diff += arrtmp[4];

        beg_blk   = (blockIdx.x > 0) * blk_beg->y;
        acc.y    += tmp_diff + (threadIdx.x > 0) * (blk_prv->y - beg_blk);
        arrtmp[5] = blk_end->y - beg_blk;
        tmp_diff += arrtmp[5];

        beg_blk   = (blockIdx.x > 0) * blk_beg->z;
        acc.z    += tmp_diff + (threadIdx.x > 0) * (blk_prv->z - beg_blk);
        arrtmp[6] = blk_end->z - beg_blk;
        tmp_diff += arrtmp[6];        

        beg_blk   = (blockIdx.x > 0) * blk_beg->w;
        acc.w    += tmp_diff + (threadIdx.x > 0) * (blk_prv->w - beg_blk);
        arrtmp[7] = blk_end->w - beg_blk;
    }
};



class AddInt4Opt {
  public:
    static __device__ inline int identity() { return 0; }  
    static __device__ inline int apply(volatile int& t1, volatile int& t2) {
        int t11 = t1, t22 = t2, t = 0, res = 0;
        #pragma unroll
        for(int i=0; i<4; i++, t+=8) {
            res += ((t11 & 127) + (t22 & 127)) << t;
            t11 = t11 >> 8; t22 = t22 >> 8;
        }
        return res;
    }
};

class Mod4Opt {
  public:
    typedef int           InType;
    typedef int          OutType;
    typedef MyInt4       ExpType;
    typedef AddInt4Opt   AddExpType;
    static const int cardinal = 4;
    static const int padelm   = 3;
    static __host__ __device__ inline int identity() { return 0; }
    static __host__ __device__ inline int   apply(volatile int t) {
        return t & 3;
    }
    static __host__ __device__ inline int expand1(const int i) {
        return 1 << (8*i);
    }
    static __host__ __device__ inline int extract(const int v, const int i) {
        return (v >> (8*i)) & 127;
    }
};


/***************************************/
/*** Scan Inclusive Helpers & Kernel ***/
/***************************************/
template<class OP, class T>
__device__ inline
T scanIncWarp( volatile T* ptr, const unsigned int idx ) {
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

template<class OP, class T>
__device__ inline
T scanIncBlock(volatile T* ptr, const unsigned int idx) {
    const unsigned int lane   = idx &  31;
    const unsigned int warpid = idx >> 5;

    T val = scanIncWarp<OP,T>(ptr,idx);
    __syncthreads();

    // place the end-of-warp results in
    //   the first warp. This works because
    //   warp size = 32, and 
    //   max block size = 32^2 = 1024
    if (lane == 31) { ptr[warpid] = const_cast<T&>(ptr[idx]); }
    __syncthreads();

    //
    if (warpid == 0) scanIncWarp<OP,T>(ptr, idx);
    __syncthreads();

    if (warpid > 0) {
        val = OP::apply(ptr[warpid-1], val);
    }

    return val;
}

template<class OP, class T>
__global__ void 
scanIncKernel(T* d_in, T* d_out, unsigned int d_size) {
    extern __shared__ char sh_mem1[];
    volatile T* sh_memT = (volatile T*)sh_mem1;
    const unsigned int tid = threadIdx.x;
    const unsigned int gid = blockIdx.x*blockDim.x + tid;
    T el    = (gid < d_size) ? d_in[gid] : OP::identity();
    sh_memT[tid] = el;
    __syncthreads();
    T res   = scanIncBlock < OP, T >(sh_memT, tid);
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

template<class OP, class T>
__global__ void 
distributeEndBlock(T* d_in, T* d_out, unsigned int d_size) {
    const unsigned int gid = blockIdx.x*blockDim.x + threadIdx.x;
    
    if(gid < d_size && blockIdx.x > 0)
        d_out[gid] = OP::apply(d_out[gid],d_in[blockIdx.x-1]);
}

template<class T>
__global__ void 
shiftRightByOne(T* d_in, T* d_out, T ne, unsigned int d_size) {
    const unsigned int gid = blockIdx.x*blockDim.x + threadIdx.x;
    
    if      (gid == 0)      d_out[gid] = ne;
    else if (gid < d_size)  d_out[gid] = d_in[gid-1];
}

/*************************************************/
/*************************************************/
/*** Segmented Inclusive Scan Helpers & Kernel ***/
/*************************************************/
/*************************************************/
template<class OP, class T, class F>
__device__ inline
T sgmScanIncWarp(volatile T* ptr, volatile F* flg, const unsigned int idx) {
    const unsigned int lane = idx & 31;

    // no synchronization needed inside a WARP,
    //   i.e., SIMD execution
    if (lane >= 1)  {
        if(flg[idx] == 0) { ptr[idx] = OP::apply(ptr[idx-1], ptr[idx]); }
        flg[idx] = flg[idx-1] | flg[idx];
    }
    if (lane >= 2)  {
        if(flg[idx] == 0) { ptr[idx] = OP::apply(ptr[idx-2], ptr[idx]); }
        flg[idx] = flg[idx-2] | flg[idx];
    }
    if (lane >= 4)  {
        if(flg[idx] == 0) { ptr[idx] = OP::apply(ptr[idx-4], ptr[idx]); }
        flg[idx] = flg[idx-4] | flg[idx];
    }
    if (lane >= 8)  {
        if(flg[idx] == 0) { ptr[idx] = OP::apply(ptr[idx-8], ptr[idx]); }
        flg[idx] = flg[idx-8] | flg[idx];
    }
    if (lane >= 16)  {
        if(flg[idx] == 0) { ptr[idx] = OP::apply(ptr[idx-16], ptr[idx]); }
        flg[idx] = flg[idx-16] | flg[idx];
    }

    return const_cast<T&>(ptr[idx]);
}

template<class OP, class T, class F>
__device__ inline
T sgmScanIncBlock(volatile T* ptr, volatile F* flg, const unsigned int idx) {
    const unsigned int lane   = idx &  31;
    const unsigned int warpid = idx >> 5;
    const unsigned int warplst= (warpid<<5) + 31;

    // 1a: record whether this warp begins with an ``open'' segment.
    bool warp_is_open = (flg[(warpid << 5)] == 0);
    __syncthreads();

    // 1b: intra-warp segmented scan for each warp
    T val = sgmScanIncWarp<OP,T>(ptr,flg,idx);

    // 2a: the last value is the correct partial result
    T warp_total = const_cast<T&>(ptr[warplst]);
    
    // 2b: warp_flag is the OR-reduction of the flags 
    //     in a warp, and is computed indirectly from
    //     the mindex in hd[]
    bool warp_flag = flg[warplst]!=0 || !warp_is_open;
    bool will_accum= warp_is_open && (flg[idx] == 0);

    __syncthreads();

    // 2c: the last thread in a warp writes partial results
    //     in the first warp. Note that all fit in the first
    //     warp because warp = 32 and max block size is 32^2
    if (lane == 31) {
        ptr[warpid] = warp_total; //ptr[idx]; 
        flg[warpid] = warp_flag;
    }
    __syncthreads();

    // 
    if (warpid == 0) sgmScanIncWarp<OP,T>(ptr, flg, idx);
    __syncthreads();

    if (warpid > 0 && will_accum) {
        val = OP::apply(ptr[warpid-1], val);
    }
    return val;
}

template<class OP, class T>
__global__ void 
sgmScanIncKernel(T* d_in, int* flags, T* d_out, 
                          int* f_rec, T* d_rec, unsigned int d_size) {
    extern __shared__ char sh_mem[];
    volatile T*   vals_sh = (volatile T*)sh_mem;
    volatile int* flag_sh = (int*) (vals_sh + blockDim.x);
    const unsigned int tid = threadIdx.x;
    const unsigned int gid = blockIdx.x*blockDim.x + tid;
    int fl;   
    if (gid < d_size) { vals_sh[tid] = d_in[gid];      fl = flags[gid]; }
    else              { vals_sh[tid] = OP::identity(); fl = 0;          }
    flag_sh[tid] = fl;
    __syncthreads();
    T res = sgmScanIncBlock <OP, T>(vals_sh, flag_sh, tid);
    if (gid < d_size) d_out [gid] = res; 

    // set the flags and data for the recursive step!
    if(tid == 0)  { f_rec[blockIdx.x] = 0; }
    __syncthreads();
    if(fl  >  0)  { f_rec[blockIdx.x] = 1; }
    if(tid == (blockDim.x - 1)) { d_rec[blockIdx.x] = res; }
}

template<class OP, class T>
__global__ void 
sgmDistributeEndBlock(T* d_rec_in, T* d_out, int* f_inds, unsigned int d_size) {
    const unsigned int gid = blockIdx.x*blockDim.x + threadIdx.x;
    
    if(gid < d_size && blockIdx.x > 0) {
        if(f_inds[gid] == 0)
            d_out[gid] = OP::apply(d_out[gid], d_rec_in[blockIdx.x-1]);
    }
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


__global__ void 
trivial_map(int* inp_d, MyInt4* inp_lift, int inp_size) {
    const unsigned int gid = blockIdx.x*blockDim.x + threadIdx.x;
    if(gid < inp_size) {
        int el = inp_d[gid];
        MyInt4 res(el,el,el,el);
        if(el < 0) { res.x = 0;  res.y = 0;  res.z = 0; }
        inp_lift[gid] = res;
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
write_lastsgm(float* tmp_scan, int* tmp_inds, int* flags_d, int tot_size, float* vct_d) {
    const unsigned int gid = blockIdx.x*blockDim.x + threadIdx.x;
    if(gid < tot_size) {
        if ( (gid == (tot_size-1)) || (flags_d[gid+1] != 0) )
            vct_d[tmp_inds[gid]-1] = tmp_scan[gid]; 
    }
}

//<OP,T><<< num_blocks, block_size >>>(inp_d, inp_lift, inp_size);


/********************/
/*** MAP   Kernel ***/
/********************/
template<class MapLambda>
__global__ void 
mapKernel(typename MapLambda::InType* d_in, typename MapLambda::OutType* d_out, unsigned int d_size) {
    const unsigned int gid = blockIdx.x*blockDim.x + threadIdx.x;
    if(gid < d_size) {
        d_out[gid] = MapLambda::apply(d_in[gid]);
    }
}

template<class MapLambda>
__global__ void 
mapChunkKernel( typename MapLambda::InType*  d_in, 
                int*                         d_out,
                int*                         d_out_chunk,
                const unsigned int           d_height,
                const unsigned int           d_width
) {
    unsigned int gid = blockIdx.x*blockDim.x + threadIdx.x, i;
    unsigned int acc = 0;
    if(gid < d_height) {
        for (i = 0; i < d_width; i++, gid += d_height) {
            int res    = MapLambda::apply(d_in[gid]);
            d_out[gid] = res;
            acc       += res; 
        }
        gid = blockIdx.x*blockDim.x + threadIdx.x;
        d_out_chunk[gid] = acc;
    }
}

template<class MapLambda>
__global__ void
mapVctKernel(   typename MapLambda::InType*  d_in, 
                typename MapLambda::OutType* d_out,
                typename MapLambda::ExpType* d_out_chunk,
                         const unsigned int  d_height,
                         const unsigned int  d_width
) {
    extern __shared__ char map_sh_mem[];
    volatile int* acc = ((volatile int*)map_sh_mem) + 
                        MapLambda::cardinal*threadIdx.x;
    unsigned int gid = blockIdx.x*blockDim.x + threadIdx.x;
    unsigned int i;

    if(gid < d_height) {
        for(i=0; i<MapLambda::cardinal; i++) 
            acc[i] = MapLambda::identity();

        for (i = 0; i < d_width; i++, gid += d_height) {
            typename MapLambda::OutType res = MapLambda::apply(d_in[gid]);
            d_out[gid] = res;
            acc[res]++;
        }
        gid = blockIdx.x*blockDim.x + threadIdx.x; // -= d_height * d_width;
        for(i=0; i<MapLambda::cardinal; i++)
            ((int*)(d_out_chunk+gid))[i] = acc[i];
    }
}


/********************/
/*** WRITE Kernel ***/
/********************/
template<class T>
__global__ void
writeKernel(T* d_in, int* perm, T* d_out, const unsigned int d_size) {
    const unsigned int gid = blockIdx.x*blockDim.x + threadIdx.x;
    if ( gid < d_size ) {
        int prev = (gid > 0) ? perm[gid-1] : 0;
        int curr = perm[gid];
        if(prev != curr) d_out[curr-1] = d_in[gid];
    }
}

template<class T>
__global__ void
writeChunkKernelDummy(   
        T* d_in, int* cond_res, int* perm_chunk, T* d_out, 
        const unsigned int d_height, const unsigned int d_width
) {
    unsigned int gid = blockIdx.x*blockDim.x + threadIdx.x, i;
    if(gid < d_height) {
        int acc = (gid > 0) ? perm_chunk[gid-1] : 0;
        for (i = 0; i < d_width; i++, gid+=d_height) {
            if(cond_res[gid] > 0) {
                d_out[acc] = d_in[gid];
                acc++;
            }
        }
    }
}

__device__ inline
int myHash(int ind) {
    return ind;
    //return ((ind >> 12) << 12) + ((ind & 4095) ^ 4095);
    //return ((ind & 255) << 4) + ((ind >> 8) & 15) + ((ind >> 12)<<12);
    //return ((ind & 511) << 3) + ((ind >> 9) & 7) + ((ind >> 12)<<12);
}


template<class T>
__global__ void
writeChunkKernel(   T* d_in, int* cond_res, int* perm_chunk, T* d_out, 
                    const unsigned int d_height, const unsigned int CHUNK
) {
    extern __shared__ char gen_sh_mem[];
    volatile int* ind_sh_mem = (volatile int*) gen_sh_mem;
    volatile T*   elm_sh_mem = (volatile T*  ) gen_sh_mem;

    int hind = blockIdx.x*blockDim.x;
    int acc_blk = (blockIdx.x  > 0) ? perm_chunk[hind - 1] : 0;
    int acc_tmp = (threadIdx.x > 0) ? perm_chunk[hind + threadIdx.x - 1] - acc_blk : 0;
    int acc0 = 0;

    unsigned int tmp_id = blockIdx.x*blockDim.x + threadIdx.y*CHUNK*d_height + threadIdx.x;
    for(int k = 0; k < CHUNK; k++,tmp_id+=d_height) { 
        if(cond_res[tmp_id] > 0) acc0++;
    }
    ind_sh_mem[threadIdx.x*blockDim.y+threadIdx.y] = acc0;
    __syncthreads();
    scanIncWarp<Add<int>,int>(ind_sh_mem, threadIdx.y*blockDim.x+threadIdx.x);
    __syncthreads();
    acc0 = (threadIdx.y > 0) ? ind_sh_mem[threadIdx.x*blockDim.y+threadIdx.y-1] : 0;
    __syncthreads();

    tmp_id = blockIdx.x*blockDim.x + threadIdx.y*CHUNK*d_height + threadIdx.x; 
    for(int k = 0; k < CHUNK; k++, tmp_id+=d_height) {
        if(cond_res[tmp_id] > 0) {
            elm_sh_mem[myHash(acc_tmp+acc0)] = d_in[tmp_id];
            acc0++;
        }
    }
    __syncthreads();
    // Finally, the write stage!
    int tot_len = perm_chunk[blockIdx.x*blockDim.x + blockDim.x - 1] - acc_blk;
    hind = blockDim.x*blockDim.y;
    tmp_id = threadIdx.y*blockDim.x + threadIdx.x;
    for( ; tmp_id < tot_len; tmp_id += hind) {
        d_out[acc_blk+tmp_id] = elm_sh_mem[myHash(tmp_id)];
    }
}


/**
 * The use of this kernel should guarantee that the blocks are full;
 * this is achieved by padding in the host. However the result array
 * is not considered padded (parameter), hence orig_size is tested
 * in the last stage (the update to global from shared memory) so
 * that we do not write out of bounds.
 * If WITH_ARRAY is defined, then accumulator's representation is
 *    an int[2/4/8] local array, hence in L1 => requires replay instrs
 *    OTHERWISE: an MyInt2/4/8 => hold in registers, but leads to divergence.
 * MyInt4 representation seems to be a bit better than int[4]. 
 */
//#define WITH_ARRAY
template<class OP>
__global__ void
writeMultiKernel(   typename OP:: InType* d_in,  int* cond_res, 
                    typename OP::ExpType* perm_chunk, 
                    typename OP:: InType* d_out, 
                    const unsigned int d_height, 
                    const unsigned int orig_size,
                    const          int CHUNK
) {
    typedef typename OP:: InType T;
    typedef typename OP::ExpType M; 

    extern __shared__ char gen_sh_mem[];
    volatile M* ind_sh_mem = (volatile M*) gen_sh_mem;
    volatile T* elm_sh_mem = (volatile T*) gen_sh_mem;
    int k;
//    int col = blockIdx.x*blockDim.x + threadIdx.x;

#ifdef WITH_ARRAY
//    volatile int* acc0 = (volatile int*)(ind_sh_mem + threadIdx.y*blockDim.x + threadIdx.x);
    int  acc0[OP::cardinal];
    #pragma unroll
    for(k = 0; k < OP::cardinal; k++) 
        acc0[k] = 0;
#else
    M acc0;
#endif

    // 1. vertically scan sequentially CHUNK elements, result in acc0
    unsigned int tmp_id = blockIdx.x*blockDim.x + threadIdx.y*CHUNK*d_height + threadIdx.x;

//    if(col < d_height)
    for(k = 0; k < CHUNK; k++, tmp_id+=d_height) {
#ifdef WITH_ARRAY
        acc0[cond_res[tmp_id]]++; //+= (tmp_id < d_size);
#else
        acc0.selInc(cond_res[tmp_id]);
#endif
    }

    { 
#ifdef WITH_ARRAY
        volatile int* mem = (volatile int*)(ind_sh_mem + threadIdx.x*(blockDim.y+1)+threadIdx.y);
        #pragma unroll
        for(k = 0; k < OP::cardinal; k++) {
            int val = acc0[k];
//            __syncthreads();
            mem[k] = val;
        }
#else
        ind_sh_mem[threadIdx.x*(blockDim.y+1)+threadIdx.y] = acc0;
#endif
    }
    __syncthreads();
    // 2. vertical warp-scan of the results from the seq scan step, put result back in acc0
    scanIncWarp<typename OP::AddExpType,M>(ind_sh_mem + threadIdx.y*(blockDim.x+1), threadIdx.x);
    __syncthreads();
    {
#ifdef WITH_ARRAY
        volatile int* mem = (volatile int*)(ind_sh_mem + threadIdx.x*(blockDim.y+1)+threadIdx.y-1);
        #pragma unroll
        for(k = 0; k < OP::cardinal; k++) {
            int val = (threadIdx.y > 0) ? mem[k] : 0;
//            __syncthreads();
            acc0[k] = val;
        }
#else
        if (threadIdx.y > 0) {
            acc0.set(ind_sh_mem[threadIdx.x*(blockDim.y+1)+threadIdx.y-1]);
        } else {
            acc0.zeroOut();
        }
#endif
    }

    int  arrtmp[OP::cardinal];
//    if(col < d_height)
    { // 3. adjust acc0 to reflect block-level indices of the multi-index.
        tmp_id = blockIdx.x*blockDim.x;
#ifdef WITH_ARRAY
        int* blk_beg = (int*)(perm_chunk + tmp_id - 1);
        int* blk_end = (int*)(perm_chunk + min(tmp_id+blockDim.x,d_height) - 1);
        int* blk_prv = (int*)(perm_chunk + tmp_id + threadIdx.x - 1);
        tmp_id = 0;
        #pragma unroll
        for(k = 0; k < OP::cardinal; k++) {
            int beg_blk = (blockIdx.x > 0) * blk_beg[k];
            acc0[k]  += tmp_id + (threadIdx.x > 0) * (blk_prv[k] - beg_blk);
            arrtmp[k] = blk_end[k] - beg_blk;
            tmp_id   += arrtmp[k];
        }
#else
        OP::incWithAccumDiff(acc0, arrtmp, perm_chunk + tmp_id - 1, 
                             perm_chunk + tmp_id + threadIdx.x - 1, 
                             perm_chunk + min(tmp_id+blockDim.x,d_height) - 1 ); 
#endif

    }
    __syncthreads();

    // 4. performs an input-array traversal in which the elements are
    //    recorded in shared mem (using the block-adjusted indices of acc0) 
    tmp_id = blockIdx.x*blockDim.x + threadIdx.y*CHUNK*d_height + threadIdx.x;
//    if (col < d_height)
    for(int k = 0; k < CHUNK; k++, tmp_id+=d_height) {
        int iind = cond_res[tmp_id];
#ifdef WITH_ARRAY
        int shind= acc0[iind];
        elm_sh_mem[myHash(shind)] = d_in[tmp_id];
        //elm_sh_mem[k*blockDim.x*blockDim.y+threadIdx.y*blockDim.x+threadIdx.x] = d_in[tmp_id];
        acc0[iind] = shind + 1;
#else 
        int shind = acc0.selInc(iind);
        elm_sh_mem[myHash(shind-1)] = d_in[tmp_id];
#endif
    }
    __syncthreads();

    // 6. Finally, the shared memory is traverse in order and
    //    and the filtered array is written to global memory;
    //    Since all the elements of an equiv-class are contiguous
    //    in shared memory (and global memory), the uncoalesced 
    //    writes are minimized. 
    {
        int* blk_vlst= ((int*)(perm_chunk + d_height - 1)); // very last (row) scan result
        k   = threadIdx.y*blockDim.x + threadIdx.x;
        unsigned int total_len = blockDim.x*blockDim.y*CHUNK;
        for( ; k < total_len; k+=blockDim.x*blockDim.y) {
            unsigned int glb_ind = 0, loc_ind = k;
            tmp_id = 0;
            while( loc_ind >= arrtmp[tmp_id] && tmp_id < OP::cardinal) {
                glb_ind += blk_vlst[tmp_id];
                loc_ind -= arrtmp[tmp_id];
                tmp_id++;
            }

            tmp_id = glb_ind + loc_ind + (blockIdx.x > 0) * 
                     ((int*) (perm_chunk + blockIdx.x*blockDim.x - 1))[tmp_id]; // blk_beg;
            if(tmp_id < orig_size) 
                d_out[tmp_id] = elm_sh_mem[myHash(k)];
        }
    }
}


/**
 * Needs separate index-value shared memories, eg (8+1)*block size!
 */
template<class OP>
__global__ void
writeMultiKernelOpt(typename OP:: InType* d_in,  int* cond_res, 
                    typename OP::ExpType* perm_chunk, 
                    typename OP:: InType* d_out, 
                    const unsigned int d_height, 
                    const unsigned int orig_size,
                    const          int CHUNK
) {
    typedef typename OP:: InType T;
    typedef typename OP::ExpType M; 

    extern __shared__ char gen_sh_mem[];
    volatile int* ind_sh_mem = (volatile int*) gen_sh_mem;
    volatile T*   elm_sh_mem = (volatile T*) (ind_sh_mem + 32*33);
    int k;

    int acc[OP::cardinal], acc0[OP::cardinal];
    { // 0. init accumulator to the scan-result value of the previous row.
        unsigned int tmp_id = blockIdx.x*blockDim.x;
        int* blk_beg = (int*)(perm_chunk + tmp_id - 1);
        int* blk_end = (int*)(perm_chunk + tmp_id + blockDim.x  - 1);
        int* blk_prv = (int*)(perm_chunk + tmp_id + threadIdx.y - 1);
        tmp_id = 0;
        #pragma unroll
        for(k = 0; k < OP::cardinal; k++) {
            int beg_blk = (blockIdx.x > 0) * blk_beg[k];
            acc[k]  = tmp_id + (threadIdx.y > 0) * (blk_prv[k] - beg_blk);
            acc0[k] = blk_end[k] - beg_blk;
            tmp_id += acc0[k];
        }
    }

    cond_res += blockIdx.x*blockDim.x + threadIdx.y*d_height + threadIdx.x;
    d_in     += blockIdx.x*1024*CHUNK + threadIdx.y*CHUNK*blockDim.x + threadIdx.x;
    for(k=0; k<CHUNK; k++, cond_res+=32*d_height, d_in+=blockDim.x) {
        int cur_cond, lw_cond;
        { // 1. transpose a 32*32 chunk of the cond_res array!
            ind_sh_mem[threadIdx.x*33 + threadIdx.y] = cond_res[0];  
            __syncthreads();
            cur_cond = ind_sh_mem[threadIdx.y*33 + threadIdx.x];
            __syncthreads();
        }
        
        { // 2. scan-warp
            int exp_cond = OP::expand1(cur_cond);
            ind_sh_mem[threadIdx.y*32 + threadIdx.x] = exp_cond;
            //__syncthreads();
            scanIncWarp<typename OP::AddExpType,int>(ind_sh_mem+threadIdx.y*32, threadIdx.x);
            //__syncthreads();
            lw_cond = ind_sh_mem[threadIdx.y*32 + 31];
        }

        { // 3. write to share memory and update vars
            int exp_cond = ind_sh_mem[threadIdx.y*32 + threadIdx.x];
            exp_cond = OP::extract(exp_cond, cur_cond);
            //__syncthreads();
            elm_sh_mem[exp_cond + acc[cur_cond] - 1] = d_in[0];
            //elm_sh_mem[CHUNK*blockDim.x*threadIdx.y+k*blockDim.x+threadIdx.x] = d_in[0]; 
            #pragma unroll
            for(int j = 0; j < OP::cardinal; j++) {
                acc[j] += OP::extract(lw_cond, j);
            }
            __syncthreads();
        }
    }
    
    // 6. Finally, the shared memory is traverse in order and
    //    and the filtered array is written to global memory;
    //    Since all the elements of an equiv-class are contiguous
    //    in shared memory (and global memory), the uncoalesced 
    //    writes are minimized. 
    {
        int* blk_vlst= ((int*)(perm_chunk + d_height - 1)); // very last (row) scan result
        k   = threadIdx.y*blockDim.x + threadIdx.x;
        unsigned int total_len = blockDim.x*blockDim.y*CHUNK;
        for( ; k < total_len; k+=blockDim.x*blockDim.y) {
            unsigned int glb_ind = 0, loc_ind = k;
            unsigned int tmp_id = 0;
            while( loc_ind >= acc0[tmp_id] && tmp_id < OP::cardinal) {
                glb_ind += blk_vlst[tmp_id];
                loc_ind -= acc0[tmp_id];
                tmp_id++;
            }

            tmp_id = glb_ind + loc_ind + (blockIdx.x > 0) * 
                     ((int*) (perm_chunk + blockIdx.x*blockDim.x - 1))[tmp_id]; // blk_beg;
            if(tmp_id < orig_size) 
                d_out[tmp_id] = elm_sh_mem[k];
        }
    }
}

/************************/
/*** TRANSPOSE Kernel ***/
/************************/
// blockDim.y = TILE; blockDim.x = TILE
// each block transposes a square TILE
template <class T, int TILE> 
__global__ void matTransposeTiledPadKer(T* A, T* B, int heightA, int widthA, int orig_size, T padel) {

  __shared__ T tile[TILE][TILE+1];

  int x = blockIdx.x * TILE + threadIdx.x;
  int y = blockIdx.y * TILE + threadIdx.y;

  int ind = y*widthA+x;
  if( x < widthA && y < heightA )
      tile[threadIdx.y][threadIdx.x] = (ind < orig_size) ? A[ind] : padel;

  __syncthreads();

  x = blockIdx.y * TILE + threadIdx.x; 
  y = blockIdx.x * TILE + threadIdx.y;

  ind = y*heightA + x;
  if( x < heightA && y < widthA )
      B[ind] = tile[threadIdx.x][threadIdx.y];
}

#endif //SCAN_KERS

