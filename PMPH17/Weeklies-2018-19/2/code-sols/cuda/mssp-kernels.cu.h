#ifndef MSSP_KERS
#define MSSP_KERS

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
};

class MsspOp {
  public:
    typedef MyInt4 ElmTp;
    static __device__ inline MyInt4 identity() { return MyInt4(0,0,0,0); }  
    static __device__ inline MyInt4 apply(volatile MyInt4& t1, volatile MyInt4& t2) { 
        int mss = max(t1.x, max(t2.x,t1.z+t2.y));
        int mis = max(t1.y, t1.w+t2.y);
        int mcs = max(t2.z, t1.z+t2.w);
        int t   = t1.w + t2.w;
        return MyInt4(mss, mis, mcs, t); 
    }
};

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

#endif 
