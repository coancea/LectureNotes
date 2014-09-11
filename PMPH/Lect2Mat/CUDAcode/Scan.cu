#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include <cuda_runtime.h>

typedef float T;

template<class T>
class Add {
  public:
    static __device__ T identity()        { return 0.0;     }
    static __device__ T apply(T t1, T t2) { return t1 + t2; }
};

template<class OP, class T>
__device__ T 
scan_warp(volatile T* ptr, const unsigned int idx) {
    const unsigned int lane = idx & 31;

    if (lane >= 1)  ptr[idx] = OP::apply(ptr[idx-1],  ptr[idx]);
    if (lane >= 2)  ptr[idx] = OP::apply(ptr[idx-1],  ptr[idx]);
    if (lane >= 4)  ptr[idx] = OP::apply(ptr[idx-1],  ptr[idx]);
    if (lane >= 8)  ptr[idx] = OP::apply(ptr[idx-1],  ptr[idx]);
    if (lane >= 16) ptr[idx] = OP::apply(ptr[idx-1],  ptr[idx]);

    return (lane > 0) ? ptr[idx-1] : OP::identity();
}

template<class OP, class T>
__device__ T 
scan_block(volatile T* ptr, const unsigned int idx) {
    const unsigned int lane   = idx &  31;
    const unsigned int warpid = idx >> 5;

    T val = scan_warp<OP,T>(ptr,idx);

    ptr[idx] = OP::identity();
    __syncthreads();

    if (lane == 31) ptr[warpid] = val; //ptr[idx];
    __syncthreads();

    if (warpid == 0) scan_warp<OP,T>(ptr, idx);
    __syncthreads();

    if (warpid > 0) {
        val = OP::apply(ptr[warpid-1], val);
    }

    return val; //ptr[idx];
}

__global__ void 
scanKernel(float* d_in, float* d_out) {
    __shared__ float sh_mem[512];
    const unsigned int tid = threadIdx.x;
    const unsigned int gid = blockIdx.x*blockDim.x + tid;
    float el    = d_in[gid];
    sh_mem[tid] = el;
    float res   = scan_block < Add<T>, T >(sh_mem, tid);
    d_out[gid]  = res; 
}


int main(int argc, char** argv) {
    unsigned int num_threads = 32;
    unsigned int mem_size    = num_threads * sizeof(float);

    float* h_in  = (float*) malloc(mem_size);
    float* h_out = (float*) malloc(mem_size);

    for(int i=0; i<num_threads; i++) {
        h_in[i] = (float) i;
    }

    // allocate device memory
    float* d_in;
    float* d_out;
    cudaMalloc((void**)&d_in , mem_size);
    cudaMalloc((void**)&d_out, mem_size);

    // copy host memory to device
    cudaMemcpy(d_in, h_in, mem_size, cudaMemcpyHostToDevice);

    // execute kernel
    scanKernel<<< 1, num_threads >>>(d_in, d_out);

    // copy host memory to device
    cudaMemcpy(h_out, d_out, mem_size, cudaMemcpyDeviceToHost);


    for(int i=0; i<num_threads; i++) {
        printf("%.1f\n", h_out[i]);
    }

    // cleanup memory
    free(h_in );
    free(h_out);
    cudaFree(d_in );
    cudaFree(d_out);

    return 0;
}

