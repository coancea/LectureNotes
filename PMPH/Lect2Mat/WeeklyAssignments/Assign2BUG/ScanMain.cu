#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "ScanHost.cu.h"

int main(int argc, char** argv) {
    unsigned int num_threads = 8353455;
    unsigned int mem_size    = num_threads * sizeof(float);

    float* h_in  = (float*) malloc(mem_size);
    float* h_out = (float*) malloc(mem_size);
    int  * flags_h = (int*) malloc(num_threads*sizeof(int));

    int sgm_size = 123;
    { // init segments and flags
        for(unsigned int i=0; i<num_threads; i++) {
            h_in[i] = (float) 1; 
            flags_h[i] = (i % sgm_size == 0) ? 1 : 0;
        }
    }
    // allocate device memory
    float* d_in;
    float* d_out;
    int  * flags_d;
    cudaMalloc((void**)&d_in ,   mem_size);
    cudaMalloc((void**)&d_out,   mem_size);
    cudaMalloc((void**)&flags_d, num_threads*sizeof(int));

    // copy host memory to device
    cudaMemcpy(d_in, h_in, mem_size, cudaMemcpyHostToDevice);
    cudaMemcpy(flags_d, flags_h, num_threads*sizeof(int), cudaMemcpyHostToDevice);

    // execute kernel
    sgmScanIncAdd< Add<float>,float,Add<int> >
        ( 512, num_threads, d_in, flags_d, d_out );
    //scanIncAdd< Add<float>,float >
    //            ( 512, num_threads, d_in, d_out );

    // copy host memory to device
    cudaMemcpy(h_out, d_out, mem_size, cudaMemcpyDeviceToHost);

    { // validation
        bool  success = true;
        float accum   = 0.0;
        for(int i=0; i<num_threads; i++) {
            if (i % sgm_size == 0) accum  = 1.0;
            else                   accum += 1.0;
            if (accum != h_out[i]) { 
                success = false;
                printf("Violation: %.1f should be %.1f\n", h_out[i], accum);
            }
        }
        if(success) printf("\n  VALID RESULT!\n");
        else        printf("\nINVALID RESULT!\n");
    }

    //printf("%d %d", sizeof(int), sizeof(unsigned int));
    // cleanup memory
    free(h_in );
    free(h_out);
    free(flags_h);
    cudaFree(d_in );
    cudaFree(d_out);
    cudaFree(flags_d);

    return 0;
}

