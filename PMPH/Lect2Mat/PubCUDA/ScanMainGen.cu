#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "ScanHost.cu.h"



template<class OP, class T>
int testGeneric(    unsigned int num_threads, 
                    unsigned int block_size,
                    T*           h_in,
                    int*         flags_h,
                    T*           h_out
) {
    unsigned int mem_size    = num_threads * sizeof(T);

    // allocate device memory
    T*   d_in;
    T*   d_out;
    int* flags_d;
    cudaMalloc((void**)&d_in ,   mem_size);
    cudaMalloc((void**)&d_out,   mem_size);
    cudaMalloc((void**)&flags_d, num_threads*sizeof(int));

    // copy host memory to device
    cudaMemcpy(d_in, h_in, mem_size, cudaMemcpyHostToDevice);
    cudaMemcpy(flags_d, flags_h, num_threads*sizeof(int), cudaMemcpyHostToDevice);

    { // execute kernel
        unsigned long int elapsed;
        struct timeval t_start, t_end, t_diff;
        gettimeofday(&t_start, NULL); 

        sgmScanInc< OP,T > ( block_size, num_threads, d_in, flags_d, d_out );
        //scanInc< OP,T > ( block_size, num_threads, d_in, d_out );

        gettimeofday(&t_end, NULL);
        timeval_subtract(&t_diff, &t_end, &t_start);
        elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec); 
        printf("Sequential Naive version runs in: %lu microsecs\n", elapsed);
    }
    // copy host memory to device
    cudaMemcpy(h_out, d_out, mem_size, cudaMemcpyDeviceToHost);

    // cleanup memory
    cudaFree(d_in );
    cudaFree(d_out);
    cudaFree(flags_d);
    return 1;
}


int mainTest(int argc, char** argv) {
    const unsigned int num_threads = 8353455;
    const unsigned int block_size  = 512;
    unsigned int mem_size = num_threads * sizeof(int);

    int* h_in    = (int*) malloc(mem_size);
    int* h_out   = (int*) malloc(mem_size);
    int* flags_h = (int*) malloc(num_threads*sizeof(int));

    int sgm_size = 123;
    { // init segments and flags
        for(unsigned int i=0; i<num_threads; i++) {
            h_in   [i] = 1; 
            flags_h[i] = (i % sgm_size == 0) ? 1 : 0;
        }
    }

    testGeneric<Add<int>,int>( num_threads, block_size, h_in, flags_h, h_out );

    { // validation
        bool  success = true;
        int   accum   = 0;
        for(int i=0; i<num_threads; i++) {
            if (i % sgm_size == 0) accum  = 1;
            else                   accum += 1;
            //accum += 1;
            if ( accum != h_out[i] ) { 
                success = false;
                printf("Violation: %.1d should be %.1d\n", h_out[i], accum);
            }
        }
        if(success) printf("\n  VALID RESULT!\n");
        else        printf("\nINVALID RESULT!\n");
    }

    // cleanup memory
    free(h_in );
    free(h_out);
    free(flags_h);

    return 0;
}
 
#include "MsspProblem.cu.h" 
#include "SparseMatVctMult.cu.h" 

int main(int argc, char** argv) { 
    const unsigned int mssp_list_size = 8353455; 
    const unsigned int matrix_row_num = 11033;
    const unsigned int vct_size       = 2076;
    const unsigned int block_size     = 256;

    MsspProblem(block_size, mssp_list_size);
    SparseMatVctMult(block_size, matrix_row_num, vct_size);
}
