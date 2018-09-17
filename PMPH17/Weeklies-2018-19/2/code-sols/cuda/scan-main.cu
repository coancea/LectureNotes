#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "scan-host.cu.h"

int scanIncTest( const unsigned int block_size
               , const unsigned int array_len) {

    unsigned int mem_size = array_len * sizeof(int);
    int* h_in    = (int*) malloc(mem_size);
    int* h_out   = (int*) malloc(mem_size);

    {   // some trivial initialization for value and flag arrays
        for(unsigned int i=0; i<array_len; i++) {
            h_in   [i] = 1; 
        }
    }

    unsigned long int elapsed;
    struct timeval t_start, t_end, t_diff;

    { // calling exclusive (segmented) scan
        int* d_in;
        int* d_out;
        cudaMalloc((void**)&d_in ,   mem_size);
        cudaMalloc((void**)&d_out,   mem_size);

        // copy host memory to device
        cudaMemcpy(d_in, h_in, mem_size, cudaMemcpyHostToDevice);

        // execute kernel and time it
        gettimeofday(&t_start, NULL); 
        scanInc< Add<int> > ( block_size, array_len, d_in, d_out );
        gettimeofday(&t_end, NULL);

        // copy host memory to device
        cudaMemcpy(h_out, d_out, mem_size, cudaMemcpyDeviceToHost);

        // cleanup memory
        cudaFree(d_in );
        cudaFree(d_out);
    }
   
    timeval_subtract(&t_diff, &t_end, &t_start);
    elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec); 
    printf("Scan Inclusive on GPU runs in: %lu microsecs! CPU-GPU transfers and GPU allocations were not measured.\n", elapsed);

    { // validation
        bool  success = true;
        int   accum   = 0;
        for(int i=0; i<array_len && success; i++) {
            accum += h_in[i];
            if ( accum != h_out[i] ) {
                success = false;
                printf("Scan Inclusive First Violation: %.1d should be %.1d Exiting!\n", h_out[i], accum);
            }
        }
        if(success) printf("Scan Inclusive +   VALID RESULT!\n\n");
        else        printf("Scan Inclusive + INVALID RESULT!\n\n");
    }

    // cleanup memory
    free(h_in );
    free(h_out);

    return 0;
}

int sgmScanIncTest( const unsigned int block_size
                  , const unsigned int array_len) {

    unsigned int mem_size = array_len * sizeof(int);
    int* h_in    = (int*) malloc(mem_size);
    int* h_out   = (int*) malloc(mem_size);
    int* flags_h = (int*) malloc(array_len*sizeof(int));

    int sgm_size = 123;
    {   // some trivial initialization for value and flag arrays
        for(unsigned int i=0; i<array_len; i++) {
            h_in   [i] = 1; 
            flags_h[i] = (i % sgm_size == 0) ? 1 : 0;
        }
    }

    unsigned long int elapsed;
    struct timeval t_start, t_end, t_diff;

    { // calling exclusive (segmented) scan
        int* d_in;
        int* d_out;
        int* flags_d;
        cudaMalloc((void**)&d_in ,   mem_size);
        cudaMalloc((void**)&d_out,   mem_size);
        cudaMalloc((void**)&flags_d, array_len*sizeof(int));

        // copy host memory to device
        cudaMemcpy(d_in, h_in, mem_size, cudaMemcpyHostToDevice);
        cudaMemcpy(flags_d, flags_h, array_len*sizeof(int), cudaMemcpyHostToDevice);

        // execute kernel and time it
        gettimeofday(&t_start, NULL); 
        sgmScanInc< Add<int> > ( block_size, array_len, d_in, flags_d, d_out );
        gettimeofday(&t_end, NULL);

        // copy host memory to device
        cudaMemcpy(h_out, d_out, mem_size, cudaMemcpyDeviceToHost);

        // cleanup memory
        cudaFree(d_in );
        cudaFree(d_out);
        cudaFree(flags_d);
    }
   
    timeval_subtract(&t_diff, &t_end, &t_start);
    elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec); 
    printf("Segmented Scan Inclusive on GPU runs in: %lu microsecs! CPU-GPU transfers and GPU allocations were not measured.\n", elapsed);

    { // validation
        bool  success = true;
        int   accum   = 0;
        for(int i=0; i<array_len && success; i++) {

            if (flags_h[i] == 1) accum  = h_in[i];
            else                 accum += h_in[i];
            
            if ( accum != h_out[i] ) {
                success = false;
                printf("Segmented Scan Inclusive First Violation: %.1d should be %.1d Exiting!\n", h_out[i], accum);
            }
        }
        if(success) printf("Segmented Scan Inclusive +   VALID RESULT!\n\n");
        else        printf("Segmented Scan Inclusive + INVALID RESULT!\n\n");
    }

    // cleanup memory
    free(h_in );
    free(h_out);
    free(flags_h);

    return 0;
}

int sgmScanExcTest( const unsigned int block_size
                  , const unsigned int array_len) {
    unsigned int mem_size = array_len * sizeof(int);
    int* h_in    = (int*) malloc(mem_size);
    int* h_out   = (int*) malloc(mem_size);
    int* flags_h = (int*) malloc(array_len*sizeof(int));

    int sgm_size = 123;
    {   // some trivial initialization for value and flag arrays
        for(unsigned int i=0; i<array_len; i++) {
            h_in   [i] = 1; 
            flags_h[i] = (i % sgm_size == 0) ? 1 : 0;
        }
    }

    unsigned long int elapsed;
    struct timeval t_start, t_end, t_diff;

    { // calling exclusive (segmented) scan
        int* d_in;
        int* d_out;
        int* flags_d;
        cudaMalloc((void**)&d_in ,   mem_size);
        cudaMalloc((void**)&d_out,   mem_size);
        cudaMalloc((void**)&flags_d, array_len*sizeof(int));

        // copy host memory to device
        cudaMemcpy(d_in, h_in, mem_size, cudaMemcpyHostToDevice);
        cudaMemcpy(flags_d, flags_h, array_len*sizeof(int), cudaMemcpyHostToDevice);

        // execute kernel and time it
        gettimeofday(&t_start, NULL); 
        sgmScanExc< Add<int> > ( block_size, array_len, 0, d_in, flags_d, d_out );
        gettimeofday(&t_end, NULL);

        // copy host memory to device
        cudaMemcpy(h_out, d_out, mem_size, cudaMemcpyDeviceToHost);

        // cleanup memory
        cudaFree(d_in );
        cudaFree(d_out);
        cudaFree(flags_d);
    }
   
    timeval_subtract(&t_diff, &t_end, &t_start);
    elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec); 
    printf("Segmented Scan Exclusive on GPU runs in: %lu microsecs! CPU-GPU transfers and GPU allocations were not measured.\n", elapsed);

    { // validation
        bool  success = true;
        int   accum   = 0;
        for(int i=0; i<array_len && success; i++) {

            if (flags_h[i] == 1) accum  = 0;
            else                 accum += h_in[i-1];
            
            if ( accum != h_out[i] ) {
                success = false;
                printf("Segmented Scan Exclusive First Violation: %.1d should be %.1d Exiting!\n", h_out[i], accum);
            }
        }
        if(success) printf("Segmented Scan Exclusive +   VALID RESULT!\n\n");
        else        printf("Segmented Scan Exclusive + INVALID RESULT!\n\n");
    }

    // cleanup memory
    free(h_in );
    free(h_out);
    free(flags_h);

    return 0;
}

int main(int argc, char** argv) { 
    const unsigned int array_len  = 8353455;
    const unsigned int block_size = 512;

    scanIncTest(block_size, array_len);
    sgmScanIncTest(block_size, array_len);
    sgmScanExcTest(block_size, array_len);
}
