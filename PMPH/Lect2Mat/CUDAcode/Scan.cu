#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "ScanUtil.cu"


/**
 * block_size is the size of the cuda block (must be a multiple 
 *                of 32 less than 1025)
 * d_size     is the size of both the input and output arrays.
 * d_in       is the device array; it is supposably
 *                allocated and holds valid values (input).
 * d_out      is the output GPU array -- if you want 
 *            its data on CPU needs to copy it back to host.
 */
void scanIncAdd(    unsigned int  block_size,
                    unsigned long d_size, 
                    float*        d_in,  
                    float*        d_out
) {
    unsigned int num_blocks;
    unsigned int sh_mem_size = block_size * sizeof(float);

    num_blocks = ( (d_size % block_size) == 0) ?
                    d_size / block_size     :
                    d_size / block_size + 1 ;

    scanIncKernel<<< num_blocks, block_size, sh_mem_size >>>(d_in, d_out, d_size);

    cudaError_t err = cudaThreadSynchronize();
    if( err != cudaSuccess)
        printf("cudaThreadSynchronize error: %s\n", cudaGetErrorString(err));
    
    if (block_size >= d_size) { return; }

    // Recursive case:
    //   1. allocate new device input & output array of size num_blocks
    //   2. copy in the end-of-block results of the previous scan 
    //   3. scan recursively 
    //   4. accumulate the recursive block scan result to all elements 
    //          of that block
    //   5. clean up
    float *d_rec_in, *d_rec_out;
    cudaMalloc((void**)&d_rec_in , num_blocks*sizeof(float));
    cudaMalloc((void**)&d_rec_out, num_blocks*sizeof(float));

    unsigned int num_blocks_rec = ( (num_blocks % block_size) == 0 ) ?
                                  num_blocks / block_size     :
                                  num_blocks / block_size + 1 ; 

    copyEndOfBlockKernel<<< num_blocks_rec, block_size >>>(d_out, d_rec_in, num_blocks);

    scanIncAdd( block_size, num_blocks, d_rec_in, d_rec_out );

    distributeEndBlock<<< num_blocks, block_size >>>(d_rec_out, d_out, d_size);

    cudaFree(d_rec_in );
    cudaFree(d_rec_out);
}

void scanExcAdd (   unsigned int  block_size,
                    unsigned long d_size, 
                    float*        d_in,  
                    float*        d_out
) {
    unsigned int num_blocks;
    float *d_out_tmp;
    cudaMalloc((void**)&d_out_tmp , d_size*sizeof(float));

    scanIncAdd( block_size, d_size, d_in, d_out_tmp );

    num_blocks = ( (d_size % block_size) == 0) ?
                    d_size / block_size     :
                    d_size / block_size + 1 ;
    shiftRightByOne<<< num_blocks, block_size >>>(d_out_tmp, d_out, 0.0, d_size);
}


/**
 * block_size is the size of the cuda block (must be a multiple 
 *                of 32 less than 1025)
 * d_size     is the size of both the input and output arrays.
 * d_in       is the device array; it is supposably
 *                allocated and holds valid values (input).
 * flags      is the flag array, in which !=0 indicates 
 *                start of a segment.
 * d_out      is the output GPU array -- if you want 
 *            its data on CPU needs to copy it back to host.
 */
void sgmScanIncAdd( unsigned int  block_size,
                    unsigned long d_size, 
                    float*        d_in,  //device
                    int*          flags, //device
                    float*        d_out  //device
) {
    unsigned int num_blocks;
    unsigned int val_sh_size = block_size * sizeof(float);
    unsigned int flg_sh_size = block_size * sizeof(int  );

    num_blocks = ( (d_size % block_size) == 0) ?
                    d_size / block_size     :
                    d_size / block_size + 1 ;

    float *d_rec_in;
    int   *f_rec_in;
    cudaMalloc((void**)&d_rec_in, num_blocks*sizeof(float));
    cudaMalloc((void**)&f_rec_in, num_blocks*sizeof(int  ));

    sgmScanIncKernel<<< num_blocks, block_size, val_sh_size+flg_sh_size >>>
                    (d_in, flags, d_out, f_rec_in, d_rec_in, d_size);

    cudaError_t err = cudaThreadSynchronize();
    if( err != cudaSuccess)
        printf("cudaThreadSynchronize error: %s\n", cudaGetErrorString(err));
    
    int   h_f_rec[128];
    float h_d_rec[128];
    cudaMemcpy(h_f_rec, f_rec_in, 4*sizeof(int), cudaMemcpyDeviceToHost);
    cudaMemcpy(h_d_rec, d_rec_in, 4*sizeof(float), cudaMemcpyDeviceToHost);
    cudaThreadSynchronize();
    printf("Flags rec: %d %d %f %f\n",
                h_f_rec[0], h_f_rec[1], 
                h_d_rec[0], h_d_rec[1] );


    if (block_size >= d_size) { cudaFree(d_rec_in); cudaFree(f_rec_in); return; }

    // Recursive case:
    //   1. allocate new device input & output array of size num_blocks
    //   2. copy in the end-of-block results of the previous scan 
    //   3. scan recursively 
    //   4. accumulate the recursive block scan result to all elements 
    //          of that block
    //   5. clean up
    float *d_rec_out;
    int   *f_inds;
    cudaMalloc((void**)&d_rec_out, num_blocks*sizeof(float));
    cudaMalloc((void**)&f_inds,    d_size    *sizeof(int  ));

    sgmScanIncAdd( block_size, num_blocks, d_rec_in, f_rec_in, d_rec_out );

    scanIncKernelAddInt<<< num_blocks, block_size, flg_sh_size >>>(flags, f_inds, d_size);

    sgmDistributeEndBlock<<< num_blocks, block_size >>>(d_rec_out, d_out, 
                                                        f_inds, d_size);

    cudaFree(d_rec_in );
    cudaFree(d_rec_out);
    cudaFree(f_rec_in );
    cudaFree(f_inds   );
}

int main(int argc, char** argv) {
    unsigned int num_threads = 128;
    unsigned int mem_size    = num_threads * sizeof(float);

    float* h_in  = (float*) malloc(mem_size);
    float* h_out = (float*) malloc(mem_size);
    int  * flags_h = (int*) malloc(num_threads*sizeof(int));

    for(int i=0; i<num_threads; i++) {
        h_in[i] = (float) 1; flags_h[i] = 0;
    }
    flags_h[0] = 1; flags_h[30] = 1; flags_h[60] = 1; flags_h[90] = 1;

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
    sgmScanIncAdd( 32, num_threads, d_in, flags_d, d_out );
    //scanExcKernel<<< 2, num_threads/2, 512 >>>(d_in, d_out, num_threads);

    // copy host memory to device
    cudaMemcpy(h_out, d_out, mem_size, cudaMemcpyDeviceToHost);

    bool success = true;
    for(int i=0; i<num_threads; i++) {
        printf("%.1f\n", h_out[i]);
        if(h_out[i]!=((i+1)%33)) success = false;
    }
    if(success) printf("SUCCESS!\n");
    else        printf("FAILED! \n");

    // cleanup memory
    free(h_in );
    free(h_out);
    free(flags_h);
    cudaFree(d_in );
    cudaFree(d_out);
    cudaFree(flags_d);

    return 0;
}

