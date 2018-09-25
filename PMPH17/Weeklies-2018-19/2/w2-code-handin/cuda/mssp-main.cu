#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "scan-host.cu.h"
#include "mssp-kernels.cu.h"

/** 
 * block size is the size of the CUDA block
 * inp_size is the size of the input array (to MSSP)
 */
int MsspProblem(int block_size, int inp_size) {
    int  mem_size = inp_size*sizeof(MyInt4);
    int    *inp_h = (int*)malloc(inp_size*sizeof(int));
    int    *inp_d;    cudaMalloc((void**)&inp_d   , inp_size*sizeof(int));
    MyInt4 *inp_lift; cudaMalloc((void**)&inp_lift, mem_size);
    MyInt4 *res_d;    cudaMalloc((void**)&res_d,    mem_size);

    // fill in the input array
    for(int i = 0; i < inp_size; i+=9) {
        inp_h[i   ] = -15;
        inp_h[i+1 ] = 2;
        inp_h[i+2 ] = -1;
        inp_h[i+3 ] = 3;
        inp_h[i+4 ] = -2;
        inp_h[i+5 ] = 4;
        inp_h[i+6 ] = 3;
        inp_h[i+7 ] = 1;
        inp_h[i+8 ] = -4;
    }
    inp_h[(inp_size/9)*9 - 18 + 7] = 2;

    unsigned long int elapsed;
    struct timeval t_start, t_end, t_diff;
    gettimeofday(&t_start, NULL); 

    cudaMemcpy(inp_d, inp_h, inp_size*sizeof(int), cudaMemcpyHostToDevice);
    { // KERNELS
        // 1. this is supposed to implement the map part of MSSP.
        // ToDo: a) fill in the implementation of trivial_map kernel in
        //          file mssp-kernels.cu.h
        //       b) also compute the number of blocks needed to spawn the kernel
        int num_blocks = 0; // ... replace this dummy implementation with something that makes sense ...
        trivial_map<<< num_blocks, block_size >>>(inp_d, inp_lift, inp_size);
    
        // 2. apply scanInc with the given operator from scan-host.cu.h
        //    ToDo: a) write the apply operator in class MsspOp in 
        //             mssp-kernels.cu.h   
        scanInc< MsspOp > ( block_size, inp_size, inp_lift, res_d );
        cudaThreadSynchronize();
    }
    MyInt4 res_h(0,0,0,0);
    // 3. copy back only the last element of the res_d array (of size sizeof(MyInt4))
    cudaMemcpy(&res_h, res_d+inp_size-1, sizeof(MyInt4), cudaMemcpyDeviceToHost);

    gettimeofday(&t_end, NULL);
    timeval_subtract(&t_diff, &t_end, &t_start);
    elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec); 
    printf("MSSP version runs in: %lu microsecs\n", elapsed);

    printf("RESULT is: mssp=%d, mis=%d, mcs=%d, ts=%d\n", res_h.x, res_h.y, res_h.z, res_h.w); 

    if(res_h.x == 11) {
        printf("MSSP VALID EXECUTION!\n");
    } else {
        printf("MSSP INVALID EXECUTION!\n");
    }

    free(inp_h);
    cudaFree(inp_d);
    cudaFree(inp_lift);
    cudaFree(res_d);

    return 1;
}

int main() {
    const unsigned int mssp_list_size = 8353455;
    const unsigned int block_size     = 256;
    MsspProblem(block_size, mssp_list_size);
}
