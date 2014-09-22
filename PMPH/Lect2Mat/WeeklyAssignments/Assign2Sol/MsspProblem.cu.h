#ifndef MSSP_SOL
#define MSSP_SOL

int MsspProblem(int block_size, int inp_size) {
    int  mem_size = inp_size*sizeof(MyInt4);
    int    *inp_h = (int*)malloc(inp_size*sizeof(int));
    int    *inp_d;    cudaMalloc((void**)&inp_d   , inp_size*sizeof(int));
    MyInt4 *inp_lift; cudaMalloc((void**)&inp_lift, mem_size);
    MyInt4 *res_d;    cudaMalloc((void**)&res_d,    mem_size);

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

    int num_blocks = ( (inp_size % block_size) == 0) ?
                        inp_size / block_size     :
                        inp_size / block_size + 1 ;

    unsigned long int elapsed;
    struct timeval t_start, t_end, t_diff;
    gettimeofday(&t_start, NULL); 

    cudaMemcpy(inp_d, inp_h, inp_size*sizeof(int), cudaMemcpyHostToDevice);
    { // KERNELS
        // 1. apply map, i.e., lift each element x to 
        //    (max(x,0),max(x,0), max(x,0), x)
        trivial_map<<< num_blocks, block_size >>>(inp_d, inp_lift, inp_size);
        cudaThreadSynchronize();
    
        // 2. apply scan with the given operator, i.e.,
        //    write the apply operator in class MsspOP in 
        //    ScanKernels.cu.h and call scanInc from ScanHost.cu.h  
        scanInc< MsspOp,MyInt4 > ( block_size, inp_size, inp_lift, res_d );
        cudaThreadSynchronize();
    }
    MyInt4 res_h(0,0,0,0);
    // 3. copy back only the last element of the res_d array (of size sizeof(MyInt4))
    cudaMemcpy(&res_h, res_d+inp_size-1, sizeof(MyInt4), cudaMemcpyDeviceToHost);

    gettimeofday(&t_end, NULL);
    timeval_subtract(&t_diff, &t_end, &t_start);
    elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec); 
    printf("MSSP version runs in: %lu microsecs\n", elapsed);

    printf("RESULT is: %d %d %d %d\n", res_h.x, res_h.y, res_h.z, res_h.w); 

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

#endif //MSSP_SOL

