#ifndef EXCLUSIVE_SCAN
#define EXCLUSIVE_SCAN

/**
 * block_size is the size of the cuda block (must be a multiple 
 *                of 32 less than 1025)
 * d_size     is the size of both the input and output arrays.
 * d_in       is the device array; it is supposably
 *                allocated and holds valid values (input).
 * d_out      is the output GPU array -- if you want 
 *            its data on CPU needs to copy it back to host.
 *
 * OP         class denotes the associative binary operator 
 *                and should have an implementation similar to 
 *                `class Add' in ScanUtil.cu, i.e., exporting
 *                `identity' and `apply' functions.
 * T          denotes the type on which OP operates, 
 *                e.g., float or int. 
 */
template<class OP, class T> inline
void scanExc(    unsigned int  block_size,
                 unsigned long d_size, 
                 T*            d_in,  // device
                 T*            d_out  // device
) {

    unsigned int num_blocks;

    num_blocks = ( (d_size % block_size) == 0) ?
                    d_size / block_size     :
                    (d_size / block_size) + 1 ;

    // 1. allocates temporary array to hold the result of inclusive scan
    T* arr_inc_scan;
    cudaMalloc((void**)&arr_inc_scan, d_size*sizeof(T));

    // 2. calls CPU stub for inclusive scan (from ScanHost.cu.h)
    scanInc<OP,T>( block_size, d_size, d_in, arr_inc_scan );

    // 3. call `shiftRightByOne' kernel to "rotate" the result of inclusive scan,
    //    i.e., d_out[0]<- 0, d_out[i] <- arr_inc_scan[i-1]
    shiftRightByOne<T> <<< num_blocks, block_size >>>(arr_inc_scan, d_out, OP::identity(), d_size);
    cudaThreadSynchronize();

    cudaFree(arr_inc_scan);
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
 *            its data on CPU you need to copy it back to host.
 *
 * OP         class denotes the associative binary operator 
 *                and should have an implementation similar to 
 *                `class Add' in ScanUtil.cu, i.e., exporting
 *                `identity' and `apply' functions.
 * T          denotes the type on which OP operates, 
 *                e.g., float or int. 
 */
template<class OP, class T> inline
void sgmScanExc( unsigned int  block_size,
                 unsigned long d_size, 
                 T*            d_in,  // device
                 int*          flags, //device
                 T*            d_out  // device
) {

    unsigned int num_blocks;

    num_blocks = ( (d_size % block_size) == 0) ?
                    d_size / block_size     :
                    (d_size / block_size) + 1 ;

    // 1. allocates temporary array to hold the result of inclusive scan
    T* arr_inc_scan;
    cudaMalloc((void**)&arr_inc_scan, d_size*sizeof(T));

    // 2. calls CPU stub for segmented inclusive scan (from ScanHost.cu.h)
    sgmScanInc<OP,T>( block_size, d_size, d_in, flags, arr_inc_scan );

    // 3. call `sgmShiftRightByOne' kernel to "rotate" the result of 
    //    segmented inclusive scan, i.e., d_out[i]<- (flags[i]!=0)? 0 : arr_inc_scan[i-1]
    sgmShiftRightByOne<T> <<< num_blocks, block_size >>>(arr_inc_scan, flags, d_out, OP::identity(), d_size);
    cudaThreadSynchronize();

    cudaFree(arr_inc_scan);
}


int scanExcTest() {
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

    unsigned long int elapsed;
    struct timeval t_start, t_end, t_diff;
    gettimeofday(&t_start, NULL); 


    { // calling exclusive (segmented) scan
        int* d_in;
        int* d_out;
        int* flags_d;
        cudaMalloc((void**)&d_in ,   mem_size);
        cudaMalloc((void**)&d_out,   mem_size);
        cudaMalloc((void**)&flags_d, num_threads*sizeof(int));

        // copy host memory to device
        cudaMemcpy(d_in, h_in, mem_size, cudaMemcpyHostToDevice);
        cudaMemcpy(flags_d, flags_h, num_threads*sizeof(int), cudaMemcpyHostToDevice);

        // execute kernel
        sgmScanExc< Add<int>,int > ( block_size, num_threads, d_in, flags_d, d_out );
        //scanExc< Add<int>,int > ( block_size, num_threads, d_in, d_out );

        // copy host memory to device
        cudaMemcpy(h_out, d_out, mem_size, cudaMemcpyDeviceToHost);

        // cleanup memory
        cudaFree(d_in );
        cudaFree(d_out);
        cudaFree(flags_d);
    }

    gettimeofday(&t_end, NULL);
    timeval_subtract(&t_diff, &t_end, &t_start);
    elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec); 
    printf("Scan Exclusive on GPU runs in: %lu microsecs\n", elapsed);


    { // validation
        bool  success = true;
        int   accum   = 0;
        for(int i=0; i<num_threads; i++) {
            // for segmented scan exclusive test
            if (i % sgm_size == 0) accum  = 0;
            else                   accum += 1;
            
            if ( accum != h_out[i] ) { 
                success = false;
                printf("Scan Exclusive Violation: %.1d should be %.1d\n", h_out[i], accum);
            }
            // for scan exclusive test
//            accum += 1;
        }
        if(success) printf("\nScan Exclusive +   VALID RESULT!\n");
        else        printf("\nScan Exclusive + INVALID RESULT!\n");
    }

    // cleanup memory
    free(h_in );
    free(h_out);
    free(flags_h);

    return 0;
}


#endif // EXCLUSIVE_SCAN
