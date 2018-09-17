#ifndef SCAN_HOST
#define SCAN_HOST

#include "scan-kernels.cu.h"

#include <sys/time.h>
#include <time.h> 

int timeval_subtract(struct timeval *result, struct timeval *t2, struct timeval *t1)
{
    unsigned int resolution=1000000;
    long int diff = (t2->tv_usec + resolution * t2->tv_sec) - (t1->tv_usec + resolution * t1->tv_sec);
    result->tv_sec = diff / resolution;
    result->tv_usec = diff % resolution;
    return (diff<0);
}

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
template<class OP>
void scanInc(    unsigned int  block_size,
                 unsigned int  d_size, 
                 typename OP::ElmTp*  d_in,  // device
                 typename OP::ElmTp*  d_out  // device
) {
    typedef typename OP::ElmTp T;
    unsigned int num_blocks;
    unsigned int sh_mem_size = block_size * 32; //sizeof(T);

    num_blocks = ( (d_size % block_size) == 0) ?
                    d_size / block_size     :
                    d_size / block_size + 1 ;

    scanIncKernel<OP><<< num_blocks, block_size, sh_mem_size >>>(d_in, d_out, d_size);
    cudaThreadSynchronize();
    
    if (block_size >= d_size) { return; }

    /**********************/
    /*** Recursive Case ***/
    /**********************/

    //   1. allocate new device input & output array of size num_blocks
    T *d_rec_in, *d_rec_out;
    cudaMalloc((void**)&d_rec_in , num_blocks*sizeof(T));
    cudaMalloc((void**)&d_rec_out, num_blocks*sizeof(T));

    unsigned int num_blocks_rec = ( (num_blocks % block_size) == 0 ) ?
                                  num_blocks / block_size     :
                                  num_blocks / block_size + 1 ; 

    //   2. copy in the end-of-block results of the previous scan
    copyEndOfBlockKernel<T><<< num_blocks_rec, block_size >>>(d_out, d_rec_in, num_blocks);
    cudaThreadSynchronize();

    //   3. scan recursively the last elements of each CUDA block
    scanInc<OP>( block_size, num_blocks, d_rec_in, d_rec_out );

    //   4. distribute the the corresponding element of the 
    //      recursively scanned data to all elements of the
    //      corresponding original block
    distributeEndBlock<OP><<< num_blocks, block_size >>>(d_rec_out, d_out, d_size);
    cudaThreadSynchronize();

    //   5. clean up
    cudaFree(d_rec_in );
    cudaFree(d_rec_out);
}


/**
 * block_size is the size of the cuda block (must be a multiple 
 *                of 32 less than 1025)
 * d_size     is the size of both the input and output arrays.
 * d_in       is the device array; it is supposably
 *                allocated and holds valid values (input).
 * d_flg      is the flag array, in which !=0 indicates 
 *                start of a segment.
 * d_out      is the output GPU array -- if you want 
 *            its data on CPU you need to copy it back to host.
 *
 * OP         class denotes the associative binary operator 
 *                and should have an implementation similar to 
 *                `class Add' in ScanUtil.cu, i.e., exporting
 *                `identity' and `apply' functions.
 * OP::ElmTp  denotes the type on which OP operates, 
 *                e.g., float or int.  For example to do
 *                segmented scan with (+) on ints, one should
 *                call sgmScanInc<Add<int> >(...)
 */
template<class OP>
void sgmScanInc( const unsigned int block_size,
                 const unsigned int d_size,
                 typename OP::ElmTp * d_in,  //device
                 int*                 d_flg, //device
                 typename OP::ElmTp * d_out  //device
) {
    typedef typename OP::ElmTp T;
    unsigned int num_blocks = (d_size + block_size - 1) / block_size;
    TupleInt<T> *d_tup_in, *d_tup_out;
    cudaMalloc((void**)&d_tup_in , d_size*sizeof(TupleInt<T>));
    cudaMalloc((void**)&d_tup_out, d_size*sizeof(TupleInt<T>));

    zipFlags<T><<< num_blocks, block_size >>>(d_tup_in, d_flg, d_in, d_size);
    scanInc< SgmOp<OP> > ( block_size, d_size, d_tup_in,  d_tup_out );
    remFlags<T><<< num_blocks, block_size >>>(d_out, d_tup_out, d_size);
    cudaFree(d_tup_in);
    cudaFree(d_tup_out);
    cudaThreadSynchronize();
}


template<class OP>
void sgmScanExc( const unsigned int block_size,
                 const unsigned int d_size,
                 typename OP::ElmTp ne,
                 typename OP::ElmTp * d_in,  //device
                 int*                 d_flg, //device
                 typename OP::ElmTp * d_out  //device
) {
    typedef typename OP::ElmTp T;
    // ... fill in the implementation here ...
}
#endif //SCAN_HOST
