#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "ParBBHost.cu.h"

#if 0
#include "MsspProblem.cu.h" 
#endif

int testClassicFilter(  const unsigned int num_elems, 
                        const unsigned int num_hwd_thds, 
                        const int          VERSION   
) {
    unsigned int mem_size = num_elems * sizeof(float);

    float* h_in    = (float*) malloc(mem_size);
    float* h_out   = (float*) malloc(mem_size);

    { // init segments and flags
        std::srand(33); // use current time as seed for random generator
        for(unsigned int i=0; i<num_elems; i++) {
            int r = std::rand();
            h_in[i] = ((float)r)/RAND_MAX; 
        }
    }

    float *d_in, *d_out;
    { // device allocation and copyin
        cudaMalloc((void**)&d_in ,   mem_size);
        cudaMalloc((void**)&d_out,   mem_size);

        // copy host memory to device
        cudaMemcpy(d_in, h_in, mem_size, cudaMemcpyHostToDevice);
        cudaThreadSynchronize();
    }
    
    unsigned int filt_size;
    { // kernel calls
        if(VERSION == 1) {  // Traditional all the way!
            filt_size = filterTrad<LessThan>( num_elems, num_hwd_thds, d_in, d_out );  //<0.5> 
        } else if (VERSION == 2 || VERSION == 3) {
            filt_size = filterTradChunked<LessThan>( num_elems, num_hwd_thds, d_in, d_out, VERSION );
        } else {
            printf("\n\n\n!!!! ERROR!!!! VERSION: %d NOT SUPPORTED ????\n\n\n", VERSION);
        }
        cudaMemcpy(h_out, d_out, mem_size, cudaMemcpyDeviceToHost);
        cudaThreadSynchronize();
        cudaFree(d_in );
        cudaFree(d_out);
    }

    { // validation
        bool  success = true;
        unsigned int accum   = 0;
        for(int i=0; i<num_elems; i++) {
            // for segmented scan exclusive test
            float cur_el = h_in[i];
            int   cond   = LessThan::apply(h_in[i]);  //<0.5>
            if(cond > 0) {
                if(cur_el != h_out[accum]) {
                    success = false;
                    //printf("Filter (Classical) Violation: %.4f should be %.4f (i,accum): (%d,%d)\n", h_out[accum], cur_el, i,accum);
                    //if(accum > 32) exit(0);
                }
                accum ++;
            }
        }
        if(success) printf("Filter (Classical) on %d elems +   VALID RESULT of size: %d accum: %d!\n\n", num_elems, filt_size, accum);
        else        printf("Filter (Classical) on %d elems + INVALID RESULT of size: %d accum: %d!\n\n", num_elems, filt_size, accum);
    }

    // cleanup memory
    free(h_in );
    free(h_out);

    return 0;
}

template<class ModN>
int testSeqMultiFilter(int* h_in, int* h_out, int num_elems) {
    struct timeval t_start, t_end, t_diff;
    unsigned long int elapsed;

    gettimeofday(&t_start, NULL);

    unsigned int accum[ModN::cardinal];
    for(int i=0; i<ModN::cardinal; i++)
        accum[i] = 0;

    for(int i=0; i<num_elems; i++) {
        int cur_el = h_in[i];
        int ind_cls= ModN::apply(cur_el);
        accum[ind_cls]++;
    }

    unsigned int tmp_cur = 0, tmp_next = 0;
    for(int k=0; k<ModN::cardinal; k++) {
        tmp_next += accum[k];
        accum[k] = tmp_cur;
        tmp_cur  = tmp_next; 
    }
    unsigned int count[ModN::cardinal];
    for(int i=0; i<ModN::cardinal; i++)
        count[i] = 0;

    for(int i=0; i<num_elems; i++) {
        // for segmented scan exclusive test
        int in_el   = h_in[i];
        int ind_cls = ModN::apply(in_el);
        h_out[count[ind_cls]+accum[ind_cls]] = in_el;
        count[ind_cls]++;
    }

    gettimeofday(&t_end, NULL);
    timeval_subtract(&t_diff, &t_end, &t_start);
    elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec); 
    printf("Sequential Multi Filter runs in: %lu microsecs\n", elapsed);
    return elapsed;
}

template<class ModN>
int testMultiFilter(const unsigned int num_elems, const unsigned int num_hwd_thds) {
    unsigned int mem_size = num_elems * sizeof(int);

    int* h_in    = (int*) malloc(mem_size);
    int* h_out   = (int*) malloc(mem_size);

    { // init segments and flags
        std::srand(33); // use current time as seed for random generator
        for(unsigned int i=0; i<num_elems; i++) {
            h_in[i] = std::rand(); 
        }
    }

    int *d_in, *d_out;
    { // device allocation and copyin
        cudaMalloc((void**)&d_in ,   mem_size);
        cudaMalloc((void**)&d_out,   mem_size);
        cudaMemcpy(d_in, h_in, mem_size, cudaMemcpyHostToDevice);
        cudaThreadSynchronize();
    }
    
    /******************************************/
    /**** Invoke MultiFilter Host Skeleton ****/
    /******************************************/
    typename ModN::ExpType filt_size = multiFilter<ModN>( num_elems, num_hwd_thds, d_in, d_out );
    //MyInt4 filt_size = multiFilter<Mod4>( num_elems, num_hwd_thds, d_in, d_out );

    { // device-host copy of the result, and deallocation
        cudaMemcpy(h_out, d_out, mem_size, cudaMemcpyDeviceToHost);
        cudaThreadSynchronize();
        cudaFree(d_in );
        cudaFree(d_out);
    }

    { // validation
        bool  success = true;
        unsigned int accum[ModN::cardinal];
        for(int i=0; i<ModN::cardinal; i++)
            accum[i] = 0;

        for(int i=0; i<num_elems; i++) {
            int cur_el = h_in[i];
            int ind_cls= ModN::apply(cur_el);
            accum[ind_cls]++;
        }
        
        bool sizes_ok = true;
        for(int i=0; i<ModN::cardinal; i++) {
            int sz = ((int*)(&filt_size))[i];
            if ( sz != accum[i] ) {
                sizes_ok = false;
                printf( "Invalid Size #%d, computed: %d, should be: %d!!! EXITING!\n\n", i, sz, accum[i]);
            }
        }
        if(!sizes_ok) exit(0);

#if 0
        if( filt_size.x != accum[0] || filt_size.y != accum[1] || 
            filt_size.z != accum[2] || filt_size.w != accum[3]  ) {
            printf( "Invalid Sizes: (x: (%d,%d), y: (%d,%d), z: (%d,%d), w: (%d,%d))! EXITING!\n",
                    filt_size.x, accum[0], filt_size.y, accum[1], filt_size.z, accum[2], filt_size.w, accum[3]);
            exit(0);
        } else {
            //printf("Valid Sizes: (%d,%d,%d,%d)\n", accum[0], accum[1], accum[2], accum[3]);
        }
#endif
        unsigned int tmp_cur = 0, tmp_next = 0;
        for(int k=0; k<ModN::cardinal; k++) {
            tmp_next += accum[k];
            accum[k] = tmp_cur;
            tmp_cur  = tmp_next; 
        }

        unsigned int count[ModN::cardinal];
        for(int i=0; i<ModN::cardinal; i++)
            count[i] = 0;

        for(int i=0; i<num_elems; i++) {
            // for segmented scan exclusive test
            int in_el   = h_in[i];
            int ind_cls = ModN::apply(in_el);
            int out_el  = h_out[count[ind_cls]+accum[ind_cls]];
            if ( out_el != in_el) {
                success = false;
                printf("Multi Filter Violation: %d should be %d, eq class: %d, i: %d, h_out[i]: %d\n", out_el, in_el, ind_cls, i, h_out[i]);
                if(i > 9) break;
            }
            count[ind_cls]++;
        }
        if(success) printf( "Multi Filter on %d elems +   VALID RESULT of partition sizes: (%d,%d,%d,%d)!\n\n", 
                            num_elems, filt_size.x, filt_size.y, filt_size.z, filt_size.w);
        else        printf( "Multi Filter on %d elems + INVALID RESULT of partition sizes: (%d,%d,%d,%d)!\n\n", 
                            num_elems, filt_size.x, filt_size.y, filt_size.z, filt_size.w);
    }

    testSeqMultiFilter<ModN>(h_in, h_out, num_elems);

    // cleanup memory
    free(h_in );
    free(h_out);

    return 0;
}


int main(int argc, char** argv) { 
    const unsigned int num_hwd_thds = 32*1024;
    const unsigned int num_elems = 50332001; //50332001; //51904512; //50332001; //50331648; //16353455;  // 65535 * 512

    testClassicFilter(num_elems, num_hwd_thds, 1);
    testClassicFilter(num_elems, num_hwd_thds, 2);
    testClassicFilter(num_elems, num_hwd_thds, 3);
    testMultiFilter<Mod4>(num_elems, num_hwd_thds);

#if 0
    const unsigned int mssp_list_size = 8353455; 
    const unsigned int matrix_row_num = 11033;
    const unsigned int vct_size       = 2076;
    const unsigned int block_size     = 256;
    MsspProblem(block_size, mssp_list_size);
#endif
}
