#ifndef SPARSE_MAT_VCT
#define SPARSE_MAT_VCT

/**
 * mat_width   is the number of rows of the matrix (input)
 * vct_size    is the input vector size (input)
 * rands       is a temporary array of randoms of size mat_width (output)
 * int Result  is the total number of elements of to-be-constructed matrix
 */
int getTotSizeAndRands(int mat_width, int vct_size, int* rands) {
    int tot_size = 0;

    for(int i=0; i<mat_width; i++) {
        int r = 1 + (rand() % (vct_size-1));
        rands[i] = r;
        tot_size += r ;
    }    

    return tot_size;
}

/**
 * vct_size   is the size of the vector (input)
 * width      is the number of rows of the matrix (input)
 * tot_size   is the total number of elements of the matrix (input)
 * rands      is an array of randoms in [0,mat_width) (input)
 * vct        is the input vector (output)
 * flags      is the flag array of the matrix, i.e., 
 *               each segment denotes a row of the matrix. (output)
 * mat_vals   is the matrix values in flat form (output)
 * mat_inds   is the matrix-column indices in flat form (output),
 *               i.e., each column index corresponds to a matrix value,
 *               and `zip mat_inds mat_vals' is the sparse matrix.
 */
void mkVactDataAndFlags(
        int     vct_size,  int   mat_width,  
        int     tot_size,  int*  rands,  
        float*  vct,       int*  flags,     
        float*  mat_vals,  int*  mat_inds
) {
    for(int i=0; i<vct_size; i++) {
        vct[i] = (rand() / (float)RAND_MAX)*5.0;
    }
    int ind = 0;
    for(int i=0; i<tot_size; i+=rands[ind], ind++ ) {
        for(int j=0; j < rands[ind]; j++) {
            flags[i+j] = (j==0) ? 1 : 0;
            mat_inds[i+j] = j;
            mat_vals[i+j] = (rand() / (float)RAND_MAX)*10.0;
        }
    }
}


void seqSparseMatVctMult( int    tot_size, int  mat_width,
                          float* vct,      int* flags,    
                          float* mat_vals, int* mat_inds, 
                          float* res_vct
) {
    int   ind   = 0;
    float accum = mat_vals[0]*vct[mat_inds[0]];
    for(int i=1; i<tot_size; i++) {
        if (flags[i] != 0) {
            res_vct[ind] = accum;
            accum = mat_vals[i]*vct[mat_inds[i]];
            ind ++;
        } else {
            accum += mat_vals[i]*vct[mat_inds[i]];
        }
    }
    res_vct[ind] = accum;
}

int SparseMatVctMult(int block_size, int mat_rows, int vct_size) {
    int* rands   = (int*)malloc(mat_rows*sizeof(int));
    int tot_size = getTotSizeAndRands(mat_rows, vct_size, rands);

    printf("Vect_size: %d, tot_size: %d mat_rows: %d\n", vct_size, tot_size, mat_rows);

    float *vct, *mat_vals, *vct_res1, *vct_res2;
    int   *flags, *mat_inds;
    vct       = (float*)malloc(vct_size*sizeof(float));
    flags     = (int  *)malloc(tot_size*sizeof(int  ));
    mat_inds  = (int  *)malloc(tot_size*sizeof(int  ));
    mat_vals  = (float*)malloc(tot_size*sizeof(float));

    vct_res1  = (float*)malloc(mat_rows*sizeof(float));
    vct_res2  = (float*)malloc(mat_rows*sizeof(float));

    mkVactDataAndFlags( vct_size, mat_rows, tot_size, rands,
                        vct,      flags,    mat_vals, mat_inds );

    seqSparseMatVctMult( tot_size, mat_rows, vct, 
                         flags, mat_vals, mat_inds, vct_res1 );

#if 1
    { // GPU execution    
        int   *flags_d, *mat_inds_d;
        float *vct_d,   *mat_vals_d, *res_vct_d;
        cudaMalloc((void**)&flags_d,    tot_size*sizeof(int  ));
        cudaMalloc((void**)&mat_inds_d, tot_size*sizeof(int  ));

        cudaMalloc((void**)&mat_vals_d, tot_size*sizeof(float));
        cudaMalloc((void**)&vct_d,      vct_size*sizeof(float));
        cudaMalloc((void**)&res_vct_d,  mat_rows*sizeof(float));

        // other temporaries:
        float *tmp_pairs, *tmp_scan;
        int  * tmp_inds;

        cudaMalloc((void**)&tmp_pairs,  tot_size*sizeof(float));
        cudaMalloc((void**)&tmp_scan,   tot_size*sizeof(float));
        cudaMalloc((void**)&tmp_inds,   tot_size*sizeof(int  ));


        int num_blocks = ( (tot_size % block_size) == 0) ?
                            tot_size / block_size     :
                            tot_size / block_size + 1 ;
        
        unsigned long int elapsed;
        struct timeval t_start, t_end, t_diff;
        gettimeofday(&t_start, NULL); 


        { // copy-in stage
            cudaMemcpy(flags_d,    flags,    tot_size*sizeof(int),   cudaMemcpyHostToDevice);
            cudaMemcpy(mat_inds_d, mat_inds, tot_size*sizeof(int),   cudaMemcpyHostToDevice);
            cudaMemcpy(mat_vals_d, mat_vals, tot_size*sizeof(float), cudaMemcpyHostToDevice);
            cudaMemcpy(vct_d,      vct,      vct_size*sizeof(float), cudaMemcpyHostToDevice);
        }

        { // Kernels
            // 1. mutiply each matrix element with its corresponding
            //    vector element, i.e., the flat map from the Haskell code
            mult_pairs<<< num_blocks, block_size >>>(mat_inds_d, mat_vals_d, vct_d, tot_size, tmp_pairs);
            cudaThreadSynchronize();
 
            // 2. perform a segmented scan, such as the last element 
            //    of each segment has the value of the corresponding 
            //    result-vector index.
            sgmScanInc< Add<float>, float > ( block_size, tot_size, tmp_pairs, flags_d, tmp_scan );

            // 3. scan inclusive on flags computes the indexes (+1) in the result
            //    vector where each of the last element of a segment needs to be placed.
            scanInc< Add<int>, int > ( block_size, tot_size, flags_d, tmp_inds );

            // 4. finally, place the last element of each segment in the
            //    corresponding index in the result vector
            write_lastsgm<<< num_blocks, block_size >>>(tmp_scan, tmp_inds, flags_d, tot_size, res_vct_d);
            cudaThreadSynchronize();
        }

        { // copy out stage:
            cudaMemcpy(vct_res2, res_vct_d, mat_rows*sizeof(float), cudaMemcpyDeviceToHost);
        }

        cudaFree(flags_d);    cudaFree(mat_inds_d);  cudaFree(vct_d);
        cudaFree(mat_vals_d); cudaFree(res_vct_d);   cudaFree(tmp_pairs);
        cudaFree(tmp_scan);   cudaFree(tmp_inds);


        gettimeofday(&t_end, NULL);
        timeval_subtract(&t_diff, &t_end, &t_start);
        elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec); 
        printf("GPU Sparse Matrix-Vector Multiplication runs in: %lu microsecs\n", elapsed);
    }
#endif
    {// validation
        bool valid = true;
        for(int i=0; i<mat_rows; i++) {
            float res1 = fabs(vct_res1[i]);
            float res2 = fabs(vct_res2[i]);
            if(fabs(res1 - res2) > 0.1) {
                printf("ERROR at result vector index %d (cpu,gpu): (%6.5f, %6.5f)\n", i, res1, res2);
                valid = false;
                break;
            }
        }
        if(valid) printf("Sparse Mat-Vect Mult VALID   RESULT\n");
        else      printf("Sparse Mat-Vect Mult INVALID RESULT\n");
    }

    { // free space
        free(rands);    
        free(vct);  
        free(flags);   
        free(mat_inds);  
        free(mat_vals); 
        free(vct_res1);  
        free(vct_res2);
    }
    return 1;
}

#endif //SPARSE_MAT_VCT

