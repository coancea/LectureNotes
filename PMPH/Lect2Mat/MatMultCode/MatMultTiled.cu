#include <stdlib.h>
#include <stdio.h>
#include <ctime>
#include <cstdlib>
#include "matMult.h"

/* 12 registers reported when using the --ptxas-options=-v flag for the nvcc compiler, which is
 * consitent with the 12 named variables in the code, excluding the parameters of the method.
 */
template <int BLOCK_SIZE>
__global__ void d_mult(float* A, float* B, float* C, int widthA, int widthB) {

  float CTemp = 0.0f;

  int bx = blockIdx.x;
  int by = blockIdx.y;

  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int tempReg = widthA * BLOCK_SIZE;

  // Iterator outerloop
  int a = tempReg * by;
  
  // Iterator outloop
  int b = BLOCK_SIZE * bx;

  tempReg = widthA - 1;
  int aEnd = a + tempReg;

  __shared__ float As[BLOCK_SIZE * BLOCK_SIZE];
  __shared__ float Bs[BLOCK_SIZE * BLOCK_SIZE];

  for( ;a <= aEnd; a += BLOCK_SIZE) {

    float as;
    float bs;
    tempReg = widthA * ty;
    tempReg = tempReg + tx;
    tempReg = a + tempReg;
    as = A[tempReg];

    tempReg = BLOCK_SIZE * ty;
    tempReg = tempReg + tx;
    As[tempReg] = as;

    tempReg = widthB * ty;
    tempReg = tempReg + tx;
    tempReg = b + tempReg;
    bs = B[tempReg];

    tempReg = BLOCK_SIZE * ty;
    tempReg = tempReg + tx;
    Bs[tempReg] = bs;

    tempReg = BLOCK_SIZE * widthB;
    b = b + tempReg;
    __syncthreads();

    //innerloop unrolled
    //iteration 0
    int indexReg = BLOCK_SIZE * ty;
    tempReg = tx;
    as = As[indexReg];
    bs = Bs[tempReg];
    as = as * bs;
    CTemp = CTemp + as;

    //iteration 1
    indexReg = indexReg + 1;
    tempReg = tempReg + BLOCK_SIZE;
    as = As[indexReg];
    bs = Bs[tempReg];
    as = as * bs;
    CTemp = CTemp + as;

    //iteration 2
    indexReg = indexReg + 1;
    tempReg = tempReg + BLOCK_SIZE;
    as = As[indexReg];
    bs = Bs[tempReg];
    as = as * bs;
    CTemp = CTemp + as;
    
    //iteration 3
    indexReg = indexReg + 1;
    tempReg = tempReg + BLOCK_SIZE;
    as = As[indexReg];
    bs = Bs[tempReg];
    as = as * bs;
    CTemp = CTemp + as;
    
    //iteration 4
    indexReg = indexReg + 1;
    tempReg = tempReg + BLOCK_SIZE;
    as = As[indexReg];
    bs = Bs[tempReg];
    as = as * bs;
    CTemp = CTemp + as;
    
    //iteration 5
    indexReg = indexReg + 1;
    tempReg = tempReg + BLOCK_SIZE;
    as = As[indexReg];
    bs = Bs[tempReg];
    as = as * bs;
    CTemp = CTemp + as;
    
    //iteration 6
    indexReg = indexReg + 1;
    tempReg = tempReg + BLOCK_SIZE;
    as = As[indexReg];
    bs = Bs[tempReg];
    as = as * bs;
    CTemp = CTemp + as;
    
    //iteration 7
    indexReg = indexReg + 1;
    tempReg = tempReg + BLOCK_SIZE;
    as = As[indexReg];
    bs = Bs[tempReg];
    as = as * bs;
    CTemp = CTemp + as;
    
    //iteration 8
    indexReg = indexReg + 1;
    tempReg = tempReg + BLOCK_SIZE;
    as = As[indexReg];
    bs = Bs[tempReg];
    as = as * bs;
    CTemp = CTemp + as;
    
    //iteration 9
    indexReg = indexReg + 1;
    tempReg = tempReg + BLOCK_SIZE;
    as = As[indexReg];
    bs = Bs[tempReg];
    as = as * bs;
    CTemp = CTemp + as;
    
    //iteration 10
    indexReg = indexReg + 1;
    tempReg = tempReg + BLOCK_SIZE;
    as = As[indexReg];
    bs = Bs[tempReg];
    as = as * bs;
    CTemp = CTemp + as;
    
    //iteration 11
    indexReg = indexReg + 1;
    tempReg = tempReg + BLOCK_SIZE;
    as = As[indexReg];
    bs = Bs[tempReg];
    as = as * bs;
    CTemp = CTemp + as;
    
    //iteration 12
    indexReg = indexReg + 1;
    tempReg = tempReg + BLOCK_SIZE;
    as = As[indexReg];
    bs = Bs[tempReg];
    as = as * bs;
    CTemp = CTemp + as;
    
    //iteration 13
    indexReg = indexReg + 1;
    tempReg = tempReg + BLOCK_SIZE;
    as = As[indexReg];
    bs = Bs[tempReg];
    as = as * bs;
    CTemp = CTemp + as;
    
    //iteration 14
    indexReg = indexReg + 1;
    tempReg = tempReg + BLOCK_SIZE;
    as = As[indexReg];
    bs = Bs[tempReg];
    as = as * bs;
    CTemp = CTemp + as;
    
    //iteration 15
    indexReg = indexReg + 1;
    tempReg = tempReg + BLOCK_SIZE;
    as = As[indexReg];
    bs = Bs[tempReg];
    as = as * bs;
    CTemp = CTemp + as;
    __syncthreads();
  }
  tempReg = widthB * BLOCK_SIZE;
  tempReg = tempReg * by;
  aEnd = BLOCK_SIZE * bx;
  tempReg = aEnd + tempReg;
  aEnd = widthB * ty;
  tempReg = aEnd + tempReg;
  tempReg = tempReg + tx;
  C[tempReg] = CTemp;
}


const unsigned int NUM_ROWS_A = 1024; //4096;
const unsigned int NUM_COLS_A = 2048; //4096;

const unsigned int NUM_ROWS_B = 2048; //4096;
const unsigned int NUM_COLS_B = 1024; //4096;

const unsigned int TILE = 16;

int main() {

  dim3 threads(TILE, TILE);
  dim3 grid(NUM_ROWS_A / threads.x, NUM_COLS_B / threads.y);
  //dim3 blockDim(TILE,TILE,1);


  srand(static_cast <unsigned> (time(0)));
  //Allocate host memory
  float* h_A = (float *) malloc(NUM_ROWS_A * NUM_COLS_A * sizeof(float));
  float* h_B = (float *) malloc(NUM_ROWS_B * NUM_COLS_B * sizeof(float));
  float* h_C = (float *) calloc(NUM_ROWS_A * NUM_COLS_B, sizeof(float));
  float* h_D = (float *) calloc(NUM_ROWS_A * NUM_COLS_B, sizeof(float));
  float* d_A, *d_B, *d_C;

  //Reserve device memory
  cudaMalloc((void **) &d_A, NUM_ROWS_A * NUM_COLS_A*sizeof(float));
  cudaMalloc((void **) &d_B, NUM_ROWS_B * NUM_COLS_B*sizeof(float));
  cudaMalloc((void **) &d_C, NUM_ROWS_A * NUM_COLS_B*sizeof(float));

  // initializing matrices A and B
  for(int i = 0; i < (NUM_ROWS_A * NUM_COLS_A); i++) {
    h_A[i] = static_cast <float> (rand()) / static_cast <float> (RAND_MAX);
  }
  for(int i = 0; i < (NUM_ROWS_B * NUM_COLS_B); i++) {
    h_B[i] = static_cast <float> (rand()) / static_cast <float> (RAND_MAX);
  }

  matMult(h_A,h_B,h_D,NUM_ROWS_A, NUM_COLS_A, NUM_COLS_B);
  cudaError_t error;
  error = cudaMemcpy(d_A,h_A,NUM_ROWS_A*NUM_COLS_A*sizeof(float),cudaMemcpyHostToDevice);
  if(error != cudaSuccess) {
    printf("1");
    exit(EXIT_FAILURE);
  }
  error = cudaMemcpy(d_B,h_B,NUM_ROWS_B*NUM_COLS_B*sizeof(float),cudaMemcpyHostToDevice);
  if(error != cudaSuccess) {
    printf("1");
    exit(EXIT_FAILURE);
  }
//  error = cudaMemcpy(d_C,h_C,NUM_ROWS_A*NUM_COLS_B*sizeof(float),cudaMemcpyHostToDevice);
//  if(error != cudaSuccess) {
//    printf("1");
//    exit(EXIT_FAILURE);
//  }

//  d_mult<16><<<32768,blockDim>>>(d_A,d_B,d_C,NUM_ROWS_A,NUM_COLS_A);
//  d_mult<TILE><<<grid,threads>>>(d_A,d_B,d_C,NUM_ROWS_A,NUM_COLS_A);
//  cudaMemcpy(h_C,d_C,NUM_ROWS_A*NUM_COLS_B*sizeof(float),cudaMemcpyDeviceToHost);

    cudaEvent_t start;
    cudaEvent_t stop;
    error = cudaEventCreate(&start);
    if(error != cudaSuccess) { printf("1"); exit(EXIT_FAILURE); }
    error = cudaEventCreate(&stop);
    if(error != cudaSuccess) { printf("2"); exit(EXIT_FAILURE); }

    error = cudaEventRecord(start, NULL);
    if(error != cudaSuccess) { printf("3"); exit(EXIT_FAILURE); }


    float msecTotal = 0.0f;
    int nIter = 8;
    for(int i = 0; i < nIter; i++) {  
        d_mult<TILE><<<grid,threads>>>(d_A,d_B,d_C,NUM_COLS_A,NUM_COLS_B);
//      d_mult<16><<<32768,blockDim>>>(d_A,d_B,d_C,4096,4096);

//      cudaDeviceSynchronize();
    }

    error = cudaEventRecord(stop, NULL); 
    if(error != cudaSuccess) { printf("4"); exit(EXIT_FAILURE); }

    // Wait for the stop event to complete
    error = cudaEventSynchronize(stop);
    if(error != cudaSuccess) { printf("4"); exit(EXIT_FAILURE); }
    
    error = cudaEventElapsedTime(&msecTotal, start, stop);
    if(error != cudaSuccess) { printf("5"); exit(EXIT_FAILURE); }


  cudaMemcpy(h_C,d_C,NUM_ROWS_A*NUM_COLS_B*sizeof(float),cudaMemcpyDeviceToHost);
  cudaDeviceSynchronize();

  bool correct = isomorphic(h_C,h_D,NUM_ROWS_A*NUM_COLS_B);
  if (correct) {

      float msecPerMatrixMul = msecTotal / nIter; 
      double flopsPerMatrixMul = 2.0 * NUM_ROWS_A*NUM_COLS_B*NUM_COLS_A; 
      double gigaFlops = (flopsPerMatrixMul * 1.0e-9f) / (msecPerMatrixMul / 1000.0f); 
      printf( "Verified Multiplication! Performance= %.2f GFlop/s, Time= %.3f msec\n", gigaFlops, msecPerMatrixMul); 
  } else {
    printf("Incorrect Result!!!!\n");
  }


  cudaFree(d_A);
  cudaFree(d_B);
  cudaFree(d_C);
  free(h_A); 
  free(h_B); 
  free(h_C); 
  free(h_D); 
  return 0;
}
