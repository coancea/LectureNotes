#include <stdlib.h>
#include <stdio.h>
#include <ctime>
#include <cstdlib>
#include "matMult.h"

template <int BLOCK_SIZE>
__global__ void d_mult(float* A, float* B, float* C, int widthA, int widthB) {

  float CTemp = 0.0f;

  int tx = threadIdx.x;
  int ty = threadIdx.y;

  int aStart = widthA * BLOCK_SIZE * blockIdx.y;
  int bStart = BLOCK_SIZE * blockIdx.x;

  int aEnd = aStart + widthA - 1;

  for(int a = aStart, b = bStart; a < aEnd;
      a += BLOCK_SIZE, b += BLOCK_SIZE * widthB) {
    
    __shared__ float As[BLOCK_SIZE][BLOCK_SIZE];
    __shared__ float Bs[BLOCK_SIZE][BLOCK_SIZE];

    As[ty][tx] = A[a + widthA * ty + tx];
    Bs[ty][tx] = B[b + widthB * ty + tx];

    __syncthreads();
    for(int i = 0; i < BLOCK_SIZE; ++i) {
      CTemp += As[ty][i] * Bs[i][tx];
    }
    __syncthreads();
  }
  int indexC = widthB * BLOCK_SIZE * blockIdx.y + BLOCK_SIZE * blockIdx.x;
  C[indexC + widthB * ty + tx] = CTemp;
}

int main() {
  dim3 blockDim(16,16,1);
  srand(static_cast <unsigned> (time(0)));
  //Allocate host memory
  float* h_A = (float *) malloc(4096 * 4096 * sizeof(float));
  float* h_B = (float *) malloc(4096 * 4096 * sizeof(float));
  float* h_C = (float *) calloc(4096 * 4096, sizeof(float));
  float* h_D = (float *) calloc(4096 * 4096, sizeof(float));
  float* d_A, *d_B, *d_C;

  //Reserve device memory
  cudaMalloc((void **) &d_A, 4096*4096*sizeof(float));
  cudaMalloc((void **) &d_B, 4096*4096*sizeof(float));
  cudaMalloc((void **) &d_C, 4096*4096*sizeof(float));

  for(int i = 0; i < (4096*4096); i++) {
    h_A[i] = static_cast <float> (rand()) / static_cast <float> (RAND_MAX);
    h_B[i] = static_cast <float> (rand()) / static_cast <float> (RAND_MAX);
  }

  matMult(h_A,h_B,h_D,4096,4096);
  cudaError_t error;
  error = cudaMemcpy(d_A,h_A,4096*4096*sizeof(float),cudaMemcpyHostToDevice);
  if(error != cudaSuccess) {
    printf("1");
    exit(EXIT_FAILURE);
  }
  error = cudaMemcpy(d_B,h_B,4096*4096*sizeof(float),cudaMemcpyHostToDevice);
  if(error != cudaSuccess) {
    printf("1");
    exit(EXIT_FAILURE);
  }
  error = cudaMemcpy(d_C,h_C,4096*4096*sizeof(float),cudaMemcpyHostToDevice);

  if(error != cudaSuccess) {
    printf("1");
    exit(EXIT_FAILURE);
  }
  d_mult<16><<<32768,blockDim>>>(d_A,d_B,d_C,4096,4096);

  cudaMemcpy(h_C,d_C,4096*4096*sizeof(float),cudaMemcpyDeviceToHost);

  bool correct = isomorphic(h_C,h_D);
  int nIter = 300;
  if(correct) {
    cudaEvent_t start;
    error = cudaEventCreate(&start);

    if(error != cudaSuccess) {
      printf("1");
      exit(EXIT_FAILURE);
    }

    cudaEvent_t stop;
    error = cudaEventCreate(&stop);

    if(error != cudaSuccess) {
      printf("2");
      exit(EXIT_FAILURE);
    }

    error = cudaEventRecord(start, NULL);

    if(error != cudaSuccess) {
      printf("3");
      exit(EXIT_FAILURE);
    }

    for(int i = 0; i < nIter; i++) {  
      d_mult<16><<<32768,blockDim>>>(d_A,d_B,d_C,4096,4096);
    }
    
    error = cudaEventRecord(stop, NULL);
    if(error != cudaSuccess) {
      printf("4");
      exit(EXIT_FAILURE);
    }

    // Wait for the stop event to complete
    error = cudaEventSynchronize(stop);

    if(error != cudaSuccess) {
      printf("4");
      exit(EXIT_FAILURE);
    }
    
    float msecTotal = 0.0f;
    error = cudaEventElapsedTime(&msecTotal, start, stop);

    if(error != cudaSuccess) {
    return 1;
    }

    float msecPerMatrixMul = msecTotal / nIter; 
    double flopsPerMatrixMul = 2.0 * 32768 * 512; 
    double gigaFlops = (flopsPerMatrixMul * 1.0e-9f) / (msecPerMatrixMul / 1000.0f); 
    printf( "Performance= %.2f GFlop/s, Time= %.3f msec", gigaFlops, msecPerMatrixMul); 
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
