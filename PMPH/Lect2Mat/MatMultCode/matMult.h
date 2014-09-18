#ifndef MULT_H
#define MULT_H

#include <math.h>
#include <stdio.h>

//void matMult(float* A, float* B, float* C, int widthA, int colsB);
//bool isomorphic(float* A, float* B);

void matMult(float* A, float* B, float* C, int colsA, int rowsA, int colsB) {
  for(int i = 0; i < rowsA; i++) {
    for(int j = 0; j < colsB; j++) {
      float sum = 0.0;
      for(int k = 0; k < colsA; k++) {
        sum += A[i*colsA + k] * B[k * colsB + j];
      }
      C[i * colsB + j] = sum;
    }
  } 
  return;
}

bool isomorphic(float* A,float* B, int sizeAB){
    for(int i = 0; i < sizeAB; i++)
      if (fabs(A[i] - B[i]) > 0.0005) 
        return false;

    return true;
}

#endif

/*
// Multiply two matrices A * B = C
 
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <matrixMul_kernel.cu>
 
// Allocates a matrix with random float entries.
void randomInit(float* data, int size)
{
   for (int i = 0; i < size; ++i)
   data[i] = rand() / (float)RAND_MAX;
}
 
/////////////////////////////////////////////////////////
// Program main
/////////////////////////////////////////////////////////
 
int
main(int argc, char** argv)
{

   // set seed for rand()
   srand(2006);
 
   // 1. allocate host memory for matrices A and B
   unsigned int size_A = WA * HA;
   unsigned int mem_size_A = sizeof(float) * size_A;
   float* h_A = (float*) malloc(mem_size_A);
 
   unsigned int size_B = WB * HB;
   unsigned int mem_size_B = sizeof(float) * size_B;
   float* h_B = (float*) malloc(mem_size_B);
 
   // 2. initialize host memory
   randomInit(h_A, size_A);
   randomInit(h_B, size_B);
    
   // 8. allocate device memory
   float* d_A;
   float* d_B;
   cudaMalloc((void**) &d_A, mem_size_A);
   cudaMalloc((void**) &d_B, mem_size_B);
 
   // 9. copy host memory to device
   cudaMemcpy(d_A, h_A, mem_size_A, 
   cudaMemcpyHostToDevice);
   cudaMemcpy(d_B, h_B, mem_size_B, 
   cudaMemcpyHostToDevice);
 
   // 4. allocate host memory for the result C
   unsigned int size_C = WC * HC;
   unsigned int mem_size_C = sizeof(float) * size_C;
   float* h_C = (float*) malloc(mem_size_C);
 
   // 10. allocate device memory for the result
   float* d_C;
   cudaMalloc((void**) &d_C, mem_size_C);
 
   // 5. perform the calculation
   // setup execution parameters
   dim3 threads(BLOCK_SIZE, BLOCK_SIZE);
   dim3 grid(WC / threads.x, HC / threads.y);
 
   // execute the kernel
   matrixMul<<< grid, threads >>>(d_C, d_A, 
                                  d_B, WA, WB);
 
   // 11. copy result from device to host
   cudaMemcpy(h_C, d_C, mem_size_C, 
   cudaMemcpyDeviceToHost);
  
   // 7. clean up memory
   free(h_A);
   free(h_B);
   free(h_C);
   cudaFree(d_A);
   cudaFree(d_B);
   cudaFree(d_C);
 
}

///////////////////////////////////////////
//! Matrix multiplication on the device: C = A * B
//! wA is A's width and wB is B's width
//////////////////////////////////////////////////////
__global__ void
matrixMul( float* C, float* A, float* B, int wA, int wB)
{
    // Block index
    int bx = blockIdx.x;
    int by = blockIdx.y;
 
    // Thread index
    int tx = threadIdx.x;
    int ty = threadIdx.y;
 
    // Index of the first sub-matrix of A processed 
    // by the block
    int aBegin = wA * BLOCK_SIZE * by;
 
    // Index of the last sub-matrix of A processed 
    // by the block
    int aEnd   = aBegin + wA - 1;
 
    // Step size used to iterate through the 
    // sub-matrices of A
    int aStep  = BLOCK_SIZE;
 
    // Index of the first sub-matrix of B processed 
    // by the block
    int bBegin = BLOCK_SIZE * bx;
 
    // Step size used to iterate through the 
    // sub-matrices of B
    int bStep  = BLOCK_SIZE * wB;
 
    // Loop over all the sub-matrices of A and B
    // required to compute the block sub-matrix
    for (int a = aBegin, b = bBegin;
             a <= aEnd;
             a += aStep, b += bStep) 
    {

        // Declaration of the shared memory array As 
        // used to store the sub-matrix of A
        __shared__ float As[BLOCK_SIZE][BLOCK_SIZE];
 
        // Declaration of the shared memory array Bs 
        // used to store the sub-matrix of B
        __shared__ float Bs[BLOCK_SIZE][BLOCK_SIZE];
 
        // Load the matrices from global memory
        // to shared memory; each thread loads
        // one element of each matrix
        As[ty][tx] = A[a + wA * ty + tx];
        Bs[ty][tx] = B[b + wB * ty + tx];
    }
}
*/
