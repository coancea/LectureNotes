#include "MatrixMult.h"
#include "MatrixMult.cu.h"

#define WIDTH_A  1024//1024 //1024//2048
#define HEIGHT_A 1024//2048//2048//2048
#define WIDTH_B  1024//1536//4096//2048
#define TILE     32
/////////////////////////////////////////////////////////
// Program main
/////////////////////////////////////////////////////////
 
int main() {
   // set seed for rand()
   srand(2006);
 
   // 1. allocate host memory for the two matrices
   unsigned int size_A = WIDTH_A * HEIGHT_A;
   unsigned int mem_size_A = sizeof(float) * size_A;
   float* h_A = (float*) malloc(mem_size_A);
 
   unsigned int size_B = WIDTH_B * WIDTH_A;
   unsigned int mem_size_B = sizeof(float) * size_B;
   float* h_B = (float*) malloc(mem_size_B);
 
   // 2. initialize host memory
   randomInit(h_A, size_A);
   randomInit(h_B, size_B);
    
   // 3. allocate device memory
   float* d_A;
   float* d_B;
   cudaMalloc((void**) &d_A, mem_size_A);
   cudaMalloc((void**) &d_B, mem_size_B);
 
   // 4. copy host memory to device
   cudaMemcpy(d_A, h_A, mem_size_A, cudaMemcpyHostToDevice);
   cudaMemcpy(d_B, h_B, mem_size_B, cudaMemcpyHostToDevice);
 
   // 5. allocate host memory for the result C
   unsigned int size_C = HEIGHT_A * WIDTH_B;
   unsigned int mem_size_C = sizeof(float) * size_C;
   float* h_C   = (float*) malloc(mem_size_C);
   float* seq_C = (float*) malloc(mem_size_C);
 
   // 6. allocate device memory for the result
   float* d_C;
   cudaMalloc((void**) &d_C, mem_size_C);
 
   // 7. compute sequential matrix multiplication
   {
      unsigned long int elapsed;
      struct timeval t_start, t_end, t_diff;
      gettimeofday(&t_start, NULL); 
      
      matMult<float>(h_A, h_B, seq_C, WIDTH_A, HEIGHT_A, WIDTH_B);

      gettimeofday(&t_end, NULL);
      timeval_subtract(&t_diff, &t_end, &t_start);
      elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec); 
      printf("Sequential Naive version runs in: %lu microsecs\n", elapsed);
   }

   // 8. perform the calculation
   // setup execution parameters
   int  dimy = ceil( ((float)HEIGHT_A)/TILE ); 
   int  dimx = ceil( ((float) WIDTH_B)/TILE );
   dim3 block(TILE, TILE, 1);
   dim3 grid (dimx, dimy, 1);
 
   // execute the kernel
   {
      unsigned long int elapsed;
      struct timeval t_start, t_end, t_diff;
      gettimeofday(&t_start, NULL); 
      
        //matMultKer<float> <<< grid, block >>>(d_A, d_B, d_C, WIDTH_A, HEIGHT_A, WIDTH_B);
        matMultTiledKer<float,TILE> <<< grid, block >>>(d_A, d_B, d_C, WIDTH_A, HEIGHT_A, WIDTH_B); 
        //matMultCacheKer<float,TILE> <<< grid, block >>>(d_A, d_B, d_C, WIDTH_A, HEIGHT_A, WIDTH_B);
        cudaThreadSynchronize();

      gettimeofday(&t_end, NULL);
      timeval_subtract(&t_diff, &t_end, &t_start);
      elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec); 
      printf("GPU version runs in: %lu microsecs\n", elapsed);

      float microsecPerMatrixMul = elapsed; 
      double flopsPerMatrixMul = 2.0 * HEIGHT_A * WIDTH_B * WIDTH_A; 
      double gigaFlops = (flopsPerMatrixMul * 1.0e-9f) / (microsecPerMatrixMul / (1000.0f * 1000.0f)); 
      printf( "Performance= %.2f GFlop/s, Time= %.3f microsec %d %d\n", gigaFlops, microsecPerMatrixMul, grid.x, grid.y); 
   }

   // 11. copy result from device to host
   cudaMemcpy(h_C, d_C, mem_size_C, cudaMemcpyDeviceToHost);
  
   // 12. validate
   validate<float>(seq_C, h_C, size_C);
   // 7. clean up memory
   free(h_A);
   free(h_B);
   free(h_C);
   cudaFree(d_A);
   cudaFree(d_B);
   cudaFree(d_C);
}

