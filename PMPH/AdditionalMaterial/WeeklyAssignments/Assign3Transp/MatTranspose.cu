#include "MatTranspose.h"
#include "MatTranspose.cu.h"

#define HEIGHT_A 1024*8//12835//2048//2048
#define WIDTH_A  1024*16//15953 //1024//2048
#define TILE       32
#define BLOCK_ROWS 8

#define WITH_TILE 0


int main() {
   // set seed for rand()
   srand(2006); 
 
   // 1. allocate host memory for the two matrices
   size_t size_A = WIDTH_A * HEIGHT_A;
   size_t mem_size_A = sizeof(float) * size_A;
   float* h_A = (float*) malloc(mem_size_A);
   float* h_B = (float*) malloc(mem_size_A);
 
   // 2. initialize host memory
   randomInit(h_A, size_A);
    
   // 3. allocate device memory
   float* d_A;
   float* d_B;
   cudaMalloc((void**) &d_A, mem_size_A);
   cudaMalloc((void**) &d_B, mem_size_A);
 
   // 4. copy host memory to device
   cudaMemcpy(d_A, h_A, mem_size_A, cudaMemcpyHostToDevice);
   
   // 5. perform the calculation
   //    setup execution parameters
#if 0 //WITH_TILE
   int  dimy = ceil( ((float)HEIGHT_A)/BLOCK_ROWS );
   int  dimx = ceil( ((float) WIDTH_A)/TILE       );
   dim3 block(TILE, BLOCK_ROWS, 1);
   dim3 grid (dimx, dimy, 1);
#else
   int  dimy = ceil( ((float)HEIGHT_A)/TILE ); 
   int  dimx = ceil( ((float) WIDTH_A)/TILE );
   dim3 block(TILE, TILE, 1);
   dim3 grid (dimx, dimy, 1);
#endif

 
   // execute the kernel
   {
      unsigned long int elapsed;
      struct timeval t_start, t_end, t_diff;
      gettimeofday(&t_start, NULL); 
#if WITH_TILE
        //printf("TILED KERNEL!!!\n");
        matTransposeTiledKer<float,TILE/*,BLOCK_ROWS*/> <<< grid, block >>>
                                (d_A, d_B, HEIGHT_A, WIDTH_A); 
#else
        //printf("UNTILED KERNEL!!!\n");
        matTransposeKer<float> <<< grid, block >>>(d_A, d_B, HEIGHT_A, WIDTH_A);
#endif
        cudaThreadSynchronize();

      gettimeofday(&t_end, NULL);
      timeval_subtract(&t_diff, &t_end, &t_start);
      elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec); 
      printf("GPU version runs in: %lu microsecs\n", elapsed);
   }

   // 11. copy result from device to host
   cudaMemcpy(h_B, d_B, mem_size_A, cudaMemcpyDeviceToHost);
  
   // 12. validate
   validate<float>( h_A, h_B, HEIGHT_A, WIDTH_A );
   // 7. clean up memory
   free(h_A);
   free(h_B);
   cudaFree(d_A);
   cudaFree(d_B);
}

