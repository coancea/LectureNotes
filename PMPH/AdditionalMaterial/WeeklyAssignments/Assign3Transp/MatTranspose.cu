#include "MatTranspose.h"
#include "MatTranspose.cu.h"

#define HEIGHT_A 1024*8   //12835//2048//2048
#define  WIDTH_A 1024*8  //15953 //1024//2048
#define TILE     32

template<class T, int tile>
void transpose( float*             inp_d,  
                float*             out_d, 
                const unsigned int height, 
                const unsigned int width
) {
   // 1. setup block and grid parameters
   int  dimy = ceil( ((float)height)/tile ); 
   int  dimx = ceil( ((float) width)/tile );
   dim3 block(tile, tile, 1);
   dim3 grid (dimx, dimy, 1);
 
   //2. execute the kernel
   //matTransposeTiledKer<float,tile> <<< grid, block >>>
   //                    (inp_d, out_d, height, width); 
   matTransposeKer<float> <<< grid, block >>>
                       (inp_d, out_d, height, width);
   
    cudaThreadSynchronize();
}

#define VERSION 3

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

#if (VERSION == 1)
   {
        unsigned long int elapsed;
        struct timeval t_start, t_end, t_diff;
        gettimeofday(&t_start, NULL); 

        //transpose<float, TILE>( d_A, d_B, HEIGHT_A, WIDTH_A );
        transpose<float, TILE>( d_A, d_B, HEIGHT_A, WIDTH_A );

        gettimeofday(&t_end, NULL);
        timeval_subtract(&t_diff, &t_end, &t_start);
        elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec); 
        printf("Transpose on GPU runs in: %lu microsecs\n", elapsed);

        // copy result from device to host
        cudaMemcpy(h_B, d_B, mem_size_A, cudaMemcpyDeviceToHost);
  
        // 12. validate
        //validateTranspose<float>( h_A, h_B, HEIGHT_A, WIDTH_A );
        validateTranspose<float>( h_A, h_B, HEIGHT_A, WIDTH_A );
   }
#elif (VERSION == 2)
   { // compute original program
      unsigned long int elapsed;
      struct timeval t_start, t_end, t_diff;
      gettimeofday(&t_start, NULL); 

//      transpose<float, TILE>( d_A, d_B, HEIGHT_A, WIDTH_A );
      unsigned int num_thds= (HEIGHT_A/64)*WIDTH_A;
      unsigned int block   = 256;
      unsigned int grid    = num_thds / block;

      origProg<<<grid,block>>>(d_A, d_B, num_thds);
      cudaThreadSynchronize();

      gettimeofday(&t_end, NULL);
      timeval_subtract(&t_diff, &t_end, &t_start);
      elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec); 
      printf("Original Program on GPU runs in: %lu microsecs\n", elapsed);

      // copy result from device to host
      cudaMemcpy(h_B, d_B, mem_size_A, cudaMemcpyDeviceToHost);

      validateProgram(h_A, h_B, num_thds);
   }
#elif (VERSION == 3)
    { // compute transformed program

        float* d_Atr;   cudaMalloc((void**) &d_Atr, mem_size_A);
        float* d_Btr;   cudaMalloc((void**) &d_Btr, mem_size_A);

        unsigned long int elapsed;
        struct timeval t_start, t_end, t_diff;
        gettimeofday(&t_start, NULL); 

        unsigned int num_thds= (HEIGHT_A/64)*WIDTH_A;
        unsigned int block   = 256;
        unsigned int grid    = num_thds / block;

        transpose<float, TILE>( d_A, d_Atr, num_thds, 64 );

        transfProg<<<grid,block>>>(d_Atr, d_Btr, num_thds);
        cudaThreadSynchronize();

        transpose<float, TILE>( d_Btr, d_B, 64, num_thds );

        gettimeofday(&t_end, NULL);
        timeval_subtract(&t_diff, &t_end, &t_start);
        elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec); 
        printf("Transformed Program on GPU runs in: %lu microsecs\n", elapsed);

        // copy result from device to host
        cudaMemcpy(h_B, d_B, mem_size_A, cudaMemcpyDeviceToHost);
        validateProgram(h_A, h_B, num_thds);

        cudaFree(d_Atr);
        cudaFree(d_Btr);
   }
#endif

   // clean up memory
   free(h_A);
   free(h_B);
   cudaFree(d_A);
   cudaFree(d_B);
}

