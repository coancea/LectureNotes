#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>     /* srand, rand */

#include <cuda_runtime.h>

#define THREADS_PER_BLOCK 8


// Blelloch exclusive-sum-scan on one block
__global__ void excl_sum_scan_kernel(float *d_in, float *d_out, float neutral)
{
    const unsigned int tid = threadIdx.x + blockDim.x*blockIdx.x;
    extern __shared__ float s_arr[];

    //Copy to shared memory
    s_arr[tid] = d_in[tid];

	__syncthreads();

	//Up-sweep
	for(int d=1; d<blockDim.x; d*=2){
		if(  tid%(2*d)==(2*d-1)  ){
			s_arr[tid] = s_arr[tid-d]+s_arr[tid];
		}
		__syncthreads();
	}
	if(threadIdx.x==blockDim.x-1)
		s_arr[tid] = neutral;

	__syncthreads();

	//Down-sweep
	for(int d=blockDim.x/2; d>0; d/=2){
		if(  tid%(2*d)==(2*d-1)  ){
			float tmp = s_arr[tid-d];
			__syncthreads();
			s_arr[tid-d] = s_arr[tid];
			s_arr[tid] = tmp+s_arr[tid];
		}
		__syncthreads();
	}

    //Copy back to global memory
    d_out[tid] = s_arr[tid];
}

/** Maps '+d_mapVals[i]' to all elements in the sub-array of d_in that corresponds to this block*/
__global__ void sum_map(float *d_in, float *d_mapVals)
{
	const unsigned int tid = threadIdx.x + blockDim.x*blockIdx.x;
	d_in[tid] = d_in[tid]+d_mapVals[blockIdx.x];
}

/** Perform a parallel exclusive scan on inArr and store the result in outArr. */
void excl_sum_scan(float* inArr, float* outArr, int sz, float neutral)
{
    unsigned int num_threads = 8;
    unsigned int num_blocks  = max(sz/num_threads,1);
    unsigned int global_mem_size = sizeof(float) * sz;
    unsigned int shared_mem_size = sizeof(float) * num_threads;

	// Allocate device memory
	float *d_in;
	float *d_out;
	cudaMalloc((void **) &d_in , global_mem_size);
	cudaMalloc((void **) &d_out, global_mem_size);

	// Scan segments within blocks
	cudaMemcpy(d_in, inArr, global_mem_size, cudaMemcpyHostToDevice);
	excl_sum_scan_kernel<<< num_blocks, num_threads, shared_mem_size >>>(d_in, d_out, 0);
	cudaMemcpy(outArr, d_out, global_mem_size, cudaMemcpyDeviceToHost);

	if(num_blocks==1){ //We're done. 
		cudaMemcpy(outArr, d_out, global_mem_size, cudaMemcpyDeviceToHost);
	}else{ 
		// Collect the list of reductions over each block
		unsigned int intermediate_size = 1;
		while(intermediate_size<num_blocks) {intermediate_size*=2;}
		unsigned int intermediate_mem_size = sizeof(float)*intermediate_size;
		float *h_intermediate_in  = (float *)malloc(intermediate_mem_size);
		float *h_intermediate_out = (float *)malloc(intermediate_mem_size);
		for(int i=0;i<num_blocks;i++)
			h_intermediate_in[i] = outArr[(i+1)*num_threads-1] + inArr[(i+1)*num_threads-1]; //Last element of block i
		for(int i=num_blocks;i<intermediate_size;i++)
			h_intermediate_in[i] = 0; //Padding to get to size 2^k

		// Recursively scan this list
		excl_sum_scan(h_intermediate_in, h_intermediate_out, intermediate_size, 0);

		// Map the scanned elements to each block with addition
		float *d_intermediate;
		cudaMalloc((void **) &d_intermediate, intermediate_mem_size);
		cudaMemcpy(d_intermediate, h_intermediate_out, intermediate_mem_size, cudaMemcpyHostToDevice);
		sum_map<<<num_blocks,num_threads>>>(d_out, d_intermediate);
		cudaMemcpy(outArr, d_out, global_mem_size, cudaMemcpyDeviceToHost);

		// cleanup
		cudaFree(d_intermediate);
		free(h_intermediate_in);
		free(h_intermediate_out);
	}

	// cleanup 
	cudaFree(d_in);
	cudaFree(d_out);

}

int main(int argc, char **argv)
{

	// Create large array of [1.0 1.0 1.0 ...]
	unsigned int arrSize = THREADS_PER_BLOCK*3;
    float *arrIn  = (float *) malloc(sizeof(float)*arrSize);
    float *arrOut = (float *) malloc(sizeof(float)*arrSize);

	for(unsigned int i = 0; i < arrSize; ++i)
		arrIn[i] = 1.0;

	// Scan large array
	excl_sum_scan(arrIn, arrOut, arrSize, 0);

	// Print result
	printf("Input:  ");
	for(unsigned int i = 0; i < arrSize; ++i)
		printf("%5.1f ",arrIn[i]);

	printf("...\nOutput: ");
	for(unsigned int i = 0; i < arrSize; ++i)
		printf("%5.1f ",arrOut[i]);
	printf("...\n");

	free(arrIn);
	free(arrOut);

	return 0;
}



