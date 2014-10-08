// widthA = heightB
template <class T> 
__global__ void floydWarshallKer(float* A, float* B, float* C, int widthA, int heightA, int widthB) {
  T accum = 0.0f;

  int gidx = blockIdx.x*blockDim.x + threadIdx.x;
  int gidy = blockIdx.y*blockDim.y + threadIdx.y; 

  if( (gidx >= widthB) || (gidy >= heightA) ) return;

  for(int k = 0; k < widthA; k ++) {
      accum += A[gidy*widthA + k] * B[k*widthB + gidx];
  }

  C[gidy*widthB + gidx] = accum;
}


#define BIG_FLOAT 99999999.99

// widthA = heightB
template <int TILE> 
__global__ void floydWarshallTiledKer(float* A, float* C, unsigned int N) {
  float accum = BIG_FLOAT;

  __shared__ float Ash[TILE][TILE];
  __shared__ float Bsh[TILE][TILE];

  int gidx = blockIdx.x*blockDim.x + threadIdx.x;
  int gidy = blockIdx.y*blockDim.y + threadIdx.y; 

  for(int kk = 0; kk < N; kk += TILE) {
      Ash[threadIdx.y][threadIdx.x] = ((gidy < N) && (kk+threadIdx.x < N)) ?
            A[gidy*N + kk + threadIdx.x] : BIG_FLOAT;
      Bsh[threadIdx.y][threadIdx.x] = ((gidx < N)  && (kk+threadIdx.y < N)) ?
            A[(threadIdx.y+kk)*N + gidx] : BIG_FLOAT;
      __syncthreads();

      #pragma unroll
      for(int k = 0; k < TILE; k++)
          accum = min(accum, Ash[threadIdx.y][k] + Bsh[k][threadIdx.x]);

      __syncthreads();
  }

  if( (gidx < N) && (gidy < N) )
    C[gidy*N + gidx] = accum;
}

