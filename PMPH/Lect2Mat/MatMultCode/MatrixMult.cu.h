// widthA = heightB
template <class T> 
__global__ void matMultKer(float* A, float* B, float* C, int widthA, int heightA, int widthB) {
  T accum = 0.0f;

  int gidx = blockIdx.x*blockDim.x + threadIdx.x;
  int gidy = blockIdx.y*blockDim.y + threadIdx.y; 

  if( (gidx >= widthB) || (gidy >= heightA) ) return;

  for(int k = 0; k < widthA; k ++) {
      accum += A[gidy*widthA + k] * B[k*widthB + gidx];
  }

  C[gidy*widthB + gidx] = accum;
}


// widthA = heightB
template <class T, int TILE> 
__global__ void matMultTiledKer(float* A, float* B, float* C, int widthA, int heightA, int widthB) {
  T accum = 0.0f;

  __shared__ T Ash[TILE][TILE];
  __shared__ T Bsh[TILE][TILE];

  int gidx = blockIdx.x*blockDim.x + threadIdx.x;
  int gidy = blockIdx.y*blockDim.y + threadIdx.y; 

  Ash[threadIdx.y][threadIdx.x] = 0.0;
  Bsh[threadIdx.y][threadIdx.x] = 0.0;

  for(int kk = 0; kk < widthA; kk += TILE) {
      Ash[threadIdx.y][threadIdx.x] = ((gidy < heightA) && (kk+threadIdx.x < widthA)) ?
            A[gidy*widthA + kk + threadIdx.x] : 0.0;
      Bsh[threadIdx.y][threadIdx.x] = ((gidx < widthB)  && (kk+threadIdx.y < widthA)) ?
            B[(threadIdx.y+kk)*widthB + gidx] : 0.0;
      __syncthreads();

      #pragma unroll
      for(int k = 0; k < TILE; k++)
          accum += Ash[threadIdx.y][k] * Bsh[k][threadIdx.x];

      __syncthreads();
  }

  if( (gidx < widthB) && (gidy < heightA) )
    C[gidy*widthB + gidx] = accum;
}

