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

  for(int kk = 0; kk < widthA; kk += TILE) {
      Ash[threadIdx.y][threadIdx.x] = ((gidy < heightA) && (kk+threadIdx.x < widthA)) ?
            A[gidy*widthA + kk + threadIdx.x] : 0.0;
#if 1
      Bsh[threadIdx.y][threadIdx.x] = ((gidx < widthB)  && (kk+threadIdx.y < widthA)) ?
            B[(threadIdx.y+kk)*widthB + gidx] : 0.0;
#else
      Bsh[threadIdx.x][threadIdx.y] = ((gidx < widthA)  && (kk+threadIdx.y < widthB)) ?
            //B[(threadIdx.y+kk)*widthB + gidx] : 0.0; 
            B[gidx*widthB + (threadIdx.y+kk)] : 0.0;
#endif
      __syncthreads();

      #pragma unroll
      for(int k = 0; k < TILE; k++)
#if 1
          accum += Ash[threadIdx.y][k] * Bsh[k][threadIdx.x];
#else
          accum += Ash[threadIdx.y][k] * Bsh[threadIdx.x][k];
#endif
      __syncthreads();
  }

  if( (gidx < widthB) && (gidy < heightA) )
    C[gidy*widthB + gidx] = accum;
}

// widthA = heightB
template <class T, int TILE> 
__global__ void matMultCacheKer(float* A, float* B, float* C, int widthA, int heightA, int widthB) {
  T accum = 0.0f;

  int gidx = blockIdx.x*blockDim.x + threadIdx.x;
  int gidy = blockIdx.y*blockDim.y + threadIdx.y; 

  for(int kk = 0; kk < widthA; kk += TILE) {
      __syncthreads();
      #pragma unroll
      for(int k = 0; k < TILE; k++)
        accum += A[gidy*widthA + kk + k] * B[gidy*widthB + (kk+k)];
        //accum += A[gidy*widthA + kk + k] * B[(kk+k)*widthB + gidx];

  }

  if( (gidx < widthB) && (gidy < heightA) )
    C[gidy*widthB + gidx] = accum;
}

