#ifndef MULT_KERNELS
#define MULT_KERNELS

// widthA = heightB
template <class ElTp> 
__global__ void matMultKer(ElTp* A, ElTp* B, ElTp* C, int heightA, int widthB, int widthA) {
  ElTp accum = 0.0f;

  int gidx = blockIdx.x*blockDim.x + threadIdx.x;
  int gidy = blockIdx.y*blockDim.y + threadIdx.y; 

  if( (gidx >= widthB) || (gidy >= heightA) ) return;

  for(int k = 0; k < widthA; k ++) {
      accum += A[gidy*widthA + k] * B[k*widthB + gidx];
  }

  C[gidy*widthB + gidx] = accum;
}


// widthA = heightB
template <class ElTp, int T> 
__global__ void matMultTiledKer(ElTp* A, ElTp* B, ElTp* C, int heightA, int widthB, int widthA) {
  ElTp accum = 0.0f;

  __shared__ ElTp Ash[T][T];
  __shared__ ElTp Bsh[T][T];

  int gidx = blockIdx.x*blockDim.x + threadIdx.x;
  int gidy = blockIdx.y*blockDim.y + threadIdx.y; 

  for(int kk = 0; kk < widthA; kk += T) {
      Ash[threadIdx.y][threadIdx.x] = ((gidy < heightA) && (kk+threadIdx.x < widthA)) ?
            A[gidy*widthA + kk + threadIdx.x] : 0.0;
      Bsh[threadIdx.y][threadIdx.x] = ((gidx < widthB)  && (kk+threadIdx.y < widthA)) ?
            B[(threadIdx.y+kk)*widthB + gidx] : 0.0;
      __syncthreads();

      #pragma unroll
      for(int k = 0; k < T; k++)
          accum += Ash[threadIdx.y][k] * Bsh[k][threadIdx.x];
      __syncthreads();
  }

  if( (gidx < widthB) && (gidy < heightA) )
    C[gidy*widthB + gidx] = accum;
}

// widthA = heightB
template <class ElTp, int T> 
__global__ void matMultCacheKer(ElTp* A, ElTp* B, ElTp* C, int heightA, int widthB, int widthA) {
  ElTp accum = 0.0f;

  int gidx = blockIdx.x*blockDim.x + threadIdx.x;
  int gidy = blockIdx.y*blockDim.y + threadIdx.y; 

  for(int kk = 0; kk < widthA; kk += T) {
      __syncthreads();
      #pragma unroll
      for(int k = 0; k < T; k++)
        accum += A[gidy*widthA + kk + k] * B[gidy*widthB + (kk+k)];
  }

  if( (gidx < widthB) && (gidy < heightA) )
    C[gidy*widthB + gidx] = accum;
}


template <class ElTp, int T> 
__global__ void matMultRegTiledKer(ElTp* A, ElTp* B, ElTp* C, int heightA, int widthB, int widthA) {
  __shared__ ElTp Ashreg[T][T];
  ElTp cs[T];

  unsigned int ii  = blockIdx.y * T;
  unsigned int jjj = blockIdx.x * T * T;
  unsigned int jj  = jjj + threadIdx.y * T;
  unsigned int j   = jj  + threadIdx.x;

  #pragma unroll
  for(int i=0; i<T; i++)
    cs[i] = 0.0;

  for(int kk = 0; kk < widthA; kk += T) {
      ElTp tmp = 0;
      if ((ii+threadIdx.y < heightA) && (kk+threadIdx.x < widthA)) {
        tmp = A[(ii+threadIdx.y)*widthA + kk+threadIdx.x];
      }
      Ashreg[threadIdx.y][threadIdx.x] = tmp;
      __syncthreads();

      for(int k = 0; k < T; k++) {
          ElTp b = 0;
          if ((k+kk < widthA) && (j < widthB)) {
            b = B[(k+kk)*widthB + j];
          }

          #pragma unroll 
          for(int i=0; i<T; i++) {
            cs[i] += Ashreg[i][k] * b;
          }
      }
      __syncthreads();
  }

  #pragma unroll
  for(int i=0; i<T; i++) {
    if( (ii+i < heightA) && (j < widthB) )
      C[(ii+i)*widthB + j] = cs[i];
  }
}


#endif
