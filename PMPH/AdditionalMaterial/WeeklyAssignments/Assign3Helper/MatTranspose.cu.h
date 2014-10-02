// Naive Transpose
template <class T> 
__global__ void matTransposeKer(float* A, float* B, int heightA, int widthA) {

  int j = blockIdx.x*blockDim.x + threadIdx.x;
  int i = blockIdx.y*blockDim.y + threadIdx.y; 

  if( (j >= widthA) || (i >= heightA) ) return;

  // FILL IN: write with flat indexes for B and A
  // i.e., what are the width and height of A and B?
  B[j,i] = A[i,j];
}

// Tiled version of Transposed
// blockDim.y = TILE; blockDim.x = TILE
// each block transposes a square TILE
template <class T, int TILE> 
__global__ void matTransposeTiledKer(float* A, float* B, int heightA, int widthA) {

  __shared__ float tile[TILE][TILE+1];

  int x = blockIdx.x * TILE + threadIdx.x;
  int y = blockIdx.y * TILE + threadIdx.y;

  if( x < widthA && y < heightA )
      // FILL IN, also write with flat index for A 
      // i.e., what are the width and height of A?
      tile[...][...] = A[y,x]; 

  __syncthreads();

  x = blockIdx.y * TILE + threadIdx.x; 
  y = blockIdx.x * TILE + threadIdx.y;

  if( x < heightA && y < widthA )
      // FILL IN, also write with flat index for B
      // i.e., what are the width and height of B?
      B[y,x] = tile[...][...];
}


__global__ void 
origProg(float* A, float* B, unsigned int N) {
    unsigned long long gid = (blockIdx.x * blockDim.x + threadIdx.x);
    if(gid >= N) return;

    gid *= 64;
    float tmpB = A[gid];
    tmpB = tmpB*tmpB;
    B[gid] = tmpB;
    for(int j=1; j<64; j++) {
        float tmpA  = A[gid + j];
        float accum = sqrt(tmpB) + tmpA*tmpA; //tmpB*tmpB + tmpA*tmpA;
        B[gid + j]  = accum;
        tmpB        = accum;
    }
}

__global__ void 
transfProg(float* A, float* B, unsigned int N) {
    unsigned long long gid = (blockIdx.x * blockDim.x + threadIdx.x);
    if(gid >= N) return;

    float tmpB = A[gid];
    tmpB = tmpB*tmpB;
    B[gid] = tmpB;
    gid += N;
    for(int j=1; j<64; j++,gid+=N) {
        float tmpA  = A[gid];
        float accum = sqrt(tmpB) + tmpA*tmpA;
        B[gid]  = accum;
        tmpB    = accum;
    }
}

