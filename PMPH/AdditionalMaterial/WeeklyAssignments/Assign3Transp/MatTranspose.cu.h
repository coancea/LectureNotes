// widthA = heightB
template <class T> 
__global__ void matTransposeKer(float* A, float* B, int heightA, int widthA) {

  int gidx = blockIdx.x*blockDim.x + threadIdx.x;
  int gidy = blockIdx.y*blockDim.y + threadIdx.y; 

  if( (gidx >= widthA) || (gidy >= heightA) ) return;

  B[gidx*heightA+gidy] = A[gidy*widthA + gidx];
}

// blockDim.y = TILE; blockDim.x = TILE
// each block transposes a square TILE
template <class T, int TILE> 
__global__ void matTransposeTiledKer(float* A, float* B, int heightA, int widthA) {

  __shared__ float tile[TILE][TILE+1];

  int x = blockIdx.x * TILE + threadIdx.x;
  int y = blockIdx.y * TILE + threadIdx.y;

  if( x < widthA && y < heightA )
      tile[threadIdx.y][threadIdx.x] = A[y*widthA + x];

  __syncthreads();

  x = blockIdx.y * TILE + threadIdx.x; 
  y = blockIdx.x * TILE + threadIdx.y;

  if( x < heightA && y < widthA )
      B[y*heightA + x] = tile[threadIdx.x][threadIdx.y];
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

