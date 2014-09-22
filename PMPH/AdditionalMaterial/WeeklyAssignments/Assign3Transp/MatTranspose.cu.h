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

  x = blockIdx.y * TILE + threadIdx.x;  // transpose block offset
  y = blockIdx.x * TILE + threadIdx.y;

  if( x < heightA && y < widthA )
      B[y*heightA + x] = tile[threadIdx.x][threadIdx.y];
}


// blockDim.y = BLOCK_ROWS; blockDim.x = TILE
// each block transposes a square TILE
template <class T, int TILE, int BLOCK_ROWS> 
__global__ void matTransposeTiledKer1(float* A, float* B, int heightA, int widthA) {

  __shared__ float tile[TILE][TILE+1];

  int x = blockIdx.x * TILE + threadIdx.x;
  int y = blockIdx.y * TILE + threadIdx.y;

  if( x < widthA )
  for (int j = 0; j < TILE; j += BLOCK_ROWS)
    if( y+j < heightA )
      tile[threadIdx.y+j][threadIdx.x] = A[(y+j)*widthA + x];

  __syncthreads();

  x = blockIdx.y * TILE + threadIdx.x;  // transpose block offset
  y = blockIdx.x * TILE + threadIdx.y;

  if( x < heightA )
  for (int j = 0; j < TILE; j += BLOCK_ROWS)
    if( y+j < widthA )
      B[(y+j)*heightA + x] = tile[threadIdx.x][threadIdx.y + j];
}

