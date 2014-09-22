#ifndef TRANSPOSE_H
#define TRANSPOSE_H

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include <sys/time.h>
#include <time.h> 

int timeval_subtract(struct timeval *result, struct timeval *t2, struct timeval *t1)
{
    unsigned int resolution=1000000;
    long int diff = (t2->tv_usec + resolution * t2->tv_sec) - (t1->tv_usec + resolution * t1->tv_sec);
    result->tv_sec = diff / resolution;
    result->tv_usec = diff % resolution;
    return (diff<0);
}


void randomInit(float* data, int size) {
   for (int i = 0; i < size; ++i)
   data[i] = rand() / (float)RAND_MAX;
}


template<class T>
void matTranspose(T* A, T* trA, int rowsA, int colsA) {
  for(int i = 0; i < rowsA; i++) {
    for(int j = 0; j < colsA; j++) {
      trA[j*rowsA + i] = A[i*colsA + j];
    }
  }
}

template<class T>
bool validate(float* A,float* trA, int rowsA, int colsA){
  bool valid = true;
  for(int i = 0; i < rowsA; i++) {
    for(int j = 0; j < colsA; j++) {
      if(trA[j*rowsA + i] != A[i*colsA + j]) {
        printf("row: %d, col: %d, A: %.4f, trA: %.4f\n", 
                i, j, A[i*colsA + j], trA[j*rowsA + i] );
        valid = false;
        break;
      }
    }
    if(!valid) break;
  }
  if (valid) printf("GPU TRANSPOSITION   VALID!\n");
  else       printf("GPU TRANSPOSITION INVALID!\n");
  return valid;
}

#endif // TRANSPOSE
