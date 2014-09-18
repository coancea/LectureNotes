#ifndef MULT_H
#define MULT_H

#include <math.h>
#include <stdio.h>

//void matMult(float* A, float* B, float* C, int widthA, int colsB);
//bool isomorphic(float* A, float* B);

void matMult(float* A, float* B, float* C, int rowsA, int colsA, int colsB) {
  for(int i = 0; i < rowsA; i++) {
    for(int j = 0; j < colsB; j++) {
      float sum = 0.0;
      for(int k = 0; k < colsA; k++) {
        sum += A[i*colsA + k] * B[k * colsB + j];
      }
      C[i * colsB + j] = sum;
    }
  } 
  return;
}

bool isomorphic(float* A,float* B, int sizeAB){
    for(int i = 0; i < sizeAB; i++)
      if (fabs(A[i] - B[i]) > 0.0005) 
        return false;

    return true;
}

#endif
