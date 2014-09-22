#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "ScanHost.cu.h"


#include "ExclScan.cu.h" 
#include "MsspProblem.cu.h" 
#include "SparseMatVctMult.cu.h" 

int main(int argc, char** argv) { 
    const unsigned int mssp_list_size = 8353455; 
    const unsigned int matrix_row_num = 11033;
    const unsigned int vct_size       = 2076;
    const unsigned int block_size     = 256;

    scanExcTest();
    MsspProblem(block_size, mssp_list_size);
    SparseMatVctMult(block_size, matrix_row_num, vct_size);
}
