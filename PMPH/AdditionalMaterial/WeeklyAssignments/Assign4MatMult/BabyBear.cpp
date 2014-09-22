#include <stdio.h>
#include <algorithm>

using namespace std;
/**
fun int MIN(int a, int b) = if(a<b) then a else b

fun [[int]] floydSbsImp(int N, *[[int]] D) =
    let DT = copy(transpose(D)) in
    loop (D) = for i < N do
        loop (D) = for j < N do
            let sumrow = map(op +, zip(D[i], DT[j])) in
            let minrow = reduce (MIN, 1200, sumrow)    in
            let minrow = MIN(D[i,j], minrow)        in
            let D[i,j] = minrow in D
        in D
    in D
**/

int* floydImp(const int N, int* Din, int* Dout) {
    for(int i=0; i<N; i++) {        
        for(int j=0; j<N; j++) {
            int accum = 10000000;
            for (int k=0; k<N; k++) {
                int tmp = Din[i*N + k] + Din[k*N + j];
                accum = std::min(accum, tmp);
            }
            Dout[i*N+j] = std::min(accum, Din[i*N+j]);
        }
    }
}

int main() {
    const int N = 3;
    int arr_in [9] = {2,4,5, 1,1000,3, 3,7,1};
    int arr_out[9];

    floydImp(3, arr_in, arr_out);

    printf("Result is:{\n");
    for(int i=0; i<N*N; i++)
        printf("%d, ", arr_out[i]);
    printf("}  !\n");

    return 1;
}

