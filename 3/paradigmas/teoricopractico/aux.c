#include<stdio.h>
#include<stdlib.h>

/*
union element {
    int i;
    float f;
};

int main () {
    union element e;
    e.f = 177689982.02993F;
    printf("Int = %d \n",e.i);
    printf("Float = %f \n",e.f);
    e.f = 0.0F;
    return 7;
}*/
typedef unsigned int uint;

uint* minfilas (uint** m,uint filas,uint columnas){
    uint min = 0;
    uint* minimos = calloc(filas,sizeof(uint));
    for(uint i = 0; i < filas; i++){
        min = 0;
        for(uint j = 0; j < columnas; j++){
            if(m[i][j] < min){
                min = m[i][j];
            }
        }
        minimos[i] = min;
    }
    return minimos;
}

static void mat_dump(uint** arr,uint filas,uint columnas){
    printf("[");
    for(uint i = 0; i < filas; i++){
        for(uint j = 0; j < columnas; j++){
            printf("%d,",arr[i][j]);
        }
    }
    printf("]\n");
}

static void arr_dump(uint* arr,uint len){
    printf("[");
    for(uint i = 0; i < len; i++){
        printf("%d,",arr[i]);
    }
    printf("]\n");
}

int main(){
    uint** matriz = calloc(9,sizeof(uint));
    for(uint i = 0; i < 3; i++){
        for(uint j = 0; j < 3; j++){
            matriz[i][j] = i+j;
        }
    }
    mat_dump(matriz,3,3);
    arr_dump(minfilas(matriz,3,3),3);
    
    free(matriz);
    return 7;
}