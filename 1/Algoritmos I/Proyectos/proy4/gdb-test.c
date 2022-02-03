#include <stdio.h>
#define N 5

// Programa con bugs!

// Devuelve la cantidad de elementos pares en un arreglo:
int cantidad_de_pares(int a[], int size) {
    int i;
    int cantidad = 0;
    while (i <= N) {
	if (a[i] % 2 == 0) {
	    cantidad = cantidad + 1;
	    i = i + 1;
	}
    }
    return cantidad;
}

int main() {
    int a[N];
    int i = 0;
    while (i < N) {
	printf("Inserte el elemento de la posición %d del arreglo: ", i);
	scanf("%d", &a[i]);
	i++;
    }
    printf("Usted insertó el siguiente arreglo: ");
    i = 0;
    while (i < N) {
	printf("%d ", a[i]);
	i++;
    }
    printf("\n La cantidad de números pares del arreglo es %d \n",
	   cantidad_de_pares(a, N + 1));
    return 0;
}
