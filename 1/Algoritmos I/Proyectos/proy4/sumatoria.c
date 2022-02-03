#include <stdio.h>
//Definición de una constante:
#define N 5
int sumatoria(int a[], int tam) {
    int y, skr;
      y = 0;
	skr = 0;
	while (y < tam) {
	skr = a[y] + skr;
	y = y+1;	
		}
return skr;
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
    printf("\n La suma total del arreglo es %d \n", sumatoria(a, N));
    return 0;
}
