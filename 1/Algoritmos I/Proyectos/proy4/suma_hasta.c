#include <stdio.h>

int suma_hasta(int N) {
        if (N<0) {
	   printf("Error, deja de hacerte el gracioso gil");
	}
	else {  
	N = (N*(N+1))/2;
	}
return N;
}
        
int main() {
	int n = 0;
	printf("Inserte un valor para n \n");
	scanf("%d",&n);
    printf("La suma hasta %d es %d \n", n, suma_hasta(n));
    return 0;
}
