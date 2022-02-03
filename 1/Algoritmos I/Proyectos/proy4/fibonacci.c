#include <stdio.h>

//int suma (m,n:int) {
  // int a;
   // a = m + n;
   //return a;
//}

int main() {
    int n, fibn0, fibn1, i, aux;
    fibn0 = 1;
    fibn1 = 1;
    aux = 0;
    i = 2;
	printf("Inserte un numero %d");
	scanf("%d",&n);
    while ( i <= n) {
         aux = fibn1; 
         fibn1 = fibn0 + fibn1;
    	 fibn0 = aux;
 	 i = i + 1;
 		}
    printf("Fib(%d) = %d\n", n, fibn1);
    return 0;
}


