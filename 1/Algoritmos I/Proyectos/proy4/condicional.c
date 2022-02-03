#include <stdio.h>

/* Para leer del teclado booleanos, no usar scanf, sino
   directamente hacer la asignación correspondiente.
*/

int main() {
    int x, y, z, m;
    x = 0;
    y = 0;
    z = 0;
    m = 0;
    
    printf("Inserte tres numeros enteros separados por espacios:\n");
    scanf("%d %d %d", &x, &y, &z);

      if ( x < y && x < z)  {
 		m = x; 
 		}
	else if ( y < z ) {
		m = y;
 		}
		else{
		m = z;
		}

    printf("El minimo entre los tres es %d \n", m);
    return 0;
} 
