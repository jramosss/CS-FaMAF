/* 
Cómo compilar:

gcc -Wall -pedantic -std=c99 -o expresiones1 expresiones1.c

Cómo ejecutar:

./expresiones1

*/


#include <stdio.h>  // printf, scanf 
#include <stdbool.h> // bool

/* En este ejercicio no usar scanf, sólo asignaciones y printf. */

int main() {
    int x, y, z;
    bool b, w;
    x = 4;
    y = 5;
    z = 8;
    w = true;
    b = false;
    //printf ("ingrese un valor para x: %d", x);
    //scanf ("%d", &x);

    //completar
     
    printf("El valor de la expresion es %d \n", x + y + 1);
    
    printf("El valor de la expresion es %d \n", z * z + y * 45 - 15 * x); 
    
    printf("El valor de la expresion es %d \n", x < z && ! w);
    
    printf("El valor de la expresion es %d \n", y - 2 == (x * 3 + 1) % 5);
    
    printf("El valor de la expresion es %d \n", y / 2  * x);
    
    printf("El valor de la expresion es %d \n", 10 < x || b);
    

    
    //completar
    
    return 0;
}
