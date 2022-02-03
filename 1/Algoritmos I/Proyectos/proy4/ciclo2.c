#include <stdio.h>
#include <stdbool.h>

int main() {
    int x, i;
    bool res = true;

    printf("Inserte un numero entero.\n");
    scanf("%d", &x);
    i = 2;

    while (res && i < x) {
       res = res && ! (x % i == 0);
       i = i+1;
    }
    if (res) { 
      printf("El numero es primo \n");
    }else{ 
      printf("El numero no es primo\n");
    }
    
    return 0;
}
