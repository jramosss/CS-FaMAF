#include <stdio.h>

int main() {
    int x, y, i;
    x = 0;
    y = 0;
    i = 0;
    
    printf("Inserte el dividendo: \n",x);
    scanf("%d", &x);
    printf("Inserte el divisor: \n",y);
    scanf("%d", &y);

//    i = 0;
//    while (y <= x) {
//	x = x - y;
//	i = i + 1; 
//    }

//printf ("El resultado es: %d",x);
//Esto deberia compilar, pero por algun misterio de la vida, cuando ponemos un x<y entra en el ciclo, asi que lo definimos de otra forma.
  
if (x<y) {
   printf ("Error\n");
 }
else { 
    while (y<=x){
     x = x-y;
     i = i+1;
	
}
	printf ("El resultado es:%d\n",x);
}


  return 0;
}
