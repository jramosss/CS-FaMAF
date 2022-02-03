//Precondicion {x=x ^ y=y ^ z=z}
//Postcondicion {x = z ^ y = x ^ z = y}

#include <stdio.h>
int main () {
	int x,y,z;
	printf("Inserte un valor para x %d");
	scanf("%d",&x);
	printf("Inserte un valor para y %d");
	scanf("%d",&y);
	printf("Inserte un valor para z %d");
	scanf("%d",&z);
	z = x;
	x = y;
	y = z;
	printf("El valor de x es %d \n",x);
        printf("El valor de y es %d \n",y);
	printf("El valor de z es %d \n",z); 
return 0;
}
