#include <stdio.h>
int main () {
	int x,y,z,temp, tempy;
	printf("Inserte un valor para x: \n");
	scanf("%d",&x);
	printf("Inserte un valor para y; \n");
	scanf("%d",&y);
	printf("Inserte un valor para z; \n");
	scanf("%d",&z);
	temp = x;	
	x = y;
	tempy = y;
	y = y + z + temp;
	z = tempy + temp;
	printf("El valor de x es %d \n",x);
	printf("El valor de y es %d \n",y);
	printf("El valor de z es %d \n",z);
return 0;
}
