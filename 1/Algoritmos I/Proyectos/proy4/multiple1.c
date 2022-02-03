#include <stdio.h>
int main () {
	int x, y, temp;
	printf("Inserte un valor para x \n");
	scanf("%d",&x);
	printf("Inserte un valor para y \n");
	scanf("%d",&y);
	temp = x+1;
	y = y + x;
	temp = x;
	printf("El valor de x es %d \n",x);
	printf("El valor de y es %d \n",y);
return 0;
}
