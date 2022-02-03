#include <stdio.h>

int main(){
int x;

    printf ("ingrese un numero: \n", x);
    scanf ("%d",&x);

    if (x>=0){
	printf("El valor absoluto es: %d\n",x);
}
    if (x<0){
	printf ("El valor absulto es: %d\n", (-x));
}

}
