#include <stdio.h>

struct div_t {
    int cociente;
    int resto;
};


struct div_t division(int x, int y) {
	struct div_t resultado;
	int cocient, rest;
	rest = x%y;
	cocient = x/y;
	resultado.resto = rest;
	resultado.cociente = cocient;
	return resultado;
  }

int main() {	
	int divisor, dividendo;
	printf("Inserte un valor para divisor: \n");
	scanf("%d",&divisor);
	printf("Inserte un valor para dividendo: \n");
	scanf("%d",&dividendo);	
	
	
    if (divisor > 0 && dividendo > 0) {
    struct div_t res = division(divisor, dividendo);
    printf("El cociente es %d y el resto %d\n", res.cociente, res.resto);
	}
	else 
	printf("Error, ingrese un valor positivo \n");
    return 0;
}
