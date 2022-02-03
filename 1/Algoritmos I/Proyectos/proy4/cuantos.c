#include <stdio.h>


int cuantos (
struct comp_t {
    int menores;
    int iguales;
    int mayores;
};

struct comp_t cuantos(int a[], int tam, int elem){
	struct comp_t resul;
	int i = 0;
	int men = 0;
	int ig = 0;
	int may = 0;
	while (i < tam) {
	if (a[i] < elem) {
	men = men++;
	if (i == elem){
	ig = ig ++;
	if (i > elem){
	may = may++;
	}
	i++;
}
	resul.menores = men;
	resul.igual = ig;
	resul.mayores = may;
}
	}
return struct resul;
}

int main (){
	int x;
	int a[x];
	int elem;
	int i = 0;

	printf("Ingrese el tamaño del arreglo: \n");
	scanf("%d", &x);
	printf("Ingrese el elemento a corroborar: \n");
	scanf("%d", &elemento);

	while (i<x){
		printf("Ingrese un elemnto %d del arreglo; \n",i);
		scanf("%d",&a[i]);
		i++;
	}
	struct comp_t resu = cuantos(a,x,elem);
	printf("Hay %d menores, %d iguales, y %d mayores\n", resu.menores, resu.iguales, resu.mayores);
	return 0;
}
