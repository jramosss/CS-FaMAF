typedef person = tuple {
	unsigned int oxygen;
	unsigned int time;
}
/*
Tengo que tener en cuenta la cantidad de oxifeno que tiene cada uno, en base a eso, el que tiene menos oxigeno es el primero que tiene que ser 
salvado.
Mientras tanto tengo que tener una funcion para sacar del conjunto a los que se quedaron sin oxigeno.
*/

array of person save (array of person x[],length) {
	y = array_copy(x);
	for(unsigned int i = 0; i < length, i++) {
		y = selection_sort (y,length); 				//Despues de ejecutar esto, van a estar ordenados por cantidad de oxigeno de menor a mayor
		y = sacar_muertos(y);
		remove(x!i);								// saco i del array
		X->time++;	
	}
	return y;
}

int min_pos_from (int a[],unsigned int length, int i) {
    unsigned int min = i;
    for (unsigned int j=i+1 ; j<length; j++){
       	if ((a[j]->oxygen)<a[min]) {
        	min=j;
        }
    }
	return min;
}

void selection_sort(int a[], unsigned int length) {
        unsigned int minp;
        for (unsigned int i=0; i+1<length; i++ ){
                minp = min_pos_from (a,length,i);
                swap(a,i,minp);
        }
}

array of person sacar_muertos (array of person x[]) {
	array of person vivos = array_copy(x);
	for (unsigned int i = 0; i < length; i++) do {
		if (x[i]->oxygen <= 0) {
			remove(i);
		}
	}
	return vivos;
}