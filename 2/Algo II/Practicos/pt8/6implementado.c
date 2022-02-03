#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
	unsigned int partida;
	unsigned int regreso;
	bool usado_o_revisado;
}amigo;

void greedy (amigo a[], unsigned int length) {
	amigo cand;
	for (unsigned int i = 0; i < length; i++) {
		cand = cand(a,length,i);
		descartar(a,length,cand);
		a[cand]->usado_o_revisado = true;
	}
}

amigo cand (amigo a[],unsigned int length, unsigned int j) {
	int min = 23523;
	for (unsigned int i = 0; i < length; i++) {
		if (a[i]->regreso < min && !a[i]->usado_o_revisado) {
			min = a[i]->regreso;
			cand = i;
		}
	}
}

void descartar (amigo a[], unsigned int length, amigo x) {
	for (unsigned int i = 0; i < length; i++) {
		if (a[i]->partida < a[x]->regreso) {
			a[i]->usado_o_revisado = true;
		}
	}
}

int main () {
	greedy(a,5);
	return 0;
}