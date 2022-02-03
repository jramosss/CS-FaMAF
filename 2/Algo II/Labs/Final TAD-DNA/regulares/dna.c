#include "dna.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

struct _node {
    nucleobase_t elem;
    struct _node *next;
};

static dna_t single_node(nucleobase_t e)
{
    struct _node *node = calloc(1, sizeof(struct _node));
    assert(node != NULL);
    node->elem = e;
    node->next = NULL;
    return (node);
}

dna_t dna_T () {
	dna_t new = single_node(T);
	return new;
}

dna_t dna_C () {
	dna_t new = single_node(C);
	return new;
}

dna_t dna_A () {
	dna_t new = single_node(A);
	return new;
}

dna_t dna_G () {
	dna_t new = single_node(G);
	return new;
}

unsigned int dna_length(dna_t dna)
{
    unsigned int count = 0;
    dna_t node = dna;
    while (node != NULL) {
        node = node->next;
        count++;
    }
    return (count);
}

bool dna_is_prefix(dna_t first, dna_t second) {
	bool b = true; 			// b esta inicializado en true, por lo cual no es necesario el caso fst == NULL || snd ==NULL
	if (dna_length(first) > dna_length(second) && first != NULL && second != NULL) {
		b = false;
	}
	else if (first != NULL && second != NULL) {
		dna_t aux1 = first;
		dna_t aux2 = second;
		while (aux1 != NULL) {
			if (aux1->elem != aux2->elem) {
				b = false;
				break;
			}
			aux1 = aux1->next;
			aux2 = aux2->next;
		}
	}
	return b;
}

bool dna_is_equal(dna_t first, dna_t second)
{
    return dna_is_prefix(first, second)
        && dna_length(first) == dna_length(second);
}

dna_t dna_join(dna_t first, dna_t second) { // no crear nueva tira, agregar los elementos de second a first
	dna_t aux1 = first;
	if (aux1 != NULL) {
		while (aux1->next != NULL) {
			aux1 = aux1->next;
		}
	}
	else{
		aux1 = calloc(1,sizeof(struct _node));
		aux1->next = second;
	}
	return first;
}
/*
static dna_t add_one (dna_t first,dna_t second) {
	dna_t aux = first;
	while (aux->next != NULL) {
		aux = aux->next;
	}
	aux->next = second;
	return first;
}

dna_t *dna_cut(dna_t dna, unsigned int count) {
	assert(count > 0 && count < dna_length(dna));
	dna_t *a = calloc (2,sizeof(struct _node));
	if (dna_length(dna) > 1) {
		dna_t aux = dna;
		dna_t aux2 = calloc(1,sizeof(struct _node));
		dna_t aux3 = calloc(1,sizeof(struct _node));
		unsigned int i = 0;
		while(i<count) {
			a[0] = add_one(aux2,aux); // C.G.A.C
			aux = aux->next;
			i++;
		}
		while (i < dna_length(dna)) {
			a[1] = add_one(aux3,aux); // C
			aux = aux->next;
			i++;
		}
	}
	else {
		a[0] = dna;
		a[1] = NULL;
	}
	return a;
}
*/
dna_t dna_copy(nucleobase_t elem){
    dna_t copy = NULL;
    switch(elem) {
    	case T: 
    		copy = dna_T();
    		break;
    	case A:
    		copy = dna_A();
    		break;
    	case C:
    		copy = dna_C();
    		break;
    	case G:
    		copy = dna_G();
    		break;
    	default:
    		break;
    }
    return copy;
}

dna_t *dna_cut(dna_t dna, unsigned int count){
    assert(dna != NULL);
    assert(count > 0);
    assert(count < dna_length(dna));
    
    dna_t *arreglo = malloc(sizeof(struct _node));
    dna_t aux1 = NULL;
    dna_t aux2 = dna;
    dna_t aux3 = NULL;

    for (unsigned int i = 0; i < count ; i++){
        aux1 = dna_join(aux1, dna_copy(aux2->elem));
        aux2 = aux2->next;
    }
    for (unsigned int i = 0; i < dna_length(dna) - count; i++){
        aux3 = dna_join(aux3, dna_copy(aux2->elem));
        aux2 = aux2->next;
    }
    arreglo[0] = aux1;
    arreglo[1] = aux3;
    return arreglo;
}

void dna_print(dna_t dna)
{
    dna_t node = dna;
    assert(node != NULL);
    while (node != NULL) {
        char *letter = "T";
        switch (node->elem) {
        case T:
            letter = "T";
            break;
        case C:
            letter = "C";
            break;
        case A:
            letter = "A";
            break;
        case G:
            letter = "G";
            break;
        default:
            break;
        }
        printf("%s.", letter);
        node = node->next;
    }
    printf("\n");
}

dna_t dna_destroy(dna_t dna) {
	dna_t aux = dna;
	while (dna != NULL) {
		aux = dna;
		dna = dna->next;
		free (aux);
	}
	return dna;
}
