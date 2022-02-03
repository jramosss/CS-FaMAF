#include <stdio.h>
#include <stdlib.h>
#include "assert.h"
#include "stack.h"

struct _stack_t {
	stack_elem_t value;
	stack_t next;
};
 
stack_t stack_empty() {
	return NULL;
}

bool stack_is_empty (stack_t s) {
	return (s == NULL);
}

stack_t stack_push(stack_t s, stack_elem_t elem) { // esto  no esta pusheando
	stack_t new = calloc (1,sizeof(struct _stack_t));
	new->value = elem;
	new->next = s;
	return new;
}

unsigned int stack_size(stack_t s) {
	unsigned int n = 0;
	stack_t aux = s;
	if (!stack_is_empty(s)){
		while (aux != NULL) {
			n++;
			aux = aux->next;
		}
	}
	return n;
}

stack_t stack_pop(stack_t s) {
	assert(!stack_is_empty(s));
	stack_t p = s;
	s = s->next;
	free(p);
	p = NULL;
	return s;
}

stack_elem_t stack_top(stack_t s) {
	assert(!stack_is_empty(s));
	return (s->value);
}

stack_elem_t *stack_to_array(stack_t s) {
	stack_elem_t *a = NULL;
	stack_t aux = s;
	unsigned int i = 0;
	if (!stack_is_empty(s)) {
		a = calloc (stack_size(s),sizeof(stack_elem_t));
		while (aux != NULL) {
			a[i] = aux->value;
			i++;
		}
	}
	return a;
}

void stack_print (stack_t s) {
	stack_t aux = s;
	unsigned int i = 0;
	if (!stack_is_empty(s)){
		while (aux != NULL) {
			if (i == stack_size(s)-1) {
				printf("%d\n",aux->value);
			}
			else {
				printf("%d,\n",aux->value);
			}
			i++;
		}
	}
	else {
		printf("Empty Stack\n");
	}
}

stack_t stack_destroy(stack_t s) {
	stack_t aux = NULL;
	while (s != NULL) {
		aux = s;
		s = s->next;
		free(aux);
	}
	return s;
}
