#include <stdlib.h>
#include "assert.h"
#include "stack.h"
 
struct _stack_t {
stack_elem_t value;
stack_t next;	
}; 

stack_t stack_empty () {
return NULL;
}

bool stack_is_empty (stack_t x) {
return (x == NULL);
} 

//Metiendo desde abajo.
stack_t stack_push (stack_t s, stack_elem_t elem) {
	stack_t p = NULL;
	p = malloc (sizeof (struct _stack_t));
	p -> value = elem;
	p -> next = s;
	return p;
}

//Metiendo desde arriba.
stack_t stack_push2(stack_t s, stack_elem_t elem) {
	stack_t a;
	stack_t b = s;
	while (b != NULL) {
		a = b -> next;
	}
	malloc (sizeof (struct _stack_t));
	a = b -> next;
	a -> value = elem;
	return s;
}
//@brief Removes the element at the top of the stack
stack_t stack_pop(stack_t s) {
	stack_t p = NULL;
	assert (!stack_is_empty(s));
	p = s -> next;
	free(s);
	return p;
}

//@brief Returns the size of the stack
unsigned int stack_size(stack_t s) {
	stack_t p = s;
	unsigned int c = 0;
	if (stack_is_empty(s)) {
		c = 0;
	}
	else {
	while (p != NULL) {
		p = p -> next;
		c++;
	}
}
return c;
}

//@brief Returns the element at the top of the stacks
stack_elem_t stack_top(stack_t s) {
	assert (!stack_is_empty(s));
return s->value;
}

//@brief Creates an array with all the elements of the stack
stack_elem_t *stack_to_array(stack_t s) {
	//stack_elem_t a[] = [stack_size(s)];
	stack_elem_t *arr = calloc (stack_size(s), sizeof (stack_elem_t));
	stack_t p = s;
	if (!stack_is_empty(s)){
	for (unsigned int i = 0; i < (stack_size(s)); i++) {
		arr[i] = p -> value;
		p = p -> next;
	}
}
	else {
		arr = NULL;
	}
return arr;
}

//@brief Destroys the stack
stack_t stack_destroy(stack_t s) {
	stack_t p = s;
	stack_t d = s;
	while (s != NULL) {
		p = p -> next;
		free (d);
		d = p;
	}
return s;	
}
