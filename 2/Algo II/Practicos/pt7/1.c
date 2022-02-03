typedef node = tuple
			   *node left;
			   elem value;
			   *node right;

typedef bintree = *node
typedef set = bintree;

elem max (set s) {
	int max = -inf;
	while (s -> right != NULL){
		if (s -> value > max) {
			max = *s -> value;
		}
	}
	s = s -> right
	return max;
}

fun node_ammount (set x) {
	unsigned int c=0;
	c = 1 + node_ammount(x->left);
	c = node_ammount (x->right);
	return c;
}

fun deep (set x, set y) {
	unsigned int c = 0;
	if (x->value < y->value){
		c = 1+deep(x->right);
	}
	else {
		c = 1+deep(x->left);
	}
	return c;
}

fun level (set x, unsigned int n) {
	unsigned int c = 0;
	if (n == deep(x)) {  // ponele que deep calcula la deep de el arbol
		c++;
	}
	else {
		c = level(x->right) + level(x->left);
	}
	return c;
}