typedef node = tuple
			   *node left;
			   elem value;
			   *node right;

typedef bintree = *node
typedef set = bintree;

elem max (set s) {
	elem max = -inf;
	while (s -> right != NULL){
		if (s -> value > max) {
			max = *s -> value;
		}
	}
	s = s -> right
	return max;
}

elem maxpos (set s) { 
	*node maxpos = NULL;
	while (s -> right != NULL){
		if (s -> value > max) {
			max = s -> value;
		}
	}
	s = s -> right
	return maxpos;
}

void delete_max (set s) { 
	*node aux = NULL;
	aux = maxpos -> right;
	free (maxpos);
	maxpos = NULL;
}

bool is_heap (set s) {
	return (s -> left -> value <= s -> value && s -> right -> value <= s -> value && is_heap (s->left) && is_heap (s->right));
}