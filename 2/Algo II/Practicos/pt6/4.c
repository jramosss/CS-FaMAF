typedef Stack = tuple
				a [1...n] of elems;
				unsigned int size;

void empty () {
return [];
}

bool is_empty (Stack x) {
return (x.size == 0);
}

bool is_full (Stack x) {
return (x.size == n);
}

void push (Stack x, elem e) {
	x.size++;
	x.a[size] = e;
}

void pop (Stack x) {
	x.size--;
}

Stack top (Stack x) {
return x.a[n];
}

void reverse (Stack x) {
	unsigned int i = 1;
	array of elem a;
	while (!is_empty(x)) {
		a[i] = x.a[i+n-1]; 
		pop(x);
		i++;
	}
}
