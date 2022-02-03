typedef node = tuple {
			   elem value;
			   node *next; //next es un puntero a nodo
			   node *prev;
}tuple;

typedef Bq = node *b;
// si a -> next == a -> prev, la cola tiene un solo elem

bool is_empty (Bq x) {
return (x == NULL);
}

elem fst (Bq x) {
return (x.value);
}

elem lst (Bq x) {
	elem e;
	x = x.prev;
	e = x.value;
return e;
}

void rmfst (Bq x) {
	node *a = x;
	node *b = x;
	a = x.next;
	b = a.prev;
	free (b);
}

void rmlst (Bq x) {
	node *a = x;
	node *b = x;
	a = x.prev;
	b = a.prev;
	free (a);
}

void addfst (Bq x, elem e) { // x.prev [x.value]

}

void addlst (Bq x, elem e) {
	
}