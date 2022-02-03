typedef Bicola of elem = tuple
						 unsigned int size;
						 unsigned int fst;
						 elem a [ 1...n-1];

bool is_empty (Bicola x) {
return (x -> size == 0);
}

bool is_full (Bicola x) {
return (x -> size == n-1);
}

elem primero (Bicola x) {
	elem e;
	assert (!is_empty(x));
		e = x -> array [fst];
return e;
}

elem ultimo (Bicola x) {
	elem e;
	assert (!is_empty(x));
	e = x -> array [size-1] + fst;
return e;
}

void rmfst (Bicola x) {
	if ((!is_empty(x)) && x -> fst != x -> size) {
		x -> fst++;
	}
}

void rmlst (Bicola x) {
	if (!is_empty(x) && x -> fst != x -> size) {
		x -> size --;
	}
}

void addfst (Bicola x, elem e) {
	if (is_empty(x)) {
		x -> size = 1;
		x -> fst = e;
	}
	else if (x -> size == 1) {
		x -> size ++;
		x -> fst+1 = x -> fst;
		x -> fst = e;
	}
	else {
		x -> size ++;
		x -> fst--;
		x -> fst = e;
	}
}

void addlst (Bicola x, elem e) {
	if (is_empty(x)) {
		x -> size = 1;
		x -> fst = e;
	}
	else if (x -> size == n-1) {
		printf("Cola llena");
		break;
	}
	else {
		x -> size++;
		x -> size = e;
	}
}

