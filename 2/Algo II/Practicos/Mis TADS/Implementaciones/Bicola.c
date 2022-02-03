#include "TADBooleano.c"
type Bicola of elem = tuple 
				size: nat
				a: array [1...n] of elem
				fst: nat

fun empty () ret x:Bicola {
	x.size = 0;
}

fun addfst (x:Bicola,e:elem) ret x {
	if (is_empty(x)) {
		x.size++;
		x.a[fst] = e;
	}
	else if (x.size == 1) {
		x.size++;
		x.a[fst+1] = e;
		x.fst++;
	}
	else {
		x.size++;
		for i to x.size-1 do {
			i++
		}
		x.a[i] = e;
		x.fst = i; // ta raro
	}
}

fun is_empty (x:Bicola) ret b: boolean {
	if (x.size == 0) {
		b = tru();
	}
	else {
		b = fal();
	}
}

fun is_full (x:Bicola) ret b:boolean {
	if (x.size == n) {
		b = tru();
	}
	else {b = fal();}
}

fun fst (x:Bicola) ret z: elem {
	z = x.a[fst];
}

fun lst (x:Bicola) ret z: elem {
	z = x.a[n-1 + x.fst];
}

proc rmfst (x:Bicola) {
	if (!is_empty(x)) {
		x.fst++;
	}
	else {
		printf("Empty Queque\n");
	}
}

proc rmlst (x:Bicola) {
	if(!is_empty(x)){
		x.size--;
	}
	else {
		printf("Empty Queque\n");
	}
}

fun addlst (x:Bicola, e: elem) ret x {
	if (is_empty(x)) {
		x.size++;
		x.a[fst] = e;
	}
	else if (is_full(x)) {
		printf("Full Queque m8\n");
		break;
	}
	else {
		x.a[size] = e;
	}
}