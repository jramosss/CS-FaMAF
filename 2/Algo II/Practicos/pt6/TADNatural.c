typedef Nat = unsigned int;

fun cero () {
	unsigned int b = 0;
	return b;
}

fun sucesor (Nat x) {
	return x++:
}

fun mas (Nat x, Nat y){
	unsigned int r=0;
	if (x == 0) {
		r = y;
	}
	else if (y == 0) {
		r = x;
	}
	else {
		r = sucesor (mas(x-1,y-1));
	}
	return r;
}

fun por (Nat x, Nat y) {
	unsigned int r = 0;
	if (x==0 || y ==0) {
		r = 0;
	}
	else if (x == 1) {
		r = y;
	}
	else if (y == 1) {
		r = x;
	}
	else {
		r = por()
	}
}