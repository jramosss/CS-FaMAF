typedef list = array [1...n] a;
			unsigned int length;
fun vacia () {
	x->length = 0;
	return x;
}

fun pegar (elem e, list x) {
	for (unsigned int i = 1; i < x->length; i++) {
		x->a!i = x->a!(i+1)
	}
	x->a!0 = e;
	return x;
}

fun list_length (list x) {
	return x->length;
}

fun pastebehind (elem e, list x) {
	x->a!(x->length) = e;
	return x;
}

fun head (list x) {
	elem e;
	if (x == vacia()){
		e = NULL;
	}
	else {
		e = x->a!1;
	}
	return e;
}

fun tail (list x) {
	assert (x != vacia());
	for (unsigned int i = 0; i < list_length(x);i++) {
		x->a!(i+1) = x->a!i; 
	}
	x->length--;
	return x; 		
}

fun is_empty (list x){
	bool b = false;
	if (x == vacia()) {
		b = true;
	}
	else {
		b = false;
	}
	return b;
}

fun concat (list x, list y) {
	y->length += x->length;
	for (unsigned int i = 0; i<y->length; i++) {
		y=pegar(x!i,x);
	}	
	return y;
}

fun drop (list x, unsigned int p) {
	list q = vacia();
	elem print;
	for (unsigned int i = 0; i < p; i++) {
		tail(x);
	}
	printf("dropped %d \n", head(x));
	return x;
}
