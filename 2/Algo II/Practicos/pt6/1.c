typedef boolean = unsigned int
// True = 1
// False = 0

boolean Verdadero () {
	b = 1;
return b;
}

boolean Falso () {
	b = 0;
return b;
}

boolean not (boolean b) {
	if (b == Falso()) {
		b = Verdadero ();
	}
	else {
		b = Falso();	
	}
return b;
}

boolean and (a boolean, b boolean, c boolean) {
	b = a*c;
return b;
}

unsigned int absoluto (int x) {
	if (x>0) {
		x = x;
	}
	else {
		x = -x;
	}
return x;
}

boolean or (boolean a, boolean b, boolean c) {
	if (and(a,c) == 1){
		b = 1;
	}
	else {
		b = absoluto (a-c);
	}
return b;
}

boolean implica (boolean a, boolean b, boolean c) {
	if (a == Verdadero() && c == Falso()) {
			b = Falso ();
	}
	else {
		b = Verdadero ();
	}
return b;
}