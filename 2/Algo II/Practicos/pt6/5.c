typedef Conjunto = list of int

Conjunto empty () {
	Conjunto x = [];
	return x;
}

bool is_empty () {
return (x == []);
}

void add (elem e, Conjunto x){
	for (unsigned int i = 0; i < length x; i++) {
		if (x!i != e) {
			for (unsigned int j = 0; j < length x; j++) {
				if (e < x!j) {
					j++
				}
				else {
					take (x,j) : e : c;
				}
			}
		}
	}
}

void union (Conjunto x, Conjunto y) { // Falta ordenar
	for (unsigned int i = 0; i < length x; i++){
		add (x!i,y);
	}
}

void cross (Conjunto x, Conjunto y) {
	Conjunto z = empty (); 
	for (unsigned int i = 0; i < length x; i++) {
		for (unsigned int j = 0; j < length y; j++){
			if (x!i == y!j){
				add (x,z);
			}
		}
	}
}

bool elem (elem e, Conjunto x) {
	bool b = true;
	for (unsigned int i = 0; i < length x; i++) {
		if (x!i == e) {
			b = true;
		}
	}
	return b;
}