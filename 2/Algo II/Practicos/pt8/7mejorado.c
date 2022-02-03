type pieza = tuple 
	quemado : nat;
	cocido : nat;
	name : string;
	done : bool;
end tuple;

var T : nat = 0;

void greedy (in /out a : array [1...n] of pieza) {
	var cand : pieza;
	var i : nat;
	while (hay_cand(a)) {
		while (!algo_se_quema(a)) {
			i++;
			T++;
		}
		descartar(a);
	}

}

fun algo_se_quema (a: array [1...n] of pieza) ret b : bool {
	b = false;
	for i = 1 to n do {
		if (a[i]. quemado == T) {
			b = true;
		}
	}
}

fun hay_cand (a: array[1...n] of pieza) ret b : bool {
	b = false;
	for i = 1 to n do {
		if (!a[i].done) {
			b = true;
		}
	}
}

void descartar (a: array [1...n] of pieza) {
	for i = 1 to n do {
		if (a[i].quemado < T && a[i].cocido > T) {
			a[i].done = true;
		}
	}
}