type amigo = tuple 
	partida : nat;
	regreso : nat;
	name : string;
	usado_o_revisado : bool;
end tuple;

fun greedy (a: array [1...n] of amigo) {
	var cand : amigo;
	for i = 1 to n do {
		cand = cand(a,i);
		descartar(a,cand);
		a[cand].usado_o_revisado = true;
	}
}

fun cand (a: array [1...n] of amigo, j : nat) ret cand : amigo {
	var min : nat = inf;
	for i = j to n do {
		if (a[i].regreso < min && !a[i].usado_o_revisado) {
			min = a[i].regreso;
			cand = i;
		}
	}
}

proc descartar (in/out a: array [1...n] of amigo, x: amigo) {
	for i = 1 to n do {
		if (a[i].partida < a[x].regreso) {
			a[i].usado_o_revisado = true;
		}
	}
}