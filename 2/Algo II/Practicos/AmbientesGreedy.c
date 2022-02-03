type ambiente = tuple
	tiempo : nat;
	valor: nat;
	revisado : bool;
end tuple;

fun greedy (a: array[1...n] of ambiente, T: nat) ret v: valor{
	cand : ambiente;
	while (existe_candidato(a,T)) {
		cand = ambiente_candidato(a,T);
		T = T - cand.tiempo;
		cand.revisado = true; 
		v = v + cand.valor;
	}
	if (T > 0) {
		cand = ultimo_cand(a);
		T = T - cand.tiempo / T;
		v = v + cand.valor/T;
	}
}

fun ambiente_candidato (a: array[1...n] of ambiente,T:nat) ret cand : ambiente{
	unsigned int max = -inf;
	for i = 1 to n do {
		if (a[i].tiempo/a[i].valor > max && a[i].tiempo < T && !a[i].revisado) {
			max = a[i].tiempo/a[i].valor;
			cand = i;
		}
	}
}

fun existe_candidato (a:array [1...n] of ambiente, T: nat) ret b: bool {
	b = false;
	for i = 1 to n do {
		if (a[i].tiempo < T && !a[i].revisado) {
			b = true;
		}
	}
}

fun ultimo_cand (a: array [1...n] of ambiente) ret lastcand : ambiente {
	unsigned int max = -inf;
	for i = 1 to n do {
		if (a[i].valor > max && !a[i].revisado) {
			lastcand = i;
		}
	}
}