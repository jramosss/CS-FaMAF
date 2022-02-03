type cuarto = tuple 
	numero : nat
	desayunos : nat
	done : bool
end tuple 

fun greedy (a: array [1...n] of cuarto) ret x : array [1...n] of cuarto {
	var cand : cuarto
	var cant_desayunos : nat
	var i : nat = 0
	while (!all_done(a)) {
		cant_desayunos = 5
		while (cant_desayunos > 0) {
			cand = cand(a)
			if (cand.desayunos <= cant_desayunos) {
				cand.desayunos = 0
				cand.done = true
				cant_desayunos = cant_desayunos - cand.desayunos
			}
			else {
				cant_desayunos = 0
				cand.desayunos = cant_desayunos - cant_desayunos
			}
		}
		x[i] = cand
	}
}

fun cand (a: array [1...n] of cuarto) ret cand : cuarto {
	var min : nat = inf
	for i = 1 to n do {
		if (a[i].numero < min && !a[i].done) {
			min = a[i].numero
			cand = i
		}
	}
}

fun all_done (a : array [1...n] of cuarto) ret b: bool {
	b = true
	for i = 1 to n do {
		if (a[i].desayunos != 0) {
			b = false
		}
	}
}