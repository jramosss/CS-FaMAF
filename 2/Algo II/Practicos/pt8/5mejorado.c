type ballena = tuple 
	aire : nat;
	name : string;
end tuple;

fun greedy (S:set of ballena, globalTime : nat) ret X :set of ballena {
	while (hay_cand(S)) {
		X = X : cand(S); 
		T = T - cand(S).aire;
	}
}

fun cand (S : set of ballena) ret min_aire : ballena {
	var min_aire : nat = inf;
	for s in S do {
		if (s.aire < min_aire && s.aire >= 0) {
			min_aire = s;
		}
	}
}

fun hay_cand (S : set of ballena) ret b : bool {
	b = false;
	for s in S do {
		if (s.aire >= 0) {
			b = true;
		}
	}
}