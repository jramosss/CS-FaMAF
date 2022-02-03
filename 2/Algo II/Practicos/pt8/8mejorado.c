type survivor = tuple
	breathe : nat
	string : name
	saved_or_dead : bool;
end tuple;

var c : nat = 100; // oxigeno
var t : nat = 20; // tiempo que toma rescatar un wn

fun greedy (a: array[1...n] of survivor) ret x : array [1...n] of survivor {
	var cand : survivor;
	a = sort_by_breathe(a);
	while (c > 0) {
		a[i].saved_or_dead = true;
		c = c - t; 
	}
}


