type localidad = next_localidad
type next_localidad : nat

fun greedy (a: array [1...n] of localidad, autonomia : nat) ret cargas : nat{
	cargas = 1;
	for i = 1 to n do {
		if (autonomia > 0) {
			autonomia = autonomia - a[i]->next_localidad;
		}
		else {
			cargas++;
		}
	}
	return cargas;
}