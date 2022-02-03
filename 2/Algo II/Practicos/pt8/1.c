/*
fun voraz (C) ret s {
	{C : Conjunto de candidatos, S : solucion a construir}
	S = {};
	do (S no es solucion) {
		c = seleccionar de C
		C = C - {c}
		if (S Union {c} es factible) {
			S = S Union {c}
		}
	}
}
*/
array mochila (array [1...n] of valor v, array [1...n] of peso w, peso w, peso W) {
	nat c = 0;
	peso resto;
	array [1...n] of real s;
	for (unsigned int i = 0; i < n; i++) {
		resto = w;
		c = 1;
		while (w [c] <= resto) {
			s [c] = 1;
			resto = resto - w[c];
			c++;
		}
		s[c] = resto / w[c];
	}
	return s;
}