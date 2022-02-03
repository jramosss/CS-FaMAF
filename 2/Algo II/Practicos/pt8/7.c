typedef piezas = tuple {
		unsigned int cocido;
		unsigned int quemado;
		unsigned int time;
}

fun sacar_cocidas (list of piezas X) {
	set of piezas listas = vacio;
	for (unsigned int i = 0; i < length(X); i++)
		while (x!i -> time != quemado) {
			if (x!i < quemado && x! > cocido) {
				listas = x : listas;
			}
			time++;
		}
	X = X - listas;
}