typedef localidad = list of unsigned int
unsigned int  autonomia
unsigned int  distancia

list of localidad dondeCargar (list of localidad X) { // esto me dice donde cargo
	list of localidad c;
	while (autonomia != 0) {
		if (localidad.distancia >= localidad.autonomia) {
			C = x : C;
			X = X-x;
		}
	}
	C = x : dondeCargar(X);
return C;
}

unsigned int cargar (localidad x) { // esto me dice cuanto cargo
	unsigned int cargar;
	for (unsigned int i = 0; i < length(localidad); i++) {
		if (localidad.autonomia <= localidad.distancia) {
			cargar++;
		}
	}
return cargar; // 1 + cargar?? ya que el tanque inicialmente esta vacio
}
