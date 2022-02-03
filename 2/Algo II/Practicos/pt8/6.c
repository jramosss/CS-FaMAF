typedef amigo = tuple 
		unsigned int partida
		unsigned int regreso

list of amigo greedy (list of amigo X) {
	y = list_copy (X);
	for (unsigned int i = 0; i < length (X); i++) {
		for (unsigned int j = 1; j < length(X); j++){
			if ((X!i)->partida > (X!j)->regreso) {
				y = x : y;
				X = x - X;
			}
		}
	}
	return y;
}


