Vos y tus amigos, en total son m personas, se quieren ir de viaje por el fin de semana y tienen a
disposici´on n autos, cada uno con capacidad para llevar una cantidad de personas c1, . . . , cn. ¿Cu´al es
la menor cantidad de autos necesaria para que las m personas puedan viajar?

type autos = array [1...n] of auto

type auto = tuple
	unsigned int capacidad;
	unsigned int personas;

fun voraz (autos[i]) ret x unsigned int
	unsigned int i = 0;
	while (autos[i].capacidad != 0 && personas != 0)
		capacidad--;
		personas--;
	od
	cantidad_autos++;
	i++;
	x = cantidad_autos + voraz(autos[i])
