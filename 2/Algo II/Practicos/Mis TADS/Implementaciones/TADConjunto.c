type conjunto = list;


fun is_empty (x: conjunto) ret b: bool {
	b = (x == []);
}

fun existe (x:conjunto, e: elem) ret b:bool {
	 while (x!i != e) {
		i++;
	 }
	 b = (x!i == e);
}

proc add (x:conjunto,e:elem) {
	i: nat = 1;
	if (is_empty(x)){
		p = e : x
	}
	else if (existe(x,e)) {
		break;
	}
	else {
		while (x!i < e) {
			i++;
		}
		x = take (i,x) : e : x
	}
}

proc union (x:conjunto, y:conjunto) {
	i:nat = 1;
	while (!(is_empty(x))) {
		add(y,x!i);
		i++;
	}
}

fun interseccion (x: conjunto, y:conjunto) ret z: conjunto {
	while (!is_empty(x)){
		if (existe(x!i,e)){
			add(x!i,z);
		}
	}
}