type node = tuple 
		value: elem
		next: pointer to node
		prev: pointer to node
end tuple
type bqueque = pointer to node

fun empty () ret x: bqueque {
	x = NULL;
}

fun is_empty (x:bqueque) ret b: bool {
	b = (x == NULL);
}

fun is_full (x:bqueque) ret b: bool {
	//?
}

fun lst (x:bqueque) ret e: elem {
	x = x.prev;
	e = x.value;
}

fun addfst (x:bqueque, e: elem) ret x {
	var c: pointer to node = x;
	var v: elem;
	var n: pointer to node = x.next;
	v = n.value
	x.value = e
	while (c != c.next) {
		v = n.value;
		n = n.next;
		x.value = v
	}
}

fun rmfst (x:bqueque) ret x {
	var n: pointer to node = x;
	while (c != c.next){
		c = c.next;
	}
	n = c.next;
	x = n.next;
}

fun rmlst (x:bqueque) ret x {  
	x.prev--;
	free (x.prev.next)
}

fun addlst (x:bqueque,e:elem) ret x  {
	
}