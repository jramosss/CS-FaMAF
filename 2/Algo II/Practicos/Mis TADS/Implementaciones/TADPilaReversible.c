type stack = tuple
		a: array [1...n] of int
		size: nat
end tuple;

fun is_full (x:stack) ret b: bool {
	b = (x.size == n)
}

fun is_empty (x: stack) ret b: bool {
	b = (x.size == 0);
}

proc push (x:stack,e:elem) {
	x.size++;
	x.a[size] = e;
}

fun lst (x:stack) ret e:elem {
	e = x.a[size];
}
 
fun fst (x:stack) ret e: elem {
	e = a[1];
}

proc pop (x:stack) {
	x.size--;
}