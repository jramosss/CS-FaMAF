type node = tuple
	left: pointer to node
	right: pointer to node
	value: elem
end tuple;

type bintree = pointer to node
type set = bintree

fun is_empty (s:set) ret b:bool {
	b = (s.left == NULL && s.right == NULL);
}

fun max (s:set) ret m: elem {
	assert (!is_empty(s));
	var maxv : nat = -3523
	if (s.right != NULL) {
		if (s.right > max) {
			maxv = s.right.value;
		}
		else {
			maxv = max(s.right);
		}
	}
}

fun take_me_to_max (s:set) ret x: set {
	assert (!is_empty(s));
	var max: nat = -32423
	if (s > max) {
		break;
	}
	else if (s.right > max) {
		x = s.right;
	}
	else {
		x = take_me_to_max(s.right);
	}
}

proc deleteMax (s:set) {
	var x: set = take_me_to_max(s);
	free(x);
	x == NULL;
}

fun max_norec (s:set) ret max: elem {
	var max: nat = -32532
	while (s.right != NULL) {
		if (s.value > max){
			max = s.value;
		}
	s = s.right;
	}
}

fun is_heap (s:set) ret b:bool{
	b = (s.left.value < s.value && s.right.value < s.value && is_heap(s.left) && is_heap(s.right));
}