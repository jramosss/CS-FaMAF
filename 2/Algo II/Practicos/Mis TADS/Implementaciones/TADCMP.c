type node = tuple
	unit: (coff,ch,cup)
	next: pointer to node
end tuple;
type machine = pointer to node;

proc empty (x: machine) {
	x = NULL;
}

fun is_empty (x:machine) ret b:bool {
	b = (x == NULL);
}
fun has_coff (x: machine) ret b: bool {
	b = (x.unit[0] != NULL);
}

fun has_ch (x:machine) ret b: bool {
	b = (x.unit[1] != NULL);
}

fun has_cup (x:machine) ret b:bool{
	b = (x.unit[2] != NULL);
}

fun cuanto_cafe (x:machine) ret n:nat {
	 
}

