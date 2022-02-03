typedef CM = counter tuple
			 counter cup;
			 counter choc;
			 counter coff:	
void init () {
	Init cup; 
	Init choc; 
	Init coff;
} 

bool is_init (CM x) {
	return (cup == Init && choc == Init && coff == Init);
}

void addCup (CM x) {
	Increase (x.cup);
}

void addCoff (CM x) {
	Increase (x.coff);
}

void addChoc (CM x) {
	Increase (x.choc);
}

bool has_Coff (CM x) {
	return (coff != Init);
}

bool has_Cup (CM x) {
	return (cup != Init);
}

bool has_Choc (CM x) {
	return (choc != Init);
}

