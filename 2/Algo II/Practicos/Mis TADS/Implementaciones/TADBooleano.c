type boolean = nat

fun tru () ret x: nat{
	x = 1;
}

fun Fal () ret x: nat {
	x = 0;
}

fun conjuncion (x: boolean,y: boolean) ret b: boolean {
	if (x == tru() && y == tru()){
		b = tru();
	}
	else {b=fal();}
}

fun disyuncion (x:boolean,y:boolean) ret b: boolean {
	if (x == tru() || y == tru()){
		b = tru();
	}
	else {b=fal();}
}

fun implica (x:boolean,y:boolean) ret b:boolean {
	if (x == tru() && y == fal()) {
		b = fal();
	}
	else {b=tru();}
}

fun eq (x:boolean,y:boolean) ret b:boolean {
	if ((x == tru() && y == tru()) || (x==fal() && y==fal())) {
		b = tru();
	} 
	else { b= fal();}
}

fun not (x:boolean) ret x {
	if (x==tru()) {
		x = fal();
	}
	else { 
		x = tru();
	}
}

fun and (x:boolean, y:boolean) ret b:boolean {
	b = x*y
}

fun or (x:boolean,y:boolean) ret b: boolean {
	b = x + y
}