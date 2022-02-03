#define lli long long int

lli ej3 (lli i, lli j, lli k, lli N) {
	if (i==N | j==N) {
		k = 2;
	} else {
		k++;
	}
	return(k);
}


