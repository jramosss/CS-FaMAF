#include <stdio.h>

void swap (int a[],unsigned int i,unsigned int j) {
        int tmp=0;
        tmp = a[i];
        a[i] = a[j];
        a[j] = tmp;
}

unsigned int q (int a[],unsigned int i,unsigned int length) {
	unsigned int m;
	unsigned int k;
	unsigned int j;
	j = i;
	if (i % 2 == 0) {
		m = a[i];
		k = i+2;
		while (k <= length) {
			if (a[k] < m) {
				m = a[k];
				j = k;
			}
			k = j+2;
		}
	}
	return j;
}

void p (int a[],unsigned int length) {
	unsigned int i = 1;
	while (i <= length) {
		swap (a,i,(q(a,i,length)));
		i++;
	}
}


int main () {
	p(a,3);
	return 0;
}