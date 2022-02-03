#include "dna.h"
#include <stdio.h>
#include <stdlib.h>

#ifndef TEST

int main() {
	dna_t t1 = dna_T();
	dna_t t2 = dna_T();
	dna_t t3 = dna_C();			
	dna_t *cut;
	t1 = dna_join(t1,dna_G());
	t1 = dna_join(t1,dna_C());
	t1 = dna_join(t1,dna_C());
	t1 = dna_join(t1,dna_G());
	t1 = dna_join(t1,dna_A());
	t1 = dna_join(t1,dna_C()); // t1 = TGCGAC
 
	t2 = dna_join(t2,dna_G());
	t2 = dna_join(t2,dna_C()); // t2 = TGC

	t3 = dna_join(t3,dna_A());
	t3 = dna_join(t3,dna_T()); // t3 = CAT

	printf("t1 : ");
	dna_print(t1);
	printf("t2 : ");
	dna_print(t2);
	printf("t3 : ");
	dna_print(t3);
	printf("t1 length = %d\n", dna_length(t1));
	printf("t2 length = %d\n", dna_length(t2));
	printf("t3 length = %d\n", dna_length(t3));
	if (dna_is_prefix(t2,t1)) { 			//SI
		printf("t2 is prefix of t1\n");
	}
	else {
		printf("t2 is not prefix of t1\n");
	}
	if (dna_is_prefix(t3,t1)) {				// NO
		printf("t3 is prefix of t1\n");
	}
	else {
		printf("t3 is not prefix of t1\n");
	}
	if (dna_is_equal(t2,t1)) {
		printf("t1 and t2 are equal\n");
	}
	else {
		printf("t1 is different of t2\n");
	}
	cut = dna_cut(t1,4);
	printf("Cut 3 t1 : \n");
	printf("Part 1 : ");
	dna_print(cut[1]);
	printf("Part 2 : ");
	dna_print(cut[0]);
	t1 = dna_destroy(t1);
	t2 = dna_destroy(t2);
	t3 = dna_destroy(t3);
	free(cut);
    return (0);
}

#endif
