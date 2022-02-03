#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "player.h"


unsigned int player_count_by_age(unsigned int age, player_t atp[], unsigned int length) {
	unsigned int c = 0;
    assert (atp!=NULL);
    for (unsigned int i = 0; i < length; i++){
    	if((atp[i] -> age) == age){
    		c++;
    	}
    }
return c;
}

player_t get_one_player_by_rank(unsigned int rank, player_t atp[], unsigned int length) {
	player_t thechosenone = NULL;
    assert (atp!=NULL);
    for (unsigned int i = 0; i < length; i++) {
    	if ((atp[i] -> rank) == rank){
    		thechosenone = atp[i];
    		break;
    	}
    }
return thechosenone;
}
