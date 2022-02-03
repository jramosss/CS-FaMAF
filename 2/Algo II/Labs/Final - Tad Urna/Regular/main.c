#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include "ballotbox.h"

int main(){
	ballotbox_t box = ballotbox_empty(3);
	ballotbox_t box1 = ballotbox_empty(2);
	ballotbox_t box2 = ballotbox_empty(0);
	box = ballotbox_candidate_vote(box,0);
	box = ballotbox_candidate_vote(box,0); 
	box = ballotbox_candidate_vote(box,0);// 3 votos para 0
	box = ballotbox_candidate_vote(box,1); // 1 voto para 1
	box = ballotbox_vote_cancel(box,0);		//anulo un voto para 0 en box
	box = ballotbox_blank_vote(box);		// voto en blanco
	box1 = ballotbox_candidate_vote(box1,0); // 1 voto para 0 en box1
	box1 = ballotbox_candidate_vote(box1,1); // 1 voto para 1 en box1

	if (ballotbox_has_votes(box,1)){
		printf("Candidate 1 has votes\n");
	}
	else {
		printf("Candidate 1 doesn't have any vote\n");
	}
	if (ballotbox_is_empty(box)) {
		printf("Ballotbox is empty\n");
	}
	else {
		printf("Ballotbox is not empty\n");
	}
	if (ballotbox_is_empty(box1)) {
		printf("Ballotbox1 is empty\n");
	}
	else {
		printf("Ballotbox1 is not empty\n");
	}
	if (ballotbox_is_empty(box2)){
		printf("Ballotbox2 Is Empty\n");
	}
	else {
		printf("Ballotbox2 is not empty\n");

	}
	if (ballotbox_has_winner(box)) {
		printf("We have a winner!: Candidate %d \n",who_won(box));
	}
	else {
		printf("We don't have a winner :(\n");
	}
	printf("There's %d votes \n", ballotbox_total_votes(box));
	printf("There's %d blank votes\n", ballotbox_blank_votes(box));

	ballotbox_print(box);
	box2 = ballotbox_join(box,box1);
	printf("/////////////////////////////////JOIN////////////////////////////////////////////////////////////\n");
	ballotbox_print(box2);
	box = ballotbox_destroy(box);
	box1 = ballotbox_destroy(box1);
	box2 = ballotbox_destroy(box2);
	return 0;
}
