#include <stdlib.h>
#include <stdio.h>
#include "ballotbox.h"

#ifndef TEST

int main() {
	//Creo una urna
	ballotbox_t box1 = ballotbox_empty(5);
	ballotbox_t box2 = ballotbox_empty(3);
	ballotbox_t box3 = ballotbox_empty(0);
	box1 = ballotbox_candidate_vote(box1,1);
	box1 = ballotbox_candidate_vote(box1,1);
	box1 = ballotbox_candidate_vote(box1,1); // 3 votos para 1 en box1, [3,0,0,0,0]
	box2 = ballotbox_candidate_vote(box2,2);
	box2 = ballotbox_candidate_vote(box2,2);
	box2 = ballotbox_candidate_vote(box2,2);
	box2 = ballotbox_candidate_vote(box2,2);
	box2 = ballotbox_candidate_vote(box2,2); // 5 votos para 2 en box2, [0,5,0,0,0]
	box2 = ballotbox_candidate_vote(box2,1); // 1 voto para 1 en box2,  [1,5,0,0,0]
 	box2 = ballotbox_vote_cancel(box2,2);    // anulo un voto para 2 en box2 [1,4,0,0,0]
 	box1 = ballotbox_blank_vote(box1); 	 	 // voto en blanco para box1

 	if (ballotbox_is_empty(box1)) {
 		printf("Ballot box 1 is empty\n");
 	}
 	else {
 		printf("Ballot box 1 is not empty\n");
 	}
 	if (ballotbox_is_empty(box2)) {
 		printf("Ballot box 2 is empty\n");
 	}
 	else {
 		printf("Ballot box 2 is not empty\n");
 	}
 	if (ballotbox_is_empty(box3)) {
 		printf("Ballot box 3 is empty\n");
 	}
 	else {
 		printf("Ballot box 3 is not empty\n");
 	}
 	printf("Candidate 1 %s\n", ballotbox_has_votes(box1,1) ? "has votes in box 1" : "does not have votes in box1");
 	if (ballotbox_has_winner(box1)) {
 		printf("Box1 has winner!, Candidate %d\n", ballotbox_who_won(box1));
 	}
 	else {
 		printf("Box1 doesn't have a winner :(\n");
 	}
 	if (ballotbox_has_winner(box2)) {
 		printf("Box2 has winner!, Candidate %d\n", ballotbox_who_won(box2));
 	}
 	else {
 		printf("Box2 doesn't have a winner :(\n");
 	}
 	if (ballotbox_has_winner(box1)) {
 		printf("Box3 has winner!, Candidate %d\n", ballotbox_who_won(box3));
 	}
 	else {
 		printf("Box3 doesn't have a winner :(\n");
 	}

 	printf("Box 1: \n");
 	ballotbox_print(box1);
 	printf("Box 2: \n");
 	ballotbox_print(box2);
 	printf("Box 3: \n");
    ballotbox_print(box3);
    printf("Join box1, box2: \n");
    box3 = ballotbox_join(box1,box2);
    ballotbox_print(box3);
    printf("El mayor numero de votos en box 2 es %d\n", ballotbox_max_vote_count(box2));
    if (!ballotbox_has_winner(box2)) { 
    	printf("Hay %d candidatos peleando por la victoria en box 2 ndeaah\n", ballotbox_best_candidates_count(box2));
    }
    else {
    	printf("Hay un unico ganador en la urna 2\n");	
    }
    if (!ballotbox_has_winner(box2)) {
	    printf("Top Candidatos: \n");
	    array_print(ballotbox_best_candidates(box2),ballotbox_best_candidates_count(box2));
    }
    box1 = ballotbox_destroy(box1);
    box2 = ballotbox_destroy(box2);
    box3 = ballotbox_destroy(box3);
	return 0;
}

#endif

/*
gcc -Wall -Werror -Wextra -pedantic -std=c99 -c ballotbox.c main.c
gcc -Wall -Werror -Wextra -pedantic -std=c99 -o ballotbox main.o ballotbox.o
./ballotbox
*/
