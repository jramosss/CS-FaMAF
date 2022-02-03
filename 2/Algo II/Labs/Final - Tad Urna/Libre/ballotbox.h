#ifndef __BALLOTBOX_H__
#define __BALLOTBOX_H__
#include <stdbool.h>

typedef struct __ballotbox_t *ballotbox_t;
typedef unsigned int candidate_t;

/**
* Creates an empty ballot box for the given amount of candidates
* @param candidates_count amount of candidates
**/
ballotbox_t ballotbox_empty(unsigned int candidates_count);

/**
* Add a new vote to the given candidate
* @param box a ballot box
* @param c a valid candidate
* @return the modified box
**/
ballotbox_t ballotbox_candidate_vote(ballotbox_t box, candidate_t c);

/**
* Add a blank vote
* @param box a ballot box
* @return the modified box
**/
ballotbox_t ballotbox_blank_vote(ballotbox_t box);

/**
* Cancel a vote to the given candidate
* @param box a ballot box
* @param c a valid candidate
* @note if the candidate has no votes, this function does nothing.
*/
ballotbox_t ballotbox_vote_cancel(ballotbox_t box, candidate_t c);

/**
* Check if the ballot box has some votes
* @param box a ballot box
**/
bool ballotbox_is_empty(ballotbox_t box);

/**
* Return the amount of votes inside the given ballot box
* @param box a ballot box
**/
unsigned int ballotbox_total_votes(ballotbox_t box);

/**
* Return the amount of blank votes inside the given ballot box
* @param box a ballot box
**/
unsigned int ballotbox_blank_votes(ballotbox_t box);

/**
* Check if the given candidate has some votes
* @param box a ballot box
* @param c a valid candidate
**/
bool ballotbox_has_votes(ballotbox_t box, candidate_t c);

/**
* Check if the given candidate is the (unique) winner of the election
* The 'winner' candidate has strictly more votes than all the others
* @param box a ballot box
* @param c a valid candidate
**/
bool ballotbox_is_winner(ballotbox_t box, candidate_t c);

/**
* Check if the given ballot box has a (unique) winner
* @param box a ballot box
**/
bool ballotbox_has_winner(ballotbox_t box);

candidate_t ballotbox_who_won (ballotbox_t box);

/**
* Join two ballot boxes into one single box
* @param box1 a ballot box
* @param box2 a ballot box
* @return a new ballot box counting votes of box1 and box2 taking
* into account all candidates of both boxes and blank votes
* @note does NOT modify box1 nor box2
* @note does NOT assume both boxes have the same amount of candidates
**/
ballotbox_t ballotbox_join(ballotbox_t box1, ballotbox_t box2);

/**
* Print a ballot box
* @param box a ballot box
**/
void ballotbox_print(ballotbox_t box);

/**
* Destroy the given ballot box1
* @param box a ballot box
**/
ballotbox_t ballotbox_destroy(ballotbox_t box);

/**
* Return the maximum amount of votes obtained by some candidate
* @param box a ballot box1
**/
unsigned int ballotbox_max_vote_count(ballotbox_t box);

/**
* Return the amount of 'best' candidates there are in the given box.
* A 'best' candidate has no less votes than any other candidate
* @param box a ballot box
**/
unsigned int ballotbox_best_candidates_count(ballotbox_t box);

/**
* Return an array with the 'best' candidates of the given box
* @param box a ballot box
* @note the returned array has size ballotbox_best_candidates_count(box)
**/
candidate_t *ballotbox_best_candidates(ballotbox_t box);

void array_print (candidate_t *a, unsigned int length); 

#endif
