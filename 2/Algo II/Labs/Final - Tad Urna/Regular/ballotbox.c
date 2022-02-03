#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "ballotbox.h"

struct __ballotbox_t {
    unsigned int *votes;
    unsigned int candidates_count;
    unsigned int blank_votes_count;
    unsigned int total_votes_count;
};

ballotbox_t ballotbox_empty (unsigned int candidates_count) {
    ballotbox_t u = calloc (1,sizeof (struct __ballotbox_t));
    u->votes = calloc (candidates_count,sizeof (unsigned int));
    u->candidates_count = candidates_count;
    u->blank_votes_count = 0;
    u->total_votes_count = 0;
    return u;
}

bool candidate_is_in_range (ballotbox_t box, candidate_t c) {
    assert(box != NULL);
    bool b = false;
    for (unsigned int i = 0; i < box->candidates_count; i++) {
        if (i == c) {
            b = true;
        }
    }
    return b;
}

ballotbox_t ballotbox_candidate_vote(ballotbox_t box, candidate_t c) {
    assert(box != NULL);
    if (candidate_is_in_range(box,c)){
        box->total_votes_count++;
        box->votes[c]++;
    }
    return box;
}

ballotbox_t ballotbox_blank_vote(ballotbox_t box) {
    assert(box != NULL);
    box->blank_votes_count++;
    box->total_votes_count++;
    return box;
}

ballotbox_t ballotbox_vote_cancel(ballotbox_t box, candidate_t c) {
    assert(box != NULL);
    if (candidate_is_in_range(box,c) && box->votes[c] > 0) {
        box->votes[c]--;
        box->total_votes_count--;
    }
    return box;
}
/*
bool ballotbox_is_empty(ballotbox_t box) {
    bool b = true;
    for (unsigned int i = 0; i < box->candidates_count;i++) {
         if (box->votes[i] == 0) {
            b = true;
         }
         else {
            b = false;
            break;
         }
    }
    if (box->candidates_count == 0 && box->total_votes_count == 0 && box->blank_votes_count == 0 && b) {
        b = true;
    }
    return b;
}
Mi version de is_empty
*/

bool ballotbox_is_empty (ballotbox_t box){
    assert(box != NULL);
    return (box->total_votes_count == 0);
}

unsigned int ballotbox_total_votes(ballotbox_t box) {
    assert(box != NULL);
    return (box->total_votes_count);
}

unsigned int ballotbox_blank_votes(ballotbox_t box) {
    assert(box != NULL);
    return (box->blank_votes_count);
}

bool ballotbox_has_votes(ballotbox_t box, candidate_t c) {
    assert(box != NULL);
    bool b = false;
    if (candidate_is_in_range(box,c)) {
        b = box->votes[c] > 0;
    }
    return b;
}

bool ballotbox_is_winner(ballotbox_t box, candidate_t c) {
    assert(box != NULL);
    bool b = false;
    if (candidate_is_in_range(box,c)){
        for (unsigned int i = 0; i < box->candidates_count; i++) {
            if (box->votes[i] >= box->votes[c] && i != c) {
                b = false;
                break;
            }
        }
    }
    return b;
}

bool ballotbox_has_winner(ballotbox_t box) {
    assert(box != NULL);
    unsigned int max = 0;
    bool b = true;
    unsigned int maxpos = 0;
    for (unsigned int i = 0; i < box->candidates_count; i++) { // calculo el maximo
        if (box->votes[i] > max) {
            max = box->votes[i];
            maxpos = i;
        }
    }
    for (unsigned int j = 0; j < box->candidates_count;j++) {
        if (max == box->votes[j] && j != maxpos){
            b = false;
            break;
        }
    }
    return b;
}

candidate_t who_won (ballotbox_t box) {
    assert(box != NULL);
    candidate_t max = 0;
    if (!ballotbox_is_empty(box)){
        for (unsigned int i = 0; i < box->candidates_count; i++) {
            if (box->votes[i] > max) {
                max = i;
            }
        }
    }
    return max;
}

ballotbox_t max_boxx (ballotbox_t box1, ballotbox_t box2) {
    if (box1->candidates_count >= box2->candidates_count) {
        return(box1);
    }
    else {
        return(box2);
    }
}

ballotbox_t min_boxx (ballotbox_t box1, ballotbox_t box2) {
    if (box1->candidates_count < box2->candidates_count) {
        return(box1);
    }
    else {
        return(box2);
    }
}

ballotbox_t ballotbox_join(ballotbox_t box1, ballotbox_t box2) {
    assert(box1 != NULL);
    assert(box2 != NULL);
    ballotbox_t max_box = max_boxx(box1,box2);  // Usar estas variables o no da lo mismo, no genera leaks ni nada
    ballotbox_t min_box = min_boxx(box1,box2);
    ballotbox_t box3 = ballotbox_empty(max_box->candidates_count);
    box3->total_votes_count = box1->total_votes_count + box2->total_votes_count;
    box3->blank_votes_count = box1->blank_votes_count + box2->blank_votes_count;
    for (unsigned int i = 0; i < min_box->candidates_count; i++) {
        box3->votes[i] = max_box->votes[i] + min_box->votes[i];
    }
    for (unsigned int j = min_box->candidates_count; j < max_box->candidates_count;j++) {
        box3->votes[j] = max_box->votes[j];
    }
    return box3;
}

void ballotbox_print(ballotbox_t box) {
    if (box != NULL) {
        printf("Ballotbox have %d candidates, %d votes and %d blank votes\n", box->candidates_count,
        box->total_votes_count, box->blank_votes_count);
        for (unsigned int i = 0; i < box->candidates_count; i++) {
            printf("Candidate %d has %d votes \n", i, box->votes[i]);
        }
    }
    else {
        printf("Null Ballotbox\n");
    }
}

ballotbox_t ballotbox_destroy(ballotbox_t box) {
    if (box != NULL) {
        free(box->votes);
        free(box);
        box = NULL;
    }
    return box;
}
