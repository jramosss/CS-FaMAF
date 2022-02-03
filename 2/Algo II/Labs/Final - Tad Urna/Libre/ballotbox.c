#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "ballotbox.h"

struct __ballotbox_t {
    candidate_t *array;
    candidate_t length;
    unsigned int total_blank;
    unsigned int total_votes;
};

ballotbox_t ballotbox_empty(unsigned int candidates_count) {
    ballotbox_t new = calloc (1,sizeof(struct __ballotbox_t));
    new->array = calloc (candidates_count,sizeof(candidate_t));
    new->length = candidates_count;
    new->total_blank = 0;
    new->total_votes = 0;
    return new;
}

bool ballotbox_is_empty(ballotbox_t box) {
    return (box->length == 0);
}


bool in_range (ballotbox_t box, candidate_t c) {
    assert(box!=NULL);
    return (box->length > c);
}

ballotbox_t ballotbox_candidate_vote(ballotbox_t box, candidate_t c) {
    assert(box!=NULL);
    if (in_range(box,c)) {
        box->array[c]++;
        box->total_votes++;
    }
    return box;
}

ballotbox_t ballotbox_blank_vote(ballotbox_t box) {
    assert(box!=NULL);
    box->total_blank++;
    box->total_votes++;
    return box;
}

ballotbox_t ballotbox_vote_cancel(ballotbox_t box, candidate_t c) {
    assert(box!=NULL);
    if (in_range(box,c) && box->array[c] > 0) {
        box->array[c]--;
        box->total_votes--;
    }
    return box;
}

unsigned int ballotbox_total_votes(ballotbox_t box) {
    return (box->total_votes);
}

unsigned int ballotbox_blank_votes(ballotbox_t box) {
    return (box->total_blank);
}

bool ballotbox_has_votes(ballotbox_t box, candidate_t c) {
    return (box->array[c] > 0);
}

bool ballotbox_is_winner(ballotbox_t box, candidate_t c) {
    assert(box!=NULL);
    bool b = true;
    if (!ballotbox_is_empty(box)) {
        candidate_t max = 0;
        for (unsigned int i = 0; i < box->length; i++) {
            if (box->array[i] > max) {
                max = i;
            }
        }
        for (unsigned int j = 0; j < box->length; j++) {
            if (max == box->array[j] && j != c) {
                b = false;
                break;
            }
        }
    }
    return b;
}

bool ballotbox_has_winner(ballotbox_t box) {
    bool b = false;
    for (unsigned int i = 0; i < box->length; i++) {
        if (ballotbox_is_winner(box,i)) {
            b = true;
            break;
        }
    }
    return b;
}

candidate_t ballotbox_who_won (ballotbox_t box) {
    candidate_t c = 0;
    if (ballotbox_has_winner(box)) {
        for (unsigned int i = 0; i < box->length; i++) {
            if (ballotbox_is_winner(box,i)) {
                c = i;
            }
        }
    }
    return c;
}

ballotbox_t max_boxx (ballotbox_t box1, ballotbox_t box2) {
    if (box1->length > box2 ->length) {
        return box1;
    }
    else {
        return box2;
    }
}

ballotbox_t min_boxx (ballotbox_t box1, ballotbox_t box2) {
    if (box1->length < box2->length) {
        return box1;
    }
    else {
        return box2;
    }
}

ballotbox_t ballotbox_join(ballotbox_t box1, ballotbox_t box2) {
    assert(box1!=NULL);
    assert(box2!=NULL);
    ballotbox_t max_box = max_boxx(box1,box2);
    ballotbox_t min_box = min_boxx(box1,box2);
    ballotbox_t box3 = ballotbox_empty(max_box->length);
    box3->length = max_box->length;
    box3->total_votes = box1->total_votes + box2->total_votes;
    box3->total_blank = box1->total_blank + box2->total_blank;
    for (unsigned int i = 0; i < min_box->length; i++) {
        box3->array[i] = max_box->array[i] + min_box->array[i];
    }
    for (unsigned int j = min_box->length; j < max_box->length; j++) {
        box3->array[j] = max_box->array[j];
    }
    return box3;
}

void ballotbox_print(ballotbox_t box) {
    assert(box!=NULL);
    if (ballotbox_is_empty(box)) {
        printf("Empty Ballot box\n");
    }
    else {
        printf("Total Votes: %d\n", box->total_votes);
        printf("There's %d blank votes\n",box->total_blank);
        printf("There's %d candidates\n", box->length);
        for (unsigned int i = 0; i < box->length; i++) {
            printf("Candidate %d has %d votes\n", i,box->array[i]);
        }
    }
}

ballotbox_t ballotbox_destroy(ballotbox_t box) {
    if (box != NULL) {
        free(box->array);
        free(box);
        box = NULL;
    }
    return box;
}

unsigned int ballotbox_max_vote_count(ballotbox_t box) {
    assert(box!=NULL);
    candidate_t max = 0;
    for (unsigned int i = 0; i < box->length; i++) {
        if (max < box->array[i]) {
            max = i;
        } 
    }
    return (box->array[max]);
}

unsigned int ballotbox_best_candidates_count(ballotbox_t box) {
    assert(box!=NULL);
    unsigned int n = 0;
    if (!ballotbox_is_empty(box)) {
        candidate_t max = 0;
        if (ballotbox_has_winner(box)){
            n = 1;
        }
        else {
            for (unsigned int i = 0; i < box->length;i++) {
                if (max < box->array[i]) {
                    max = i;
                }
            }
            for (unsigned int j = 0; j < box->length;j++) {
                if (max == box->array[j]) {
                    n++;
                }
            }
        }
    }
    return n;
}

candidate_t *ballotbox_best_candidates(ballotbox_t box){
    assert (box != NULL);
    candidate_t *a = calloc(ballotbox_best_candidates_count(box),sizeof(candidate_t));
    if (!ballotbox_is_empty(box)) {
        candidate_t max = 0;
        for (unsigned int i = 0; i < box->array[i]; i++) {
            if (max < box->array[i]) {
                max = i;
            }
        }
        for (unsigned int i = 0; i < box->length; i++) {
            if (max == box->array[i]) {
                a[i] = box->array[i];
            }
        }
    }
    return a;
}

void array_print (candidate_t *a, unsigned int length) { 
    printf("[");
    for (unsigned int i = 0; i < length; i++) {
        printf("%d",a[i]);
    }
    printf("]\n");
}