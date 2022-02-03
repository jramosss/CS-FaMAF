/*
  @file sort.c
  @brief sort functions implementation
*/

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include "helpers.h"
#include "sort.h"
#include "player.h"

void swap (player_t a[],unsigned int i,unsigned int j) {
        player_t tmp;
        tmp = a[i];
        a[i] = a[j];
        a[j] = tmp;
}

int min_pos_from (player_t a[],unsigned int length, int i) {
        int min = i;
        for (unsigned int j=i+1 ; j<length; j++){
                        if ((a[j].rank)<a[min].rank) {
                                min=j;
                               }
         }
         return min;
     }

void selection_sort(player_t a[], unsigned int length) {
        unsigned int minp;
        for (unsigned int i=0; i+1<length; i++ ){
                minp = min_pos_from (a,length,i);
                swap(a,i,minp);
        }
}

bool goes_before(player_t x, player_t y){
        bool siono; 
        if(x.rank < y.rank) {
            siono = true;
        }
return siono;
}

bool array_is_sorted(player_t atp[], unsigned int length) {
    unsigned int i = 1u;
    while (i < length && goes_before(atp[i - 1u], atp[i])) {
        i++;
    }
    return (i == length);
}

void sort(player_t a[], unsigned int length) {
        selection_sort(a,length);
}

