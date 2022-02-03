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

bool goes_before(player_t x, player_t y){
return x -> rank < y -> rank;
} 

bool array_is_sorted(player_t atp[], unsigned int length) {
    unsigned int i = 1u;
    while (i < length && goes_before(atp[i - 1u], atp[i])) {
        i++;
    }
    return (i == length);
}

void arreglo_puntero(player_t a[], unsigned int length,player_t *pointer[]){
        player_t *k=NULL;
        for (unsigned int i = 0; i < length; ++i){
                k = &a[i];
                pointer[i] = k;
        }        
}

void arreglo_final (player_t a[], unsigned int length, player_t *pointer[]){
    for (unsigned int i=0; i<length; i++){
            a[i] = *pointer[i];
    }
}

int min_pos_from(player_t a[],unsigned int length,int i){
    int min=i;
        for (unsigned int j=(i+1); (j<length); j++) { 
                if (a[j]->rank < a[min]->rank) {
                        min = j;
                }
        }
return min;
}

void swap(player_t *pointer[],unsigned int i,unsigned int j){
        player_t *tmp;
        tmp = pointer[i];
        pointer[i] = pointer[j];
        pointer[j] = tmp;
}

void selection_sort(unsigned int length, player_t *pointer[],player_t a[]){
        unsigned int minp;
       
       for (unsigned int i=0;i+1<length; i++){
            minp= min_pos_from(a,length,i);
            swap(pointer,i,minp);
        }       
}

void sort (player_t a[], unsigned int length, player_t *pointer[]){
   arreglo_puntero (a,length,pointer);
   selection_sort (length,pointer,a);
   arreglo_final (a, length, pointer);
}