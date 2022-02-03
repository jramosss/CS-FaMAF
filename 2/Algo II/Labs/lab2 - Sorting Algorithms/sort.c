#include <stdio.h>
#include "sort.h"
#include <stdlib.h>

bool array_is_sorted(int array[], unsigned int length)
{
	bool siono = true;
	for (unsigned int i=0; i+1<length; i++) {
	        if (array[i]>array[i+1]) {
			siono = false;
		}
}
return siono;
}

void swap (int a[],unsigned int i,unsigned int j) {
        int tmp=0;
        tmp = a[i];
        a[i] = a[j];
        a[j] = tmp;
}

void insert (int a[],unsigned int i){
        for (unsigned int j=i; j>0; j--) {
                if (a[j]<a[j-1]){
                        swap(a,(j-1),j);                              
                }
        }
}

void insertion_sort(int a[], unsigned int length) {
	for (unsigned int i=1; i<length; i++) {
                insert(a,i);
                }
	
}

int min_pos_from (int a[],unsigned int length, int i) {
        int min = i;
        for (unsigned int j=i+1 ; j<length; j++){
                        if (a[j]<a[min]) {
                                min=j;
                               }
         }
return min;
}

void selection_sort(int a[], unsigned int length) {
        unsigned int minp;
        for (unsigned int i=0; i+1<length; i++ ){
                minp = min_pos_from (a,length,i);
                swap(a,i,minp);
        }
}

int partition (int a[],unsigned int izq, unsigned int drc, unsigned int pivot) {
        unsigned int i;
        unsigned int j;
        pivot = izq;
        i = izq+1;
        j = drc;
        while (i<=j) {
                if (a[i]<=a[pivot]){
                        i++;
                } else if (a[j]>=a[pivot]) {
                        j--;
                } else { 
                        swap(a,i,j);
                        i++;
                        j--;

                }
        }
        swap(a,pivot,j);
        pivot = j;
        
        return pivot;
}

int random (unsigned int y, unsigned int z) {
    unsigned int x = 0;
    x = rand() % ((y + z)+1);
return x;
}
    
int partitionstar (int a[],unsigned int izq, unsigned int drc, unsigned int pivot) {
        unsigned int r;
        unsigned int i;
        unsigned int j;
        r = random (izq,drc);
        pivot = r;
        printf("El random es %d \n", r);
        printf("El pivot es %d \n",pivot);
        i = izq+1;
        j = drc;
        swap(a,pivot,izq);
        pivot = izq;
        while (i<=j) {
                if (a[i]<=a[pivot]){ // si el elemento a[i] < a[piv], segui nomas
                        i++;
                } else if (a[j]>=a[pivot]) { 
                        j--;
                } else {  //((a[i] > a[pivot]) && (a[j] < a[pivot])) 
                        swap(a,i,j);
                        i++;
                        j--;
                }
        }
        swap(a,pivot,j);
        pivot = j;
        
        return pivot;
}

void quick_sort_rec(int a[], unsigned int izq, unsigned int drc){
        unsigned int pivot = 0;

        if (drc > izq) {
                pivot = partition(a,izq,drc,pivot);
                if (pivot>0) {
                quick_sort_rec(a,izq,(pivot-1));
                }
                quick_sort_rec(a,(pivot+1),drc);
        }
}


void quick_sort (int a[], unsigned int length) {
        quick_sort_rec(a,0,length-1);
}
