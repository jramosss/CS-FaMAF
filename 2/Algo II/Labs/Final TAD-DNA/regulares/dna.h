#ifndef DNA_H
#define DNA_H

#include <stdbool.h>
#include <assert.h>

typedef enum nucleobase { T, C, A, G } nucleobase_t;
typedef struct _node *dna_t;

dna_t dna_T();
dna_t dna_C();
dna_t dna_A();
dna_t dna_G();

dna_t dna_join(dna_t first, dna_t second);
unsigned int dna_length(dna_t dna);
void dna_print(dna_t dna);
bool dna_is_prefix(dna_t first, dna_t second);
bool dna_is_equal(dna_t first, dna_t second);
dna_t *dna_cut(dna_t dna, unsigned int count);
dna_t dna_destroy(dna_t dna);

#endif
