#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "assert.h"
#include "../stack.h"
#include "hanoi.h"

/*
    Completar la siguiente función recursiva. Usar la solución python como guía:
    https://en.wikipedia.org/wiki/Tower_of_Hanoi
    Ver código incompleto más abajo.

    El resto de las funciones ya están completas.

*/
static void move(unsigned int current,
        hanoi_t hanoi,
        stack_t *source_ptr,
        stack_t *target_ptr,
        stack_t *aux_ptr
    );

static void print(stack_t source, stack_t aux, stack_t target,
    unsigned int disk_count);


struct _hanoi {
    stack_t source;
    stack_t aux;
    stack_t target;
    unsigned int disk_count;
};

hanoi_t hanoi_init(unsigned int disk_count) {
    hanoi_t hanoi = calloc(1, sizeof(struct _hanoi));
    assert(hanoi != NULL);
    hanoi->source = stack_empty();
    hanoi->aux = stack_empty();
    hanoi->target = stack_empty();
    hanoi->disk_count = disk_count;
    for (unsigned int i = disk_count; i > 0; --i) {
        hanoi->source = stack_push(hanoi->source, i);
    }
    return hanoi;
}

void hanoi_solve(hanoi_t hanoi) {
    move(hanoi->disk_count, hanoi, &hanoi->source,
        &hanoi->target, &hanoi->aux);
}

void hanoi_print(hanoi_t hanoi) {
    print(hanoi->source, hanoi->aux, hanoi->target, hanoi->disk_count);
}

hanoi_t hanoi_destroy(hanoi_t hanoi) {
    assert(hanoi != NULL);
    hanoi->source = stack_destroy(hanoi->source);
    hanoi->aux = stack_destroy(hanoi->aux);
    hanoi->target = stack_destroy(hanoi->target);
    free(hanoi);
    return NULL;
}

static void move(unsigned int current,
    hanoi_t hanoi,
    stack_t *source_ptr,
    stack_t *target_ptr,
    stack_t *aux_ptr
    ) {
        /* Completar */
}

static char *create_disk_str(int size) {
    char *result = NULL;
    if (size == 1) {
        const unsigned int tam = 3;
        result = calloc(tam, sizeof(char));
        strcpy(result, "/\\");
    } else {
        const char *prefix = "/";
        char *rec = create_disk_str(size - 1);
        const char *suffix = "\\";
        const unsigned int tam = 3 + strlen(rec);
        result = calloc(tam, sizeof(char));
        strcat(result, prefix);
        strcat(result, rec);
        strcat(result, suffix);
        free(rec);
    }
    return result;
}

static char *position_str(stack_elem_t *arr, int length, int position) {
    char *result = NULL;
    char *value = NULL;
    if (position >= length) {
        const unsigned int tam = 2;
        value = calloc(tam, sizeof(char));
        strcpy(value, " ");
    } else {
        value = create_disk_str(arr[position]);
    }
    result = calloc(strlen(value) + 1, sizeof(char));
    assert(result != NULL);
    strcpy(result, value);
    free(value);
    return result;
}

static void print_disks(stack_elem_t *arr_a,
    unsigned int length_a,
    stack_elem_t *arr_b,
    unsigned int length_b,
    stack_elem_t *arr_c,
    unsigned int length_c,
    int position,
    int width) {
    if (position >= 0) {
        char *value_a = position_str(arr_a, length_a, position);
        char *value_b = position_str(arr_b, length_b, position);
        char *value_c = position_str(arr_c, length_c, position);
        int padlen1 = (width - strlen(value_a)) / 2;
        int padlen2 = (width - strlen(value_b)) / 2;
        int padlen3 = (width - strlen(value_c)) / 2;
        printf("%*s%s%*s%*s%s%*s%*s%s%*s\n",
            padlen1, "", value_a, padlen1, "",
            padlen2, "", value_b, padlen2, "",
            padlen3, "", value_c, padlen3, ""
        );
        free(value_a);
        free(value_b);
        free(value_c);
        print_disks(arr_a, length_a, arr_b, length_b, arr_c, length_c,
            position - 1, width);
    } else {
        int padlen = (width - 1) / 2;
        printf("%*s%s%*s%*s%s%*s%*s%s%*s\n",
            padlen, "", "S", padlen, "",
            padlen, "", "M", padlen, "",
            padlen, "", "T", padlen, ""
        );
    }
}

static void print(stack_t source, stack_t aux, stack_t target,
    unsigned int disk_count) {
    stack_elem_t *arr1 = stack_to_array(source);
    stack_elem_t *arr2 = stack_to_array(aux);
    stack_elem_t *arr3 = stack_to_array(target);
    const unsigned int length1 = stack_size(source);
    const unsigned int length2 = stack_size(aux);
    const unsigned int length3 = stack_size(target);
    const unsigned int position = disk_count;
    const unsigned int width = 2 * disk_count;
    print_disks(arr1, length1, arr2, length2, arr3, length3,
        position, width);
    free(arr1);
    free(arr2);
    free(arr3);
}
