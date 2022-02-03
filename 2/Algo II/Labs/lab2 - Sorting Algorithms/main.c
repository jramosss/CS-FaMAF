#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "arguments_parser.h"
#include "array_helpers.h"
#include "sort.h"

#define MAX_SIZE 100000

static const double MILLISECONDS = 1000.0;

typedef enum algorithm_option {
  SELECTION_SORT,
  INSERTION_SORT,
  QUICK_SORT,
  DO_NOT_SORT
} algorithm_t;

void process(int array[], unsigned int length,
             algorithm_t algorithm, parse_arguments_result result)
{
    int copy[MAX_SIZE];
    array_copy(copy, array, length);
    const clock_t start = clock();
    switch (algorithm) {
    case SELECTION_SORT:{
            printf("===Selection Sort===\n");
            selection_sort(copy, length);
            break;
        }
    case INSERTION_SORT:{
            printf("===Insertion Sort===\n");
            insertion_sort(copy, length);
            break;
        }
    case QUICK_SORT:{
            printf("===Quick Sort===\n");
            quick_sort(copy, length);
            break;
        }
    case DO_NOT_SORT:{
            printf("=== Result ===\n");
            break;
        }
    default:
          printf("Unsupported algorithm\n");
          exit(EXIT_FAILURE);
    }
    const clock_t stop = clock();
    const double elapsed = (double) (stop - start) * MILLISECONDS / CLOCKS_PER_SEC;
    if (result.print_result) {
        array_dump(copy, length);
    }
    printf("Time elapsed in milliseconds: %f\n", elapsed);
    if (result.check_result) {
        const bool sorted = array_is_sorted(copy, length);
        printf("Array %s sorted\n", sorted ? "is" : "is NOT");
        const bool permutation = array_is_permutation_of(array, copy, length);
        printf("Array %s a permutation of original array\n",
               permutation ? "is" : "is NOT");
    }
}

int main(int argc, char *argv[])
{
    parse_arguments_result result = parse_arguments(argc, argv);
    for (unsigned int i = 0u; i < result.files_count; ++i) {
        int array[MAX_SIZE];
        char *filepath = result.filepaths[i];
        printf("[FILE %s]\n", filepath);
        unsigned int length = array_from_file(array, MAX_SIZE, filepath);
        if (result.do_selection_sort) {
            process(array, length, SELECTION_SORT, result);
        }
        if (result.do_insertion_sort) {
            process(array, length, INSERTION_SORT, result);
        }
        if (result.do_quick_sort) {
            process(array, length, QUICK_SORT, result);
        }
        if (result.do_not_sort) {
            process(array, length, DO_NOT_SORT, result);
        }
        printf("\n\n");
    }

    return EXIT_SUCCESS;
}
