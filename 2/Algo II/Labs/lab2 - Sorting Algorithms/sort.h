/*!
  @file
  @brief Sorting algorithms declarations
*/
#ifndef SORT_H
#define SORT_H
#include <stdbool.h>

/*!
  Checks if an array is sorted

  @param array  an array with \p length elements
  @param length length of \p array

  @return true if array is sorted in ascending order,
          false otherwise
*/
bool array_is_sorted(int array[], unsigned int length);

/*!
  Sorts an array in ascending order using the 'Selection sort' algorithm

  @param array  an array with \p length elements
  @param length length of \p array
*/
void selection_sort(int array[], unsigned int length);

/*!
  Sorts an array in ascending order using the 'Insertion sort' algorithm

  @param array  an array with \p length elements
  @param length length of \p array
*/
void insertion_sort(int array[], unsigned int length);

/*!
  Sorts an array in ascending order using the 'Quick sort' algorithm

  @param array  an array with \p length elements
  @param length length of \p array
*/
void quick_sort(int array[], unsigned int length);

#endif
