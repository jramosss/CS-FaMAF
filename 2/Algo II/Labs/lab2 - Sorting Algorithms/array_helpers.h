/*!
  @file
  @brief Array helpers declarations
*/
#ifndef _ARRAY_HELPERS_H
#define _ARRAY_HELPERS_H

#include <stdbool.h>

/*!
  Checks if an array is a permutation of another.
  The arrays \p a and \p other must have both the same \p length.

  @param a  an array of \p length elements
  @param other  an array of \p length elements
  @param length length of both \p a and \p other

  @returns whether the array \p other is a permutation of the array \p a
*/
bool array_is_permutation_of(int a[], int other[], unsigned int length);

/*!
  Write the content of the array \p a into stdout. The array \p a must have
  exactly \p length elements.

  @param a an array of \p length elements
  @param length the length of \p a
*/
void array_dump(int a[], unsigned int length);

/*!
   Copy the array \p src into the array \p dst

   @param dst array to which the content of \p src will be copied into
   @param src array to be copied
   @param length length of \p src
*/
void array_copy(int dst[], int src[], unsigned int length);

/*!
  Each element is read from the file located at \p filepath.
  The file must exist in disk and must have its contents in the following
  format:

  <array_length>
  <array_elem_1> <array_elem_2> <array_elem_3> ... <array_elem_N>

  In other words, the first line must be the array length, and the next line
  must contain \p length amount of elements separated by one or more
  blank characters.

  Those elements are copied into the array \p a.
  The array_length must be lower or equal to \p max_size.

  @param a array with space to store \p max_size elements
  @param filepath path to the file to be read
  @returns  returns the length of the array.
*/
unsigned int array_from_file(int a[], unsigned int max_size,
                             const char *filepath);

#endif
