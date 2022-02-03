/*!
  @file arguments_parser.h
  @brief arguments parser definitions
*/

#ifndef PARSE_ARGUMENTS_H
#define PARSE_ARGUMENTS_H

#include <stdbool.h>

#define FILE_OPT 301
#define MAX_FILES 200

/**
 * structure which represents arguments parsed.
 */
typedef struct _parse_arguments_result {
    char *filepaths[MAX_FILES];
    unsigned int files_count;
    bool do_selection_sort;
    bool do_insertion_sort;
    bool do_quick_sort;
    bool do_not_sort;
    bool print_result;
    bool check_result;
} parse_arguments_result;

/**
 * @brief      parse command line arguments and returns an 
 *             structure with main program args
 *
 * @param[in]  argc  Amount of arguments
 * @param[in]  argv  string array which contains all command line arguments
 *
 * @return     command line parsed arguments
 */
parse_arguments_result parse_arguments(int argc, char *argv[]);

#endif
