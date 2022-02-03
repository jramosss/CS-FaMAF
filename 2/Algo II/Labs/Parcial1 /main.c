/*
  @file main.c
  @brief Main program function implementation
*/
/* First, the standard lib includes, alphabetically ordered */
#include <stdio.h>
#include <stdlib.h>

/* Then, this project's includes, alphabetically ordered */
#include "helpers.h"
#include "player.h"

#define MAX_PLAYERS 5000

/**
 * @brief print usage help
 * @param[in] program_name Executable name
 */
void print_help(char *program_name) {
    /* Print the usage help of this program. */
    printf("Usage: %s <input file path>\n\n"
           "Sort an array given in a file in disk.\n"
           "\n"
           "The input file must have the following format:\n"
           " * Each line must contain the name of a player"
           " without spaces followed by a three-letter country"
           " code, the rank of the player, his age, his atp points"
           " and the number of tournaments played during the year.\n"
           " * Values must be separated by one or more spaces.\n"
           " * Numeric values must be natural numbers.\n\n",
           program_name);
}

/**
 * @brief reads file path from command line
 *
 * @param[in] argc amount of command line arguments
 * @param[in] argv command line arguments
 *
 * @return An string containing read filepath
 */
char *parse_filepath(int argc, char *argv[]) {
    /* Parse the filepath given by command line argument. */
    char *result = NULL;

    if (argc < 2) {
        print_help(argv[0]);
        exit(EXIT_FAILURE);
    }

    result = argv[1];

    return (result);
}

/**
 * @brief Main program function
 *
 * @param[in] argc amount of command line arguments
 * @param[in] argv command line arguments
 *
 * @return EXIT_SUCCESS when programs executes correctly, EXIT_FAILURE otherwise
 */
int main(int argc, char *argv[]) {
    char *filepath = NULL;
    player_t atp[MAX_PLAYERS];
    unsigned int age= 29;
    unsigned int x = 1;
    player_t thechosenone = NULL; 
    //unsigned int rank = 3;
    /* parse the filepath given in command line arguments */
    filepath = parse_filepath(argc, argv);

    /* parse the file to load the players */
    unsigned int length = process_file(filepath, atp);

    
    //player_count_by_age (age,atp,length);
    printf(" Edad?:");
    scanf("%u",&age);
    while (age < 1){
      printf("Edad Incorrecta\n");
      printf("Edad?:\n");
      scanf ("%u",&age); 
    }
    
    printf("Hay %u jugadores con %u años \n", player_count_by_age(age,atp,length), age);  
    
    printf("Ranking del jugador?:");
    scanf ("%u",&x);
    get_one_player_by_rank (x,atp,length);
    if (thechosenone != NULL){
    printf("%s %s %u %u %u %u \n", thechosenone -> name, thechosenone -> country, thechosenone -> rank, thechosenone -> age, thechosenone -> points, thechosenone -> tournaments );
    }
    else {
    printf("No existe jugador con ese ranking\n");
    }  
    
    destroy(atp, length);
    return (EXIT_SUCCESS);
    
}
