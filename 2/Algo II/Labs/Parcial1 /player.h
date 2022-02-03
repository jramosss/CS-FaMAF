/*
  @file player.h
  @brief Player structure definition
*/
#ifndef _PLAYER_H
#define _PLAYER_H
#define PLAYER_NAME_MAX_LENGTH 100u
#define PLAYER_COUNTRY_MAX_LENGTH 3u
#include "player.h"
/** @brief structure used to represent a tennis player position in ranking  */
typedef struct _player_t {
    char name[PLAYER_NAME_MAX_LENGTH + 1u];
    char country[PLAYER_COUNTRY_MAX_LENGTH + 1u];
    unsigned int rank;
    unsigned int age;
    unsigned int points;
    unsigned int tournaments;
} * player_t; // NOTE! player_t is an alias of struct _player_t *

/**
 * @brief Returns the count of players whose age is equal to 'age'.
 *        The array 'atp' must have exactly 'length' elements.
 *
 * @param[in]      age       Age of player
 * @param[in]      atp       Players array
 * @param[in]      length  	 Length of players array
 * @return count of players in array atp with age equal to 'age', 0 otherwise.
 */
unsigned int player_count_by_age(unsigned int age, player_t atp[], unsigned int length);

/**
 * @brief    Returns the first player whose rank is equal to 'rank'
 *
 * @param[in]  rank    The rank of a player
 * @param[in]  atp     The Player array
 * @param[in]  length  Length of player array
 *
 * @return the first player
 * in array atp whose rank is equal to 'rank', NULL otherwise.
 */
player_t get_one_player_by_rank(unsigned int rank, player_t atp[], unsigned int length);

#endif //_PLAYER_H
