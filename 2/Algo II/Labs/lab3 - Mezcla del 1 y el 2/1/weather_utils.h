#ifndef _WEATHER_UTILS_H
#define _WEATHER_UTILS_H
#include <stdio.h>
#include "array_helpers.h"
#include "weather_utils.h"

int h_min_temp(WeatherTable array);
void max_temp_anual (WeatherTable array, int output[]);
void max_yearly_rain (WeatherTable array, int  registro[]);
#endif
