#include "array_helpers.h"
#include "weather.h"
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include "weather_utils.h"


int h_min_temp (WeatherTable array){
    int mintmp= INT_MAX;    
    for(int i=0 ; i<YEARS ; i++){
        for(int j=0 ; j<MONTHS ; j++){
            for(int k=0 ; k<LST_DAY ; k++){
                if((array[i][j][k])._min_temp < mintmp) {
                    mintmp=(array[i][j][k]._min_temp);
                }
            }
        }
    }
    return mintmp;
}

void max_temp_anual (WeatherTable array, int output[]) {
    int aux = INT_MIN;
        for(int i=0 ; i<YEARS ; i++){
            int maxtmp = INT_MIN;
            for(int j=0 ; j<MONTHS ; j++){
                for(int k=0 ; k<LST_DAY ; k++){
                    if ((array[i][j][k]._max_temp) > maxtmp){
                        maxtmp = array[i][j][k]._max_temp;
                        }
                }
                        if (aux < maxtmp) {
                            aux = maxtmp;
                    }

                }
                            output[i] = aux;
}
}
/*
void max_yearly_rain_times (WeatherTable array, int registro[]) {
    int may_prec = INT_MIN;
    int may_prec_pos = 0;
        for(int i=0 ; i<YEARS ; i++){
            for(int j=0 ; j<MONTHS ; j++){
                    int  monthlyRain = 0;
                for(int k=0 ; k<LST_DAY ; k++){
                    if ((array[i][j][k]._rainfall) > 0) {
                        monthlyRain++;
                    }
                if (monthlyRain > may_prec) {
                    may_prec = monthlyRain;
                    may_prec_pos = j;
                    registro[j] = j;
                } 
              }
            }
        }   
}
*/
void max_yearly_rain (WeatherTable array, int registro[]) {
    int may_prec = INT_MIN;
    int may_prec_pos = 0;
        for(int i=0 ; i<YEARS ; i++){
                may_prec = INT_MIN;
            for(int j=0 ; j<MONTHS ; j++){
                    int  monthlyPrec = 0;
                for(int k=0 ; k<LST_DAY ; k++){
                    monthlyPrec = monthlyPrec + (array[i][j][k]._rainfall);
                    }
                if (monthlyPrec > may_prec) {
                    may_prec=monthlyPrec;
                    may_prec_pos = j+1;
                    } 
            }
            registro[i] = may_prec_pos;
        }
}