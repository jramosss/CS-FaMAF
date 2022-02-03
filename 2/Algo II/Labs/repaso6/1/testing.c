#include <stdio.h>  /* printf(), fprintf()        */
#include "map.h"    /* funciones del TAD map_t    */
#include "string.h" /* Funciones del TAD string_t */

/*
 * Este es un archivo de ejemplo para hacer pruebas manuales del TAD Mapeo. La
 * idea es que hagan las pruebas que necesiten para asegurarse que las
 * funciones implementadas andan correctamente.
 *
 * No solo pueden probar la funcionalidad sino que pueden usar `valgrind` para
 * verificar si la prueba que hicieron genera memory leaks. Tener en cuenta que
 * en este archivo de ejemplo, cuando ocurre un error se sale del programa sin
 * liberar la memoria, por lo que en estos casos `valgrind` reportará varios
 * leaks.
 *
 * Se recomienda ir haciendo pruebas incrementales, comenzando con las más
 * simples. Si se detecta algún error, recordar utilizar las herramientas `gdb`
 * y `valgrind` para poder determinar las causas y resolverlo.
 *
 */

int main(void) {
    /*
     * Para probar el TAD Mapeo necesitaremos una variable del tipo map_t
     */
    map_t map;
    /*
     * Creamos un nuevo mapa vacío
     */
    map = map_empty();
    /*
     * Agregamos una palabra con su definición
     */
    map = map_put(map, string_create("hola"), string_create("saludo"));
    /*
     * Chequeamos si andan bien map_contains() / map_put()
     */
    string_t word = string_create("hola");
    if (map_contains(map, word)) {
        /* Vamos bien! */
        printf("La palabra se agregó al mapeo!\n");
    } else {
        /* Algo anda mal..*/
        fprintf(stderr, "ERROR: La palabra no se agregó\n");
        /* Salimos del programa indicando error */
        return -1;
    }
    /*
     * Chequeamos si andan bien map_get() / map_put()
     */
    string_t definition;
    string_t expected_def = string_create("saludo");
    /*
     * Si todo esta bien, la definición guardada en map debe ser 'saludo'
     */
    definition = map_get(map, word);
    if (string_eq(definition, expected_def)) {
        /* Buenisimo!*/
        printf("La definicion de '%s' es '%s'\n", string_ref(word),
                                                  string_ref(expected_def));
    } else {
        /* Algo salio mal :( */
        fprintf(stderr, "ERROR: La definición '%s' no es correcta!",
                string_ref(definition));
        /* Salimos del programa indicando error */
        return -1;
    }

    /* Destruimos el mapa */
    map = map_destroy(map);
    /* Destruimos los string_t auxiliares que creamos */
    word = string_destroy(word);
    expected_def = string_destroy(expected_def);
    /*
     * Si todo funciona bien la salida de este programa debería ser:
     *
     * $ ./test
     * La palabra se agregó al mapeo!
     * La definicion de 'hola' es 'saludo'
     *
     */
    return 0;
}

