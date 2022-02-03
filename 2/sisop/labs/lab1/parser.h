#ifndef PARSER_H
#define PARSER_H

#include <stdbool.h>	/* bool */
#include <stdio.h> 		/* FILE */
#include "command.h"	/* pipeline */

/* Tipo opaco, implementación oculta */
typedef struct parser_s * Parser;


/* Constructor de Parser.
 * El input es el archivo de donde se quieren parsear pipelines.
 * REQUIRES:
 *     input != NULL
 * ENSURES:
 *     Devuelve un Parser para el archivo
 *     o NULL en caso de haber un error de inicialización
 */
Parser parser_new(FILE *input);


/* Destructor de Parser.
 * REQUIRES:
 *     parser != NULL
 * ENSURES:
 *     Devuelve NULL
 */
Parser parser_destroy(Parser parser);


/* Lee todo un pipeline de `parser' hasta llegar a un fin de línea (inclusive)
 * o de archivo.
 * Devuelve un nuevo pipeline (a liberar por el llamador), o NULL en caso
 * de error.
 * REQUIRES:
 *     parser != NULL
 *     ! parser_at_eof (parser)
 * ENSURES:
 *     No se consumió más entrada de la necesaria
 *     El parser esta detenido justo luego de un \n o en el fin de archivo.
 *     Si lo que se consumió es un pipeline valido, el resultado contiene la
 *     estructura correspondiente.
 */
pipeline parse_pipeline(Parser parser);


/* Consulta si el parser llegó al final del archivo.
 * REQUIRES:
 *     parser != NULL
 */
bool parser_at_eof(Parser parser);

#endif /* PARSER_H */
