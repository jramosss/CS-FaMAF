#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "builtin.h"
#include "command.h"

bool builtin_is_internal(scommand cmd) {
    return strcmp(scommand_front(cmd),"cd")==0
    || strcmp(scommand_front(cmd),"exit") == 0;
}

void builtin_exec(scommand cmd) {

    if (strcmp(scommand_front(cmd),"cd") == 0){

       char* dir = scommand_peek_nth(cmd,1);
       int redirect = chdir(dir);

       if(redirect != 0)
        printf("CD_ERROR: No se encuentra el directorio\n");
    }
    else if (strcmp(scommand_front(cmd),"exit") == 0){
        exit(EXIT_SUCCESS);

    }
    else {
        printf("Comando invalido\n");
    }
}