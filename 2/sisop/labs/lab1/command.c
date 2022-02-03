#include <assert.h>
#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "command.h"

//gcc test.c -Wall -o test `pkg-config --cflags --libs glib-2.0`

#define check(s) assert(s != NULL)

scommand scommand_new(void) {
    scommand comm = (scommand)calloc(1,sizeof(struct scommand_s));
    comm->queue = g_queue_new();
    return comm;
}

scommand scommand_destroy(scommand self){
    if (self != NULL){
        free(self);
        self = NULL;    //Nunca supe si esto es necesario o ya lo hace el free
    }
    return NULL;
}

void scommand_push_back(scommand self, char * argument) {
    assert(self != NULL && argument != NULL);
    g_queue_push_tail(self->queue,argument);
}

void scommand_pop_front(scommand self){
    assert(self!=NULL && !scommand_is_empty(self));
    //Elimino el segundo elemento, ya que el primero es el comando
    //TODO PREGUNTAR si la cadena contiene al comando
    g_queue_pop_nth(self->queue,1);
}

bool scommand_is_empty(const scommand self) {
    check(self);
    return scommand_length(self) == 0;
}

unsigned int scommand_length(const scommand self) {
    check(self);
    return self->queue->length;
}

void scommand_set_redir_in(scommand self, char * filename) {
    check(self);
    self->in = filename;
}

void scommand_set_redir_out(scommand self, char * filename){
    check(self);
    self->out = filename;
}

char * scommand_front(const scommand self){
    assert(self!=NULL && !scommand_is_empty(self));
    return self->queue->head->next->data;
}

char * scommand_get_redir_in(const scommand self) {
    check(self);
    return self->in;
}

char * scommand_get_redir_out(const scommand self) {
    check(self);
    return self->out;
}

char * scommand_to_string(const scommand self) {
    check(self);
    char* res = malloc(sizeof(char*));
    //Empiezo por el segundo elemento
    GList* aux = self->queue->head->next;
    while (aux->next != NULL){
        res = strcat(res,aux->data);
        aux = aux->next;
    }
    return res;
}

int main (void) {
    printf("Hello world\n");
    return 0;
}
