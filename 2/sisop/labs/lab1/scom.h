#include<glib.h>

typedef struct scommand_s {
    GQueue* queue;
    char* in;
    char* out;
}scommand_s;