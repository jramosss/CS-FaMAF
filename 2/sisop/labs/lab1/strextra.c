#include <stdlib.h>
#include <string.h>
#include "strextra.h"

char * strmerge(char *s1, char *s2)
{
    char* res = (char*)calloc(strlen(s1)+strlen(s2)+1,sizeof(char));
    res = strcat(s1,s2);
    return res;
}
