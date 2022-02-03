#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../stack.h"

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Usage: ./reverse <string-to-reverse>\n");
        exit(EXIT_FAILURE);
    }
    char *str = argv[1];
    unsigned int i = 0;
    stack_t s = stack_empty();
    for (unsigned int i = 0; i < strlen (str); i++) {
    	s = stack_push (s, str[i]);
    } // str = hola
      // s = |hola
    while (0 < stack_size(s)){
    	str[i] = stack_top(s);
    	s =  stack_pop(s);
        i++;
    }
    printf("%s\n", str);
}
