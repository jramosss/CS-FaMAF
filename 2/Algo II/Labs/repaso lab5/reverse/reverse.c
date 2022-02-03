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
    stack_t stack = stack_empty();
    unsigned int i = 0;
    while (i < strlen(str)) {
    	stack = stack_push(stack,str[i]);
    	i++;
    }
    i = 0;
    while (!stack_is_empty(stack)) {
    	str[i] = stack_top(stack);
    	stack = stack_pop(stack);
    	i++;
    }
    printf("%s\n", str);
    return 0;
}
