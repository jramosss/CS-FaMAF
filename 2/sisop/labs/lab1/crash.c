#include <assert.h>
#include <check.h>
#include <signal.h>
#include <stdio.h> //
#include <stdlib.h>

#include "execute.h"
#include "builtin.h"
#include "command.h"
#include "parser.h"
#include "strextra.h"
#include "tests/syscall_mock.h"

static int test_execution(void) //int argc, char *argv[])
{
    pipeline pipe = pipeline_new();
    scommand sc = scommand_new();
    scommand sc1 = scommand_new();
    scommand_push_back(sc,"ls");
    scommand_push_back(sc1,"sort");

    //scommand_set_redir_in(sc,"crash.c");

    printf("%s\n",builtin_is_internal(sc) ? 
        "El comando es interno\n" : "El comando no es interno\n");
        
    printf("%s\n",scommand_front(sc));

    pipeline_push_back(pipe,sc);
    pipeline_push_back(pipe,sc1);
    //scommand_pop_front(sc);

    //scommand_push_back(sc,"sort");

    //pipeline_push_back(pipe,sc);
    pipeline_set_wait(pipe,true);
    char* print = pipeline_to_string(pipe);
    printf("%s\n",print);
    free(print);
    
    execute_pipeline(pipe);

    pipeline_destroy(pipe);

    return 7;
}

static int prompt_execution(pipeline apipe, Parser input, gchar *current_directory)
{
    input = parser_new(stdin);
    while (1)
    {
        current_directory = g_get_current_dir();
        printf("\033[1;36m");
        printf("%s ~ crash $ ", current_directory);
        printf("\033[0m");
        apipe = parse_pipeline(input);
        free(current_directory);
        if (apipe == NULL)
            continue;
        execute_pipeline(apipe);
        free(apipe);
    }
    return 0;
}

int main(void)
{

    printf("Type 0 for test execution or literally any other integer for prompt execution\n");
    int test_or_prompt;
    scanf("%d", &test_or_prompt);

    if (!test_or_prompt)
        return test_execution();

    else
    {
        pipeline apipe = NULL;
        Parser input = NULL;
        gchar *dir = NULL;
        int res = prompt_execution(apipe, input, dir);
        if (res != 0)
          free(input);        
    }   
    return 0;
}