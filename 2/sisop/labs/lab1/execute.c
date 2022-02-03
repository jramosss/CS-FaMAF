#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "tests/syscall_mock.h"
#include <unistd.h>

#include "builtin.h"
#include "command.h"
#include "execute.h"
#include "parser.h"

#define TRY(fd,where) if (fd < 0){ printf("ERROR %s\n",where); exit(1);}
#define TRYs(fd,where) if(fd < 0) printf("SOFT ERROR %s\n",where);


static void open_redir (char* redir,int std){

    //if std == STDOUT_FILENO
    int flags = std ? O_CREAT | O_WRONLY | O_TRUNC : O_RDONLY;
    
    int fd = open(redir,flags,S_IRWXU);

    TRY(fd,"open redir");

    if (std == STDOUT_FILENO)
        TRY(dup2(fd,STDOUT_FILENO),"open redir IN");
        //No se si este TRYeo es necesario

    if (std == STDIN_FILENO)
        TRY(dup2(fd,STDIN_FILENO),"open redir OUT");
    

    TRY(close(fd),"close open redir");

    free(redir);
}


//Determina si el redir es NULL y si es redir_in o redir_out
static void handle_redirs (scommand scmd) 
{
    if (scommand_get_redir_in(scmd))
        open_redir(scommand_get_redir_in(scmd),STDIN_FILENO);

    if (scommand_get_redir_out(scmd))
        open_redir(scommand_get_redir_out(scmd),STDOUT_FILENO);

}

static char** command_build (scommand scmd) 
{
    assert(scmd != NULL);

    if (builtin_is_internal(scmd))
        builtin_exec(scmd);

    const unsigned int len = scommand_length(scmd);
    char** argv = calloc(len+1,sizeof(char*));

    for (unsigned int i = 0; i < len; i++){
        argv[i] = strdup(scommand_peek_nth(scmd,i));
    }

    return argv;
}

static void exec_simple_command(scommand scomm){
    assert(scomm != NULL);

    char **argv = command_build(scomm);
    handle_redirs(scomm);
    TRY(execvp(argv[0],argv),"exec simple command");
}


void execute_pipeline(pipeline apipe) {
    assert(apipe != NULL);

    scommand scomm = NULL;
    const unsigned short len = pipeline_length(apipe);

    if (pipeline_is_empty(apipe)){
        printf("EMPTY PIPELINE\n");
        return;
    }
   
    scomm = pipeline_front(apipe);
    if (len > 2)
        printf("Muchos comandos\n");
    
    else if (len==1){

        if(builtin_is_internal(scomm))
            builtin_exec(scomm);
        else
        {
            pid_t pid1 = fork();
            TRY(pid1,"fork execute_pipeline");
            if(!pid1)
                exec_simple_command(scomm);
            else{
                if(pipeline_get_wait(apipe))
                    waitpid(pid1,NULL,0);
            }
        }
    }
    else {

        int pipefd[2];
        TRY(pipe(pipefd),"pipe");

        int pid = fork();
        TRY(pid,"fork execute");

        //Carlitos
        //Como tenemos dos comandos, corremos el primero en carlitos
        //y el segundo en raul.
        if (!pid) {

            if (builtin_is_internal(scomm))
                builtin_exec(scomm);
            else {
                //No usamos el read-end
                TRY(close(pipefd[0]),"close read-end");
            
                //Hacemos que pipefd[1] apunte al file descriptor 1
                TRY(dup2(pipefd[1],STDOUT_FILENO),"dup2 execute_pipeline");
                TRY(close(pipefd[1]),"close write-end");
                char** args = command_build(scomm);
                TRY(execvp(args[0], args),"execvp carlitos");
            }
        }
        //Raul
        else {
            if (pipeline_get_wait(apipe))
                TRY(waitpid(pid,NULL,0),"wait carlitos");

            pipeline_pop_front(apipe);
            scomm = pipeline_is_empty(apipe) ? NULL : pipeline_front(apipe);
            int pid2 = fork();
            TRY(pid2,"fork2 execute");
            //Tomas
            if (!pid2) {
                if (scomm != NULL){
                    handle_redirs(scomm);
                    TRY(dup2(pipefd[0],STDIN_FILENO),"dup2,pipefd[0],STDIN");
                    TRY(close(pipefd[0]),"close read-end");
                    TRY(close(pipefd[1]),"close write-end");
                    char** args2 = command_build(scomm);
                    TRY(execvp(args2[0], args2),"execvp tomas");
                }
            }
            if (pipeline_get_wait(apipe)){
                TRY(close(pipefd[0]),"close execute_pipeline");
                TRY(close(pipefd[1]),"close execute_pipeline");
                TRY(waitpid(pid2,NULL,0),"wait tomas");
            }
        }
    }
}