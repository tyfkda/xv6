//#define DEBUG_SHOW_EXECVE_ENVS

#include "stdlib.h"

#if defined(DEBUG_SHOW_EXECVE_ENVS)
#include "fcntl.h"
#include "stdio.h"
#include "unistd.h"
#endif  /*  DEBUG_SHOW_EXECVE_ENVS  */

char **environ={0};

extern int main(int argc, char** argv);

void _start(int argc, char** argv, char **envp) {
#if defined(DEBUG_SHOW_EXECVE_ENVS)
  int fd;
#endif  /*  DEBUG_SHOW_EXECVE_ENVS  */

#if defined(DEBUG_SHOW_EXECVE_ENVS)
  // Open standard input, output and error in advance.
  while((fd = open("console", O_RDWR)) >= 0){
    if(fd >= 3){
      close(fd);
      break;
    }
  }
#endif  /*  DEBUG_SHOW_EXECVE_ENVS  */

  environ = envp;

#if defined(DEBUG_SHOW_EXECVE_ENVS)
  fprintf(stderr, "crt0: envp: %p environ: %p\n", envp, environ);
#endif  /*  DEBUG_SHOW_EXECVE_ENVS  */

  int code = main(argc, argv);
  exit(code);
}
