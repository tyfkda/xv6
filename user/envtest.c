#include "stdio.h"
extern char **environ;
int
main(int argc, char *argv[])
{
  int i;
  printf("user argv:%p environ:%p \n", argv, environ);
  for(i = 1; i < argc; i++)
    printf("args: %s%s", argv[i], i+1 < argc ? " " : "\n");
  for(i = 0; environ[i] != 0; ++i)
    printf("envs: environ[%d]:  %s\n", i, environ[i]);


  return 0;
}
