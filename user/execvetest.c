#include "stdio.h"
#include "stdlib.h"
#include "unistd.h"

char *envargv[] = { "envtest", "ALL", "TESTS", "PASSED", 0 };
char *envenvp[] = { "NAME=envtest", "TESTS=OK", "RESULT=\"PASSED\"", 0 };

void
execvetest(void)
{
  printf("execve test\n");

  if(execve("/bin/envtest", envargv, envenvp) < 0){
    printf("execve envtest failed\n");
    exit(1);
  }
}

int
main(int argc, char *argv[])
{

  printf("execvetest starting\n");
  execvetest();

  return 0;
}
