#include "sys/stat.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "unistd.h"
#include "../kernel/fs.h"
#include "fcntl.h"
#include "../kernel/memlayout.h"
#include "../kernel/param.h"
#include "../kernel/traps.h"

#ifndef X64
#include "../kernel/syscall.h"
#endif

char *envargv[] = { "envtest", "ALL", "TESTS", "PASSED", 0 };
char *envenvp[] = { "TERM=vt100", "NAME=envtest", "TESTS=OK", "RESULT=\"PASSED\"", 0 };

void
execvetest(void)
{
  printf("execve test\n");

  if(execve("envtest", envargv, envenvp) < 0){
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
