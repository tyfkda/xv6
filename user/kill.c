#include "signal.h"
#include "stdio.h"
#include "stdlib.h"

int
main(int argc, char **argv)
{
  int i;

  if(argc < 2){
    fprintf(stderr, "usage: kill pid...\n");
    exit(1);
  }
  for(i=1; i<argc; i++)
    kill(atoi(argv[i]), 0);
  return 0;
}
