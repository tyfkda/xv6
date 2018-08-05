#include "stdio.h"
#include "stdlib.h"
#include "unistd.h"

int
main(int argc, char *argv[])
{
  int i;

  if(argc < 2){
    fprintf(stderr, "Usage: mkdir files...\n");
    return EXIT_FAILURE;
  }

  for(i = 1; i < argc; i++){
    if(mkdir(argv[i]) < 0){
      fprintf(stderr, "mkdir: %s failed to create\n", argv[i]);
      break;
    }
  }

  return EXIT_SUCCESS;
}
