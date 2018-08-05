#include "stdio.h"
#include "stdlib.h"
#include "unistd.h"

int
main(int argc, char *argv[])
{
  int i;

  if(argc < 2){
    fprintf(stderr, "Usage: rm files...\n");
    return EXIT_FAILURE;
  }

  for(i = 1; i < argc; i++){
    if(unlink(argv[i]) < 0){
      fprintf(stderr, "rm: %s failed to delete\n", argv[i]);
      return EXIT_FAILURE;
    }
  }

  return EXIT_SUCCESS;
}
