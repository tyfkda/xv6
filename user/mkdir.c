#include "stdio.h"
#include "sys/stat.h"

int
main(int argc, char *argv[])
{
  int i;

  if(argc < 2){
    fprintf(stderr, "Usage: mkdir files...\n");
    return 1;
  }

  for(i = 1; i < argc; i++){
    if(mkdir(argv[i], 0777) < 0){
      perror("mkdir");
      return 1;
    }
  }

  return 0;
}
