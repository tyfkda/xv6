#include "stdio.h"
#include "stdlib.h"
#include "unistd.h"

char buf[512];

void
cat(int fd)
{
  int n;

  while((n = read(fd, buf, sizeof(buf))) > 0) {
    if (write(1, buf, n) != n) {
      fprintf(stderr, "cat: write error\n");
      exit(EXIT_FAILURE);
    }
  }
  if(n < 0){
    fprintf(stderr, "cat: read error\n");
    exit(EXIT_FAILURE);
  }
}

int
main(int argc, char *argv[])
{
  int fd, i;

  if(argc <= 1){
    cat(0);
    return EXIT_SUCCESS;
  }

  for(i = 1; i < argc; i++){
    if((fd = open(argv[i], 0)) < 0){
      fprintf(stderr, "cat: cannot open %s\n", argv[i]);
      return EXIT_FAILURE;
    }
    cat(fd);
    close(fd);
  }
  return EXIT_SUCCESS;
}
