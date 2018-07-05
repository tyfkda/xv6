#include "types.h"
#include "user.h"

char buf[512];

void
cat(int fd)
{
  int n;

  while((n = read(fd, buf, sizeof(buf))) > 0) {
    if (write(1, buf, n) != n) {
      printf(2, "cat: write error\n");
      exit(1);
    }
  }
  if(n < 0){
    printf(2, "cat: read error\n");
    exit(1);
  }
}

int
main(int argc, char *argv[])
{
  int fd, i;

  if(argc <= 1){
    cat(0);
    return 1;
  }

  for(i = 1; i < argc; i++){
    if((fd = open(argv[i], 0)) < 0){
      printf(2, "cat: cannot open %s\n", argv[i]);
      return 1;
    }
    cat(fd);
    close(fd);
  }
  return 0;
}
