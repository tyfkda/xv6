#include "fcntl.h"
#include "sys/stat.h"
#include "unistd.h"

extern char **environ;

int
stat(const char *n, struct stat *st)
{
  int fd;
  int r;

  fd = open(n, O_RDONLY);
  if(fd < 0)
    return -1;
  r = fstat(fd, st);
  close(fd);
  return r;
}

