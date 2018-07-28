#include "errno.h"
#include "stdio.h"

int errno;

void perror(const char* message) {
  fprintf(stderr, "perror: errno=%d\n%s", errno, message);
}
