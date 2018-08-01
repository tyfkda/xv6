#include "errno.h"
#include "stdio.h"

int errno;

void perror(const char* message) {
  fprintf(stderr, "perror: errno=%d\n%s\n", errno, message);
}

char *strerror(int errnum) {
  static char buf[32];
  snprintf(buf, sizeof(buf), "ERROR %d", errnum);
  return buf;
}
