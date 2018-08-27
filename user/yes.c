#include "stdio.h"
#include "unistd.h"
#include "sys/wait.h"
#include "signal.h"

int main() {
  for (;;) {
    printf("y\n");
  }

  return 0;
}
