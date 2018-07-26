#include "stdio.h"

int main() {
  for (int i = 0; i < 8; ++i) {
    printf("\x1b[4%dm", i);
    for (int j = 0; j < 8; ++j) {
      printf("\x1b[3%dm@", j);
    }
    printf("\n");
  }

  for (int i = 0; i < 8; ++i) {
    printf("\x1b[10%dm", i);
    for (int j = 0; j < 8; ++j) {
      printf("\x1b[9%dm@", j);
    }
    printf("\n");
  }

  printf("\x1b[39m\x1b[49m");

  printf("Hello, \x1b[1mworld\x1b[0m!\n");
  printf("I \x1b[4mhave\x1b[0m an \x1b[41mAPP\x1b[7mLE\x1b[0m.\n");

  printf("\x1b[39m\x1b[49m");

  return 0;
}
