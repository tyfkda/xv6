#include "stdio.h"
#include "stdlib.h"

int main() {
  printf("\x1b[2J"  // Clear screen.
         "\x1b[H");  // Move to the top left.

  return EXIT_SUCCESS;
}
