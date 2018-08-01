#include "stdio.h"

int main() {
  printf("\x1b[2J"  // Clear screen.
         "\x1b[H");  // Move to the top left.

  return 0;
}
