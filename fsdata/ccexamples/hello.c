#include "./lib/crt0.c"
#include "util.c"

int main() {
  write(1, "Hello, world!\n", 14);
  return 0;
}
