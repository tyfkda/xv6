#include "./lib/crt0.c"
#include "util.c"

int main() {
  int i;
  for (i = 1; i <= 100; ++i) {
    if ((i % 15) == 0) {
      putstr("fizzbuzz\n");
    } else if ((i % 5) == 0) {
      putstr("buzz\n");
    } else if ((i % 3) == 0) {
      putstr("fizz\n");
    } else {
      putdeci(i);
      putstr("\n");
    }
  }
  return 0;
}
