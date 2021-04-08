#include "../lib/crt0.c"
#include "util.c"

int main(int argc, char **argv) {
  int i;
  for (i = 1; i < argc; ++i) {
    if (i > 1)
      putstr(" ");
    putstr(argv[i]);
  }
  putstr("\n");
  return 0;
}
