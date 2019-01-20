#include "stdbool.h"
#include "stdio.h"
#include "string.h"

int
main(int argc, char *argv[])
{
  bool first, newline;
  int i;

  newline = true;
  for (i = 1; i < argc; ++i) {
    if (strcmp(argv[i], "-n") == 0)
      newline = false;
    else
      break;
  }

  first = true;
  for(; i < argc; ++i) {
    printf("%s%s", first ? "" : " ", argv[i]);
    first = false;
  }

  if (newline)
    printf("\n");

  return 0;
}
