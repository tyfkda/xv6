#include "stdio.h"

int main() {
  int n = 5;
  for (int i = 0; i < n; ++i) {
    char buf[512];
    if (fgets(buf, sizeof(buf), stdin) == NULL)
      break;
    printf("%s", buf);
  }

  return 0;
}
