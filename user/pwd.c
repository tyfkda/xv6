#include "stdio.h"
#include "unistd.h"

int main(int argc, char *argv[]) {
  char resultPath[512];
  if (!getcwd(resultPath, sizeof(resultPath))) {
    fprintf(stderr, "pwd failed\n");
    return 1;
  }

  printf("%s\n", resultPath);
  return 0;
}
