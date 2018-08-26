#include "stdlib.h"

extern int main(int argc, char** argv);

char** _environ;

void _start(int argc, char** argv, char **envp) {
  int code;
  _environ = envp;
  code = main(argc, argv);
  exit(code);
}
