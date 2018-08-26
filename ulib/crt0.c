#include "stdlib.h"

extern int main(int argc, char** argv);
extern void _setup_environment(char *envs[]);

void _start(int argc, char** argv, char **envp) {
  int code;
  _setup_environment(envp);
  code = main(argc, argv);
  exit(code);
}
