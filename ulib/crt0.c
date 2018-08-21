#include "stdlib.h"

extern char **environ;

extern int main(int argc, char** argv);
extern void _setup_environment(char *[]);

void _start(int argc, char** argv, char **envp) {
  environ = envp;

  _setup_environment(envp);

  int code = main(argc, argv);
  exit(code);
}
