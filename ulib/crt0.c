#include "stdlib.h"

extern int main(int argc, char** argv);

// For C++
extern void (*__preinit_array_start []) (void) __attribute__((weak));
extern void (*__preinit_array_end []) (void) __attribute__((weak));
extern void (*__init_array_start []) (void) __attribute__((weak));
extern void (*__init_array_end []) (void) __attribute__((weak));

char** _environ;

void _start(int argc, char** argv, char **envp) {
  int code;
  _environ = envp;

  // C++
  for (void (**pp)() = __preinit_array_start; pp < __preinit_array_end; ++pp)
    (**pp)();
  for (void (**pp)() = __init_array_start; pp < __init_array_end; ++pp)
    (**pp)();

  code = main(argc, argv);
  exit(code);
}
