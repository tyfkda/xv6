#include "stdlib.h"
//#include "exit.h"
#include "unistd.h"  // for NULL

extern void _sysexit(int) __attribute__((noreturn));

// For C++
extern void (*__fini_array_start []) (void) __attribute__((weak));
extern void (*__fini_array_end []) (void) __attribute__((weak));

void _exit(int code) {
  for (void (**pp)() = __fini_array_start; pp < __fini_array_end; ++pp)
    (**pp)();

  _sysexit(code);
}
