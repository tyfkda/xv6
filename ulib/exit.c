#include "stdlib.h"
#include "exit.h"
#include "unistd.h"  // for NULL

extern void _sysexit(int) __attribute__((noreturn));

// For C++
extern void (*__fini_array_start []) (void) __attribute__((weak));
extern void (*__fini_array_end []) (void) __attribute__((weak));

struct atexitinfo __atexit;

void exit(int code) {
  if (!__atexit.exiting) {
    __atexit.exiting = 1;

    for (void (**pp)() = __fini_array_start; pp < __fini_array_end; ++pp)
      (**pp)();

    ExitFuncList* p = __atexit.exitFuncList;
    __atexit.exitFuncList = NULL;
    for (; p != NULL; ) {
      void (*function)(void) = p->function;
      p = p->next;
      (*function)();
    }
  }
  _sysexit(code);
}
