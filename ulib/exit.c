#include "stdlib.h"
#include "exit.h"
#include "unistd.h"  // for NULL

extern void _sysexit(int) __attribute__((noreturn));

struct atexitinfo __atexit;

void exit(int code) {
  if (!__atexit.exiting) {
    __atexit.exiting = 1;

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
