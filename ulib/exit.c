#include "stdlib.h"
#include "exit.h"
#include "unistd.h"  // for NULL

extern int _sysexit(int) __attribute__((noreturn));

struct atexitinfo __atexit;

int exit(int code) {
  if (__atexit.exiting)
    return -1;
  __atexit.exiting = 1;

  ExitFuncList* p = __atexit.exitFuncList;
  __atexit.exitFuncList = NULL;
  for (; p != NULL; ) {
    void (*function)(void) = p->function;
    p = p->next;
    (*function)();
  }

  return _sysexit(code);
}
