#include "stdlib.h"
#include "exit.h"
#include "unistd.h"  // for NULL

int atexit(void (*function)(void)) {
  if (__atexit.exiting)
    return -1;

  ExitFuncList* p = (ExitFuncList*)malloc(sizeof(*p));
  if (p == NULL)
    return -1;
  // TODO: Lock
  p->next = __atexit.exitFuncList;
  __atexit.exitFuncList = p;
  p->function = function;
  return 0;
}
