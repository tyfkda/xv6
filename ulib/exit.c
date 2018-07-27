#include "user.h"

extern int _sysexit(int) __attribute__((noreturn));

typedef struct ExitFuncList {
  struct ExitFuncList* next;
  void (*function)(void);
} ExitFuncList;

static ExitFuncList* s_exitFuncList;
static char exiting;

int atexit(void (*function)(void)) {
  if (exiting)
    return -1;

  ExitFuncList* p = (ExitFuncList*)malloc(sizeof(*p));
  if (p == 0)
    return -1;
  p->next = s_exitFuncList;
  p->function = function;
  s_exitFuncList = p;
  return 0;
}

int exit(int code) {
  if (exiting)
    return -1;
  exiting = 1;

  ExitFuncList* p = s_exitFuncList;
  s_exitFuncList = 0;
  for (; p != 0; ) {
    void (*function)(void) = p->function;
    p = p->next;
    (*function)();
  }

  return _sysexit(code);
}
