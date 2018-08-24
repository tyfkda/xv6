#pragma once

typedef struct ExitFuncList {
  struct ExitFuncList* next;
  void (*function)(void);
} ExitFuncList;

struct atexitinfo {
  ExitFuncList* exitFuncList;
  char exiting;
};

extern struct atexitinfo __atexit;
