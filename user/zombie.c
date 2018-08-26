// Create a zombie process that
// must be reparented at exit.

#include "unistd.h"

extern void _sysexit(int);

void
_start(void)
{
  if(fork() > 0)
    sleep(5);  // Let child exit before parent.
  _sysexit(0);
}
