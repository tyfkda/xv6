#include "types.h"
#include "defs.h"
#include "proc.h"
#include "x86.h"

void
syscall(void)
{
  struct proc *curproc = myproc();

  curproc->tf->eax = -1;
}
