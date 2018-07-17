#include "../kernel/syscall.h"
#include "../kernel/traps.h"

#define SYSCALL2(name, syslabel) \
  .globl name; \
  name: \
    movl $ ## syslabel, %eax; \
    int $T_SYSCALL; \
    ret

#define SYSCALL(name)  SYSCALL2(name, SYS_ ## name)

SYSCALL(fork)
SYSCALL(exit)
SYSCALL(wait)
SYSCALL(pipe)
SYSCALL(read)
SYSCALL(write)
SYSCALL(close)
SYSCALL(kill)
SYSCALL(exec)
SYSCALL(open)
SYSCALL(mknod)
SYSCALL(unlink)
SYSCALL(fstat)
SYSCALL(link)
SYSCALL(mkdir)
SYSCALL(chdir)
SYSCALL(dup)
SYSCALL(getpid)
SYSCALL(sbrk)
SYSCALL(sleep)
SYSCALL(uptime)
SYSCALL2(_systime, SYS_time)
