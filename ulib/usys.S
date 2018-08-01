#include "../kernel/syscall.h"
#include "../kernel/traps.h"

#define SYSCALL2(name, syslabel) \
  .globl name; \
  name: \
    movl $ ## syslabel, %eax; \
    int $T_SYSCALL; \
    ret

#define SYSCALL(name)  SYSCALL2(name, SYS_ ## name)

// If return value is negative, then set flipped value into 'errno' and returns -1.
#ifdef X64
#define SETERRNO() \
    test %eax, %eax; \
    jns 1f; \
    neg %eax; \
    mov %eax, errno(%rip); \
    mov $-1, %eax; \
  1:
#else
#define SETERRNO() \
    test %eax, %eax; \
    jns 1f; \
    neg %eax; \
    mov %eax, errno; \
    mov $-1, %eax; \
  1:
#endif

#define SYSCALL_WITH_ERROR(name) \
  .globl name; \
  name: \
    movl $ SYS_ ## name, %eax; \
    int $T_SYSCALL; \
    SETERRNO(); \
    ret

SYSCALL(fork)
SYSCALL2(_sysexit, SYS_exit)
SYSCALL(wait)
SYSCALL(pipe)
SYSCALL(read)
SYSCALL(write)
SYSCALL(close)
SYSCALL(kill)
SYSCALL(exec)
SYSCALL_WITH_ERROR(open)
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
SYSCALL(ioctl)
SYSCALL(isatty)
SYSCALL(ftruncate)
