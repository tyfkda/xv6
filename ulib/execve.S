#include "../kernel/syscall.h"
#include "../kernel/traps.h"
#include "syscallmacro.h"

SYSCALL2(_sysexecve, SYS_execve)
