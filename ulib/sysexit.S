#include "../kernel/syscall.h"
#include "../kernel/traps.h"
#include "syscallmacro.h"

SYSCALL2(_sysexit, SYS_exit)
