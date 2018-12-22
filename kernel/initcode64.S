# Initial process execs /init.
# This code runs in user space.

#include "syscall.h"
#include "traps.h"


# exec(init, argv)
.globl start
start:
  mov $init, %rdi
  mov $argv, %rsi
  mov $0, %rdx  // envp
  mov $SYS_execve, %rax
  int $T_SYSCALL

# for(;;) exit();
exit:
  mov $SYS_exit, %rax
  int $T_SYSCALL
  jmp exit

# char init[] = "/bin/init\0";
init:
  .string "/bin/init\0"

# char *argv[] = { init, 0 };
.p2align 2
argv:
  .quad init
  .quad 0
