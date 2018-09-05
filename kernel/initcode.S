# Initial process execs /init.
# This code runs in user space.

#include "syscall.h"
#include "traps.h"


# exec(init, argv)
.globl start
start:
  pushl $envp
  pushl $argv
  pushl $init
  pushl $0  // where caller pc would be
  movl $SYS_execve, %eax
  int $T_SYSCALL

# for(;;) exit();
exit:
  movl $SYS_exit, %eax
  int $T_SYSCALL
  jmp exit

# char init[] = "/bin/init\0";
init:
  .string "/bin/init\0"

# char init_env[] = "PATH=/bin\0";
init_env:
  .string "PATH=/bin\0"

# char *argv[] = { init, 0 };
.p2align 2
argv:
  .long init
  .long 0

# char *envp[] = { init_env, 0 };
.p2align 2
envp:
  .long init_env
  .long 0
