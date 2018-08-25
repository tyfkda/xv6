# Initial process execs /init.

#include "syscall.h"
#include "traps.h"


# exec(init, argv)
.globl start
start:
  mov $init, %rdi
  mov $argv, %rsi
  mov $envp, %rdx	
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

# char init_env[] = "PATH=/bin\0";
init_env:
  .string "PATH=/bin\0"

# char *argv[] = { init, 0 };
.p2align 2
argv:
  .quad init
  .quad 0


# char *envp[] = { init_env, 0 };
.p2align 2
envp:
  .quad init_env
  .quad 0
