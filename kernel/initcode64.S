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

# char init[] = "init\0";
init:
  .string "init\0"

# char init_env[] = "TERM=xv6\0";
init_env_term:
  .string "TERM=xv6\0"

init_env_path:
  .string "PATH=/sbin:/bin:/usr/sbin:/usr/bin\0"
	
# char *argv[] = { init, 0 };
.p2align 2
argv:
  .quad init
  .quad 0

# char *envp[] = { init_env, 0 };
.p2align 2
envp:
  .quad init_env_term
  .quad init_env_path	
  .quad 0
