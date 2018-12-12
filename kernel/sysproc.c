#include "types.h"
#include "x86.h"
#include "defs.h"
#include "date.h"
#include "param.h"
#include "memlayout.h"
#include "mmu.h"
#include "proc.h"

int
sys_fork(void)
{
  return fork();
}

void
sys_exit(void)
{
  int code;

  if(argint(0, &code) < 0)
    //return -1;
    code = -1;
  exit(code);
}

int
sys_waitpid(void)
{
  int pid, option;
  uintp p;
  if(argint(0, &pid) < 0 || arguintp(1, &p) < 0 || argint(2, &option) < 0)
    return -1;
  return waitpid(pid, (int*)p, option);
}

int
sys_kill(void)
{
  int pid;

  if(argint(0, &pid) < 0)
    return -1;
  return kill(pid);
}

int
sys_getpid(void)
{
  return myproc()->pid;
}

uintp
sys_sbrk(void)
{
  uintp addr;
  uintp n;

  if(arguintp(0, &n) < 0)
    return -1;
  addr = myproc()->dataend;
  if(growproc(n) < 0)
    return -1;
  if (addr == 0)
    addr = myproc()->datastart;
  return addr;
}

int
sys_sleep(void)
{
  int n;
  uint ticks0;

  if(argint(0, &n) < 0)
    return -1;
  acquire(&tickslock);
  ticks0 = ticks;
  while(ticks - ticks0 < n){
    if(myproc()->killed){
      release(&tickslock);
      return -1;
    }
    sleep(&ticks, &tickslock);
  }
  release(&tickslock);
  return 0;
}

// return how many clock tick interrupts have occurred
// since start.
int
sys_uptime(void)
{
  uint xticks;

  acquire(&tickslock);
  xticks = ticks;
  release(&tickslock);
  return xticks;
}
