#include "defs.h"
#include "errno.h"
#include "memlayout.h"
#include "mmu.h"
#include "param.h"
#include "proc.h"
#include "spinlock.h"
#include "x86.h"

struct {
  struct spinlock lock;
  struct proc proc[NPROC];
} ptable;

int nextpid = 1;
extern void forkret(void);
extern void trapret(void);

// Must be called with interrupts disabled
int
cpuid() {
  return mycpu()-cpus;
}

// Must be called with interrupts disabled to avoid the caller being
// rescheduled between reading lapicid and running through the loop.
struct cpu*
mycpu(void)
{
  return 0;
}

// Disable interrupts so that we are not rescheduled
// while reading proc from the cpu structure
struct proc*
myproc(void) {
  return 0;
}

// Exit the current process.  Does not return.
// An exited process remains in the zombie state
// until its parent calls wait() to find out it exited.
void
exit(int code)
{
  panic("zombie exit");
}

// Give up the CPU for one scheduling round.
void
yield(void)
{
}

// Wake up all processes sleeping on chan.
void
wakeup(void *chan)
{
}
