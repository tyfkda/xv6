#pragma once

#include "types.h"
#include "../commonsrc/commonsrc.h"

struct buf;
struct context;
struct file;
struct inode;
struct pipe;
struct proc;
struct rtcdate;
struct spinlock;
struct sleeplock;
struct stat;
struct superblock;

// bio.c
void            binit(void);
struct buf*     bread(uint, uint);
void            brelse(struct buf*);
void            bwrite(struct buf*);

// console.c
void            consoleinit(void);
void            cprintf(char*, ...);
void            consoleintr(int(*)(void));
void            panic(char*) __attribute__((noreturn));

// exec.c
int             exec(const char*, const char* const*);

// file.c
struct file*    filealloc(void);
void            fileclose(struct file*, int error);
struct file*    filedup(struct file*);
void            fileinit(void);
int             fileread(struct file*, void*, int n);
int             filereaddir(struct file *f, void *addr);
int             filestat(struct file*, struct stat*);
int             filewrite(struct file*, void*, int n);
int             filetruncate(struct file*, uint);
int             fileisatty(struct file*);

// fs.c
void            readsb(int dev, struct superblock *sb);
int             dirlink(struct inode*, const char*, uint);
struct inode*   dirlookup(struct inode*, const char*, uint*);
struct inode*   ialloc(uint, short);
struct inode*   idup(struct inode*);
void            iinit(int dev);
void            ilock(struct inode*);
void            iput(struct inode*);
void            iunlock(struct inode*);
void            iunlockput(struct inode*);
void            iupdate(struct inode*);
int             namecmp(const char*, const char*);
struct inode*   namei(const char*);
struct inode*   nameiparent(const char*, char*);
int             readi(struct inode*, void*, uint, uint);
void            stati(struct inode*, struct stat*);
int             writei(struct inode*, void*, uint, uint);
void            isetsize(struct inode*, uint);

// ide.c
void            ideinit(void);
void            ideintr(void);
void            iderw(struct buf*);

// ioapic.c
void            ioapicenable(int irq, int cpu);
extern uchar    ioapicid;
void            ioapicinit(void);

// kalloc.c
char*           kalloc(void);
void            kfree(char*);
void            kinit1(void*, void*);
void            kinit2(void*, void*);

// kbd.c
void            kbdintr(void);

// lapic.c
void            cmostime(struct rtcdate *r);
int             lapicid(void);
extern volatile uint*    lapic;
void            lapiceoi(void);
void            lapicinit(void);
void            lapicstartap(uchar, uint);
void            microdelay(int);
uint            cmosepochtime(void);

// log.c
void            initlog(int dev);
void            log_write(struct buf*);
void            begin_op();
void            end_op();

// mp.c
extern int      ismp;
void            mpinit(void);

// apic.c
int             acpiinit(void);

// picirq.c
void            picenable(int);
void            picinit(void);

// pipe.c
int             pipealloc(struct file**, struct file**);
void            pipeclose(struct pipe*, int writable, int error);
int             piperead(struct pipe*, void*, int);
int             pipewrite(struct pipe*, void*, int);

//PAGEBREAK: 16
// proc.c
int             cpuid(void);
void            exit(int) __attribute__((noreturn));
int             fork(void);
int             growproc(int);
int             kill(int);
struct cpu*     mycpu(void);
struct proc*    myproc();
void            pinit(void);
void            procdump(void);
void            scheduler(void) __attribute__((noreturn));
void            sched(void);
void            setproc(struct proc*);
void            sleep(void*, struct spinlock*);
void            userinit(void);
int             wait(int*);
void            wakeup(void*);
void            yield(void);

// swtch.S
void            swtch(struct context**, struct context*);

// spinlock.c
void            acquire(struct spinlock*);
void            getcallerpcs(void*, uintp*);
void            getstackpcs(uintp*, uintp*);
int             holding(struct spinlock*);
void            initlock(struct spinlock*, char*);
void            release(struct spinlock*);
void            pushcli(void);
void            popcli(void);

// sleeplock.c
void            acquiresleep(struct sleeplock*);
void            releasesleep(struct sleeplock*);
int             holdingsleep(struct sleeplock*);
void            initsleeplock(struct sleeplock*, char*);

// syscall.c
int             argint(int, int*);
int             argptr(int, char**, int);
int             argcstr(int, const char**);
int             arguintp(int, uintp*);
int             fetchuintp(uintp, uintp*);
int             fetchstr(uintp, const char**);
void            syscall(void);

// sysfile.c
int             argfd(int n, int *pfd, struct file **pf);

// timer.c
void            timerinit(void);

// trap.c
void            idtinit(void);
extern uint     ticks;
void            tvinit(void);
extern struct spinlock tickslock;

// uart.c
void		uartearlyinit(void);
void            uartinit(void);
void            uartintr(void);
void            uartputc(int);

// vm.c
void            seginit(void);
void            kvmalloc(void);
pde_t*          setupkvm(void);
char*           uva2ka(pde_t*, char*);
int             allocuvm(pde_t*, uint, uint);
int             deallocuvm(pde_t*, uintp, uintp);
void            freevm(pde_t*);
void            inituvm(pde_t*, char*, uint);
int             loaduvm(pde_t*, char*, struct inode*, uint, uint);
pde_t*          copyuvm(pde_t*, uint);
void            switchuvm(struct proc*);
void            switchkvm(void);
int             copyout(pde_t*, uint, const void*, uint);
void            clearpteu(pde_t *pgdir, char *uva);

// number of elements in fixed-size array
#define NELEM(x) (sizeof(x)/sizeof((x)[0]))
