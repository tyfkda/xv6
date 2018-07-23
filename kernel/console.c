// Console input and output.
// Input is from the keyboard or serial port.
// Output is written to the screen and serial port.

#include <stdarg.h>

#include "types.h"
#include "defs.h"
#include "param.h"
#include "traps.h"
#include "spinlock.h"
#include "sleeplock.h"
#include "fs.h"
#include "file.h"
#include "memlayout.h"
#include "mmu.h"
#include "proc.h"
#include "x86.h"
#include "input.h"

static void consputc(int);
static void consuartputc(int);

static int panicked = 0;

static struct {
  struct spinlock lock;
  int locking;
} cons;

static char digits[] = "0123456789abcdef";

static void
printptr(uintp x) {
  int i;
  for (i = 0; i < (sizeof(uintp) * 2); i++, x <<= 4)
    consuartputc(digits[x >> (sizeof(uintp) * 8 - 4)]);
}

static void
printint(int xx, int base, int sign)
{
  char buf[16];
  int i;
  uint x;

  if(sign && (sign = xx < 0))
    x = -xx;
  else
    x = xx;

  i = 0;
  do{
    buf[i++] = digits[x % base];
  }while((x /= base) != 0);

  if(sign)
    buf[i++] = '-';

  while(--i >= 0)
    consuartputc(buf[i]);
}
//PAGEBREAK: 50

// Print to the console. only understands %d, %x, %p, %s.
void
cprintf(char *fmt, ...)
{
  va_list ap;
  int i, c, locking;
  char *s;

  va_start(ap, fmt);

  locking = cons.locking;
  if(locking)
    acquire(&cons.lock);

  if (fmt == 0)
    panic("null fmt");

  for(i = 0; (c = fmt[i] & 0xff) != 0; i++){
    if(c != '%'){
      consuartputc(c);
      continue;
    }
    c = fmt[++i] & 0xff;
    if(c == 0)
      break;
    switch(c){
    case 'd':
      printint(va_arg(ap, int), 10, 1);
      break;
    case 'x':
      printint(va_arg(ap, int), 16, 0);
      break;
    case 'p':
      printptr(va_arg(ap, uintp));
      break;
    case 's':
      if((s = va_arg(ap, char*)) == 0)
        s = "(null)";
      for(; *s; s++)
        consuartputc(*s);
      break;
    case '%':
      consuartputc('%');
      break;
    default:
      // Print unknown % sequence to draw attention.
      consuartputc('%');
      consuartputc(c);
      break;
    }
  }

  va_end(ap);

  if(locking)
    release(&cons.lock);
}

void
panic(char *s)
{
  int i;
  uintp pcs[10];

  cli();
  cons.locking = 0;
  // use lapiccpunum so that we can call panic from mycpu()
  cprintf("lapicid %d: panic: ", lapicid());
  cprintf(s);
  cprintf("\n");
  getcallerpcs(&s, pcs);
  for(i=0; i<10; i++)
    cprintf(" %p", pcs[i]);
  panicked = 1; // freeze other CPU
  for(;;)
    ;
}

//PAGEBREAK: 50
#define CRTPORT 0x3d4
static ushort *crt = (ushort*)P2V(0xb8000);  // CGA memory

const int SCRW = 80, SCRH = 25;

static void
cgaputc(int c)
{
  const ushort ATTR = 0x0700;  // black on white
  int pos;

  // Cursor position: col + SCRW * row.
  outb(CRTPORT, 14);
  pos = inb(CRTPORT + 1) << 8;
  outb(CRTPORT, 15);
  pos |= inb(CRTPORT + 1);

  if(c == '\n')
    pos += SCRW - pos % SCRW;
  else if(c == '\b'){
    if(pos > 0)
      --pos;
  } else
    crt[pos++] = (c & 0xff) | ATTR;

  if(pos >= SCRW * SCRH){  // Scroll up.
    memmove(crt, crt + SCRW, sizeof(crt[0]) * SCRW * (SCRH - 1));
    pos -= SCRW;
    for (int i = pos; i < SCRW * SCRH; ++i)
      crt[i] = 0x00 | ATTR;
  }

  if(pos < 0 || pos >= SCRW * SCRH)
    panic("pos under/overflow");

  outb(CRTPORT, 14);
  outb(CRTPORT + 1, pos >> 8);
  outb(CRTPORT, 15);
  outb(CRTPORT + 1, pos);
}

static void
consputc(int c)
{
  if(panicked){
    cli();
    for(;;)
      ;
  }
  cgaputc(c);
}

static void
consuartputc(int c)
{
  consputc(c);
  uartputc(c);
}

int
consolewrite(const void *buf_, int n)
{
  const uchar* buf = buf_;
  int i;

  acquire(&cons.lock);
  for(i = 0; i < n; i++)
    consputc(buf[i]);
  release(&cons.lock);

  return n;
}

static struct input s_input;

void
consoleintr(int (*getc)(void))
{
  acquire(&cons.lock);
  inputintr(&s_input, getc, consputc);
  release(&cons.lock);
}

int
consoleread(void *dst, int n)
{
  return inputread(&s_input, dst, n);
}

void
consoleinit(void)
{
  initlock(&cons.lock, "console");

  devsw[CONSOLE].write = consolewrite;
  devsw[CONSOLE].read = consoleread;
  cons.locking = 1;

  ioapicenable(IRQ_KBD, 0);

  inputinit(&s_input);
}
