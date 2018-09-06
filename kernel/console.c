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
#include "sys/ioctl.h"

static void consputc(int);
static void consuartputc(int);

static int panicked = 0;

static struct {
  struct spinlock lock;
  int locking;

  int bufCount;
  uchar buf[16];

  ushort attr;
  int flipColor;
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
const int ESC = '\x1b';

static const ushort kColorTable[16] = {
  0,   // Black
  4,   // Red
  2,   // Green
  6,   // Orange
  1,   // Blue
  5,   // Purple
  3,   // Cyan
  7,   // White
  8,   // Gray
  12,
  10,
  14,
  9,
  13,
  11,
  15,
};
const int kDefaultColor = 15;
const int kDefaultBgColor = 0;

static struct input s_input;

static void
setFontColor(int c)
{
  if (!cons.flipColor)
    cons.attr = (cons.attr & 0xf0ff) | (kColorTable[c] << 8);
  else
    cons.attr = (cons.attr & 0x0fff) | (kColorTable[c] << 12);
}

static void
setBgColor(int c)
{
  if (!cons.flipColor)
    cons.attr = (cons.attr & 0x0fff) | (kColorTable[c] << 12);
  else
    cons.attr = (cons.attr & 0xf0ff) | (kColorTable[c] << 8);
}

static int getCursorPos(void) {
  // Cursor position: col + SCRW * row.
  outb(CRTPORT, 14);
  int pos = inb(CRTPORT + 1) << 8;
  outb(CRTPORT, 15);
  pos |= inb(CRTPORT + 1);
  return pos;
}

static void setCursorPos(int pos) {
  outb(CRTPORT, 14);
  outb(CRTPORT + 1, pos >> 8);
  outb(CRTPORT, 15);
  outb(CRTPORT + 1, pos);
}

static int
scrollUp(int pos)
{
  memmove(crt, crt + SCRW, sizeof(crt[0]) * SCRW * (SCRH - 1));
  pos -= SCRW;
  for (int i = pos; i < SCRW * SCRH; ++i)
    crt[i] = cons.attr;
  return pos;
}

static void
cgaputc(int c)
{
  int pos;

  // Cursor position: col + SCRW * row.
  pos = getCursorPos();

  if(c == '\n') {
    if (!s_input.noechoback || pos < SCRW * (SCRH - 1))
      pos += SCRW - pos % SCRW;
  } else if (c == '\r') {
    pos -= pos % SCRW;
  } else if(c == '\b'){
    if(pos > 0)
      --pos;
  } else {
    crt[pos] = (c & 0xff) | cons.attr;
    if (!s_input.noechoback || pos % SCRW < SCRW - 1)
      ++pos;
  }

  if(pos >= SCRW * SCRH){
    if (s_input.noechoback)
      pos = SCRW * SCRH - 1;
    else
      pos = scrollUp(pos);
  }

  if(pos < 0 || pos >= SCRW * SCRH)
    panic("pos under/overflow");

  setCursorPos(pos);
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
  //uartputc(c);
}

int
consolewrite(const void *buf_, int n)
{
  const uchar* buf = buf_;
  int i;

  acquire(&cons.lock);
  for(i = 0; i < n; i++) {
    uchar c = buf[i];
    consputc(c);
  }
  release(&cons.lock);

  return n;
}

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

int
consoleioctl(int request, uintp flag)
{
  switch (request) {
  case TCGETS:
    {
      int value = 0;
      if (s_input.nobuffering)
        value |= 1 << 0;
      if (s_input.noechoback)
        value |= 1 << 1;
      return value;
    }

  case TCSETS:
  case TCSETSW:
  case TCSETSF:
    s_input.nobuffering = flag & 1;
    s_input.noechoback = (flag >> 1) & 1;
    return 0;

  default:
    return -1;
  }
  return 0;
}

void
consoleinit(void)
{
  initlock(&cons.lock, "console");

  devsw[CONSOLE].write = consolewrite;
  devsw[CONSOLE].read = consoleread;
  devsw[CONSOLE].ioctl = consoleioctl;
  cons.locking = 1;
  cons.bufCount = 0;
  cons.attr = 0x0000;
  cons.flipColor = 0;
  setFontColor(kDefaultColor);
  setBgColor(kDefaultBgColor);

  ioapicenable(IRQ_KBD, 0);

  inputinit(&s_input);
}
