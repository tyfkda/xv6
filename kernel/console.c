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

  int escape;
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

static void
flipColor(void)
{
  ushort a = cons.attr;
  cons.attr = (a & 0x00ff) | ((a & 0xf000) >> 4) | ((a & 0x0f00) << 4);
  cons.flipColor = 1 - cons.flipColor;
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

static char*
putint(char*p, int x)
{
  if (x < 0) {
    *p++ = '-';
    x = -x;
  }

  char buf[sizeof(int) * 3 + 1];
  char* q = buf;
  for (int i = 0; i < sizeof(buf) - 1; ++i) {
    *q++ = (x % 10) + '0';
    x /= 10;
    if (x <= 0)
      break;
  }
  while (q > buf)
    *p++ = *(--q);
  // No nul-terminated.

  return p;
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
  uartputc(c);
}

static void
procescseq(uchar c)
{
  cons.buf[cons.bufCount++] = c;
  if (cons.buf[1] == '[') {
    switch (c) {
    case '?':
      //cons.question = TRUE;
      return;
    case 'B':
      // CUD: CUrsor Downward.
      {
        int d = atoi((char*)&cons.buf[2]);
        int pos = getCursorPos();
        int y = pos / SCRW;
        int ny = y + d;
        if (ny > SCRH - 1)
          ny = SCRH - 1;
        setCursorPos(pos + (ny - y) * SCRW);
      }
      break;
    case 'C':
      // CUF: CUrsor Forward.
      {
        int f = atoi((char*)&cons.buf[2]);
        int pos = getCursorPos();
        int x = pos % SCRW;
        int nx = x + f;
        if (nx > SCRW - 1)
          nx = SCRW - 1;
        setCursorPos(pos - x + nx);
      }
      break;
    case 'H':
      // CUP
      {
        // Set cursor position
        int cols = 0, rows = 0;
        for (int i = 2; i < cons.bufCount - 1; ++i) {
          if (cons.buf[i] == ';') {
            cols = atoi((char*)&cons.buf[2]) - 1;
            rows = atoi((char*)&cons.buf[i + 1]) - 1;
            cols = cols < 0 ? 0 : cols > SCRH - 1 ? SCRH - 1 : cols;
            rows = rows < 0 ? 0 : rows > SCRW - 1 ? SCRW - 1 : rows;
            break;
          }
        }
        setCursorPos(cols * SCRW + rows);
      }
      break;
    case 'J':
      // ED
      {
        // TODO: Consider SPA and ERM.
        int n = atoi((char*)&cons.buf[2]);
        if (0 <= n && n <= 2) {
          int pos = getCursorPos();
          int start = 0, end = SCRW * SCRH;
          switch (n) {
          case 0:  start = pos; break;
          case 1:  end = pos; break;
          }
          for (int i = start; i < end; ++i)
            crt[i] = cons.attr;
        }
      }
      break;
    case 'K':
      // EL
      {
        // TODO: Consider SPA and ERM.
        int n = atoi((char*)&cons.buf[2]);
        if (0 <= n && n <= 2) {
          int pos = getCursorPos();
          int y = pos / SCRW;
          int start = 0 + y * SCRW, end = 0 + (y + 1) * SCRW;
          switch (n) {
          case 0:  start = pos; break;
          case 1:  end = pos; break;
          }
          for (int i = start; i < end; ++i)
            crt[i] = cons.attr;
        }
      }
      break;
    case 'h':
      // DECSET
      // TODO: Implement
      break;
    case 'l':
      // DECRST
      // TODO: Implement
      break;
    case 'm':
      // SGR: Select Graphic Rendition
      {
        int v = atoi((char*)&cons.buf[2]);
        switch (v) {
        case 0:  // Reset
          cons.flipColor = 0;
          setFontColor(kDefaultColor);
          setBgColor(kDefaultBgColor);
          break;
        case 7:
          if (!cons.flipColor) {
            flipColor();
          }
          break;
        default:
          {
            int x = v % 10;
            switch (v / 10) {
            case 3: case 9:
              setFontColor(x == 9 ? kDefaultColor : x + (v >= 90 ? 8 : 0));
              break;
            case 4: case 10:
              setBgColor(x == 9 ? kDefaultBgColor : x + (v >= 100 ? 8 : 0));
              break;
            }
          }
          break;
        }
      }
      break;
    case 'n':
      // DSR: Device Status Report.
      switch (cons.buf[2]) {
      case '6':  // Return cursor position
        {
          int pos = getCursorPos();
          char buf[16];
          // Build "\x1b[%d;%dR"
          strncpy(buf, "\x1b[", sizeof(buf));
          char* p = putint(buf + 2, pos / SCRW + 1);
          *p++ = ';';
          p = putint(buf + 2, pos / SCRW + 1);
          *p++ = 'R';
          inputwrite(&s_input, buf, p - buf);
        }
        break;
      }
      break;
    default:
      if ((c < 'A'|| c > 'Z') && (c < 'a' || c > 'z') &&
          cons.bufCount < sizeof(cons.buf)) {
        return;  // Continue.
      }

      // Unhandled: emit buffered characters and quit escape sequence.
      for (int i = 0; i < cons.bufCount; ++i) {
        consputc(cons.buf[i]);
      }
      break;
    }
  }

  // Quit escape sequence.
  cons.escape = 0;
  cons.bufCount = 0;
}

int
consolewrite(const void *buf_, int n)
{
  const uchar* buf = buf_;
  int i;

  acquire(&cons.lock);
  for(i = 0; i < n; i++) {
    uchar c = buf[i];
    if (cons.escape) {
      procescseq(c);
    } else if (c == ESC) {
      cons.buf[0] = c;
      cons.bufCount = 1;
      cons.escape = 1;
    } else {
      consputc(c);
    }
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
  cons.escape = 0;
  cons.bufCount = 0;
  cons.attr = 0x0000;
  cons.flipColor = 0;
  setFontColor(kDefaultColor);
  setBgColor(kDefaultBgColor);

  ioapicenable(IRQ_KBD, 0);

  inputinit(&s_input);
}
