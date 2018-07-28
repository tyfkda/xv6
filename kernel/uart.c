// Intel 8250 serial port (UART).

#include "defs.h"
#include "input.h"
#include "traps.h"
#include "x86.h"

#define COM1    0x3f8
#define ESC     (0x1b)

//#define CURSORUP        (0x001b5b41)
//#define CURSORDOWN      (0x001b5b42)
#define CURSORFORWARD   (0x001b5b43)
#define CURSORBACKWARD  (0x001b5b44)

#define C(x)  ((x)-'@')  // Control-x

static int uart;    // is there a uart?

static struct {
  struct spinlock lock;
  int locking;
  int escape;

  int bufCount;
  int emitIndex;
  uchar buf[4];
} cons;

static struct input s_input;

void
uartputc(int c)
{
  int i;

  if(!uart)
    return;
  for(i = 0; i < 128 && !(inb(COM1+5) & 0x20); i++)
    microdelay(10);
  outb(COM1+0, c);
}

static int
uartemit(void)
{
  int c = cons.buf[cons.emitIndex];
  if (++cons.emitIndex >= cons.bufCount) {
    cons.emitIndex = -1;  // Emission ended.
    cons.bufCount = 0;
  }
  return c;
}

static int
uartgetc(void)
{
  const int NOINPUT = -1;

  if(!uart)
    return NOINPUT;

  if (cons.emitIndex >= 0) {
    return uartemit();
  }

  if(!(inb(COM1+5) & 0x01))
    return NOINPUT;

  int c = inb(COM1+0);
  if (cons.escape != 0) {
    cons.buf[cons.bufCount++] = c;
    c = (cons.escape << 8) | c;
    switch (c) {
    case (ESC << 8) | '[':
      cons.escape = c;
      c = NOINPUT;
      break;
    default:
      cons.escape = 0;
      switch (c) {
      case CURSORBACKWARD:
        c = C('B');
        break;
      case CURSORFORWARD:
        c = C('F');
        break;
      default:  // Not handled: pass through.
        cons.emitIndex = 0;
        return uartemit();
      }
      break;
    }
  } else if (c == ESC && !s_input.nobuffering) {
    cons.buf[cons.bufCount++] = c;
    cons.escape = c;
    c = NOINPUT;
  }
  return c;
}

int
uartread(void *dst, int n)
{
  return inputread(&s_input, dst, n);
}

int
uartwrite(const void *buf_, int n)
{
  const uchar* buf = buf_;
  int i;

  acquire(&cons.lock);
  for(i = 0; i < n; i++) {
    uartputc(buf[i]);
  }
  release(&cons.lock);

  return n;
}

int
uartioctl(int request, int flag)
{
  s_input.nobuffering = flag & 1;
  s_input.noechoback = (flag >> 1) & 1;
  return 0;
}

void
uartearlyinit(void)
{
  char *p;

  // Turn off the FIFO
  outb(COM1+2, 0);

  // 9600 baud, 8 data bits, 1 stop bit, parity off.
  outb(COM1+3, 0x80);    // Unlock divisor
  outb(COM1+0, 115200/9600);
  outb(COM1+1, 0);
  outb(COM1+3, 0x03);    // Lock divisor, 8 data bits.
  outb(COM1+4, 0);
  outb(COM1+1, 0x01);    // Enable receive interrupts.

  // If status is 0xFF, no serial port.
  if(inb(COM1+5) == 0xFF)
    return;
  uart = 1;

  // Announce that we're here.
  for(p="xv6...\n"; *p; p++)
    uartputc(*p);
}

void
uartinit(void)
{
  if (!uart)
    return;

  // Acknowledge pre-existing interrupt conditions;
  // enable interrupts.
  inb(COM1+2);
  inb(COM1+0);
  ioapicenable(IRQ_COM1, 0);


  initlock(&cons.lock, "uart");
  cons.escape = 0;
  cons.bufCount = 0;
  cons.emitIndex = -1;

  devsw[UART].write = uartwrite;
  devsw[UART].read = uartread;
  devsw[UART].ioctl = uartioctl;
  cons.locking = 1;

  inputinit(&s_input);
}

void
uartintr(void)
{
  acquire(&cons.lock);
  inputintr(&s_input, uartgetc, uartputc);
  release(&cons.lock);
}
