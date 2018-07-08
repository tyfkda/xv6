// Intel 8250 serial port (UART).

#include "defs.h"
#include "input.h"
#include "traps.h"
#include "x86.h"

#define COM1    0x3f8

static int uart;    // is there a uart?

static struct {
  struct spinlock lock;
  int locking;
} cons;

static struct input s_input;

static void
uartputc_(int c)
{
  int i;

  if(!uart)
    return;
  for(i = 0; i < 128 && !(inb(COM1+5) & 0x20); i++)
    microdelay(10);
  outb(COM1+0, c);
}

void
uartputc(int c)
{
  if(c == BACKSPACE){
    uartputc_('\b');
    uartputc_(' ');
    uartputc_('\b');
  } else
    uartputc_(c);
}

static int
uartgetc(void)
{
  if(!uart)
    return -1;
  if(!(inb(COM1+5) & 0x01))
    return -1;
  return inb(COM1+0);
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
    uartputc_(buf[i]);
  }
  release(&cons.lock);

  return n;
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
    uartputc_(*p);
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

  devsw[UART].write = uartwrite;
  devsw[UART].read = uartread;
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
