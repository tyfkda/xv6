#include "input.h"
#include "defs.h"
#include "proc.h"
#include "x86.h"

#define C(x)  ((x)-'@')  // Control-x

void
inputinit(struct input *input)
{
  initlock(&input->lock, "input");
  input->r = input->w = input->e = 0;
}

void
inputintr(struct input *input, int (*getc)(void), void (*putc)(int))
{
  int c;
  int doprocdump = 0;

  acquire(&input->lock);
  while((c = getc()) >= 0){
    switch(c){
    case C('Z'): // reboot
      lidt(0,0);
      break;
    case C('P'):  // Process listing.
      // procdump() locks cons.lock indirectly; invoke later
      doprocdump = 1;
      break;
    case C('U'):  // Kill line.
      while(input->e != input->w &&
            input->buf[(input->e-1) % INPUT_BUF] != '\n'){
        input->e--;
        putc(BACKSPACE);
      }
      break;
    case C('H'): case '\x7f':  // Backspace
      if(input->e != input->w){
        input->e--;
        putc(BACKSPACE);
      }
      break;
    default:
      if(c != 0 && input->e-input->r < INPUT_BUF){
        c = (c == '\r') ? '\n' : c;
        input->buf[input->e++ % INPUT_BUF] = c;
        putc(c);
        if(c == '\n' || c == C('D') || input->e == input->r+INPUT_BUF){
          input->w = input->e;
          wakeup(&input->r);
        }
      }
      break;
    }
  }
  release(&input->lock);
  if(doprocdump) {
    procdump();  // now call procdump() wo. cons.lock held
  }
}

int
inputread(struct input *input, void *dst_, int n)
{
  uchar* dst = dst_;
  uint target;
  int c;

  target = n;
  acquire(&input->lock);
  while(n > 0){
    while(input->r == input->w){
      if(myproc()->killed){
        release(&input->lock);
        return -1;
      }
      sleep(&input->r, &input->lock);
    }
    c = input->buf[input->r++ % INPUT_BUF];
    if(c == C('D')){  // EOF
      if(n < target){
        // Save ^D for next time, to make sure
        // caller gets a 0-byte result.
        input->r--;
      }
      break;
    }
    *dst++ = c;
    --n;
    if(c == '\n')
      break;
  }
  release(&input->lock);

  return target - n;
}
