#include "input.h"
#include "defs.h"
#include "proc.h"
#include "x86.h"

#define C(x)  ((x)-'@')  // Control-x

void
inputinit(struct input *input)
{
  initlock(&input->lock, "input");
  input->r = input->w = input->e = input->c = 0;
}

void
inputintr(struct input *input, int (*getc)(void), void (*putc)(int))
{
  int c;
  int doprocdump = 0;

  acquire(&input->lock);
  while((c = getc()) != -1){
    switch(c){
    case C('Z'): // reboot
      lidt(0,0);
      break;
    case C('P'):  // Process listing.
      // procdump() locks cons.lock indirectly; invoke later
      doprocdump = 1;
      break;
    case C('H'): case '\x7f':  // Backspace
      if(input->c != input->w){
        // Shift after cursor to the left.
        putc('\b');
        for (uint i = input->c; i < input->e; ++i) {
          int c = input->buf[i % INPUT_BUF];
          putc(c);
          input->buf[(i - 1) % INPUT_BUF] = c;
        }
        putc(' ');
        for (int n = input->e - input->c + 1; n > 0; --n)
          putc('\b');

        --input->c;
        --input->e;
      }
      break;
    case C('B'):  // Backward (Cursor left)
      if(input->c != input->w &&
         input->buf[(input->c - 1) % INPUT_BUF] != '\n'){
        --input->c;
        putc('\b');
      }
      break;
    case C('F'):  // Forward (Cursor right)
      if(input->c != input->e &&
         input->buf[(input->c + 1) % INPUT_BUF] != '\n'){
        putc(input->buf[input->c % INPUT_BUF]);  // Put same character to move right.
        ++input->c;
      }
      break;
    case C('A'):  // Beginning of line
      for (int i = input->c - input->w; i > 0; --i)
        putc('\b');
      input->c = input->w;
      break;
    case C('E'):  // End of line
      for (uint i = input->c; i < input->e; ++i)
        putc(input->buf[i % INPUT_BUF]);
      input->c = input->e;
      break;
    case C('K'):  // Kill
      if  (input->e > input->c) {
        int n = input->e - input->c;
        for (int i = 0; i < n; ++i)
          putc(' ');
        for (int i = 0; i < n; ++i)
          putc('\b');
        input->e = input->c;
      }
      break;
    default:
      if(c != 0 && input->e - input->r < INPUT_BUF){
        if (c == '\n' || c == '\r') {
          input->buf[input->e++ % INPUT_BUF] = '\n';
          input->w = input->c = input->e;
          putc('\n');
          wakeup(&input->r);
          break;
        }

        if ((c < ' ' && c != 0x1b) || c >= 0x80)
          break;

        // Shift after cursor to the right.
        for (uint i = input->e + 1; i > input->c; --i) {
          input->buf[i % INPUT_BUF] = input->buf[(i - 1) % INPUT_BUF];
        }
        input->buf[input->c % INPUT_BUF] = c;
        for (uint i = input->c; i <= input->e; ++i) {
          putc(input->buf[i % INPUT_BUF]);
        }
        for (int i = input->e - input->c; i > 0; --i) {
          putc('\b');
        }
        ++input->c;
        ++input->e;

        if(/*|| c == C('D') ||*/ input->e == input->r + INPUT_BUF){
          input->w = input->c = input->e;
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
