#include <stdarg.h>

#include "types.h"
#include "user.h"

// Output is not '\0' terminated.
static int
snprintint(char *out, uint n, int xx, int base, int sgn)
{
  static char digits[] = "0123456789ABCDEF";
  char buf[16];
  int i, neg, o;
  uint x;

  neg = 0;
  if(sgn && xx < 0){
    neg = 1;
    x = -xx;
  } else {
    x = xx;
  }

  i = 0;
  do{
    buf[i++] = digits[x % base];
  }while((x /= base) != 0);
  if(neg)
    buf[i++] = '-';

  for (o = 0; --i >= 0 && o < n; ++o)
    out[o] = buf[i];

  return o;
}

int
vsnprintf(char *out, uint n, const char *fmt, va_list ap)
{
  char *s;
  int c, i;
  int o;


  for(i = o = 0; fmt[i] != '\0' && o < n; i++){
    c = fmt[i] & 0xff;
    if(c != '%'){
      out[o++] = c;
      continue;
    }

    // Handle '%'
    c = fmt[++i] & 0xff;
    if(c == 'd'){
      o += snprintint(out + o, n - o, va_arg(ap, int), 10, 1);
    } else if(c == 'x' || c == 'p'){
      o += snprintint(out + o, n - o, va_arg(ap, int), 16, 0);
    } else if(c == 's'){
      s = va_arg(ap, char*);
      if(s == 0)
        s = "(null)";
      while(*s != '\0' && o < n)
        out[o++] = *s++;
    } else if(c == 'c'){
      out[o++] = va_arg(ap, uint);
    } else if(c == '%'){
      out[o++] = c;
    } else {
      // Unknown % sequence.  Print it to draw attention.
      out[o++] = '%';
      if (o >= n)
        break;
      out[o++] = c;
    }
  }

  if (o < n)
    out[o] = '\0';
  return o;
}

int
snprintf(char *out, uint n, const char *fmt, ...)
{
  va_list ap;
  int len;
  va_start(ap, fmt);
  len = vsnprintf(out, n, fmt, ap);
  va_end(ap);
  return len;
}

int
sprintf(char *out, const char *fmt, ...)
{
  va_list ap;
  int len;
  va_start(ap, fmt);
  len = vsnprintf(out, (uint)-1, fmt, ap);
  va_end(ap);
  return len;
}

// Print to the given fd. Only understands %d, %x, %p, %s.
int
printf(int fd, const char *fmt, ...)
{
  // TODO: directly output to fd, not use vsnprintf.
  va_list ap;
  char buf[1024];
  int len;

  va_start(ap, fmt);
  len = vsnprintf(buf, sizeof(buf), fmt, ap);
  va_end(ap);
  return write(fd, buf, len);
}
