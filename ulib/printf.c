#include "file_def.h"
#include "stdio.h"
#include "user.h"

static FILE _stdin = {0};
static FILE _stdout = {1};
static FILE _stderr = {2};
FILE *stdin = &_stdin;
FILE *stdout = &_stdout;
FILE *stderr = &_stderr;

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

// Only understands %d, %x, %p, %s.
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

int
vsprintf(char *out, const char *fmt, va_list ap)
{
  return vsnprintf(out, (uint)-1, fmt, ap);
}

// Print to the given fd. Only understands %d, %x, %p, %s.
int
vfprintf(FILE* fp, const char* fmt, va_list ap)
{
  // TODO: directly output to fd, not use vsnprintf.
  char buf[1024];
  int len;

  len = vsnprintf(buf, sizeof(buf), fmt, ap);
  return write(fp->fd, buf, len);
}

// Print to the stdout. Only understands %d, %x, %p, %s.
int
printf(const char *fmt, ...)
{
  va_list ap;
  int len;

  va_start(ap, fmt);
  len = vfprintf(stdout, fmt, ap);
  va_end(ap);
  return len;
}

// Print to the given fd. Only understands %d, %x, %p, %s.
int
fprintf(FILE* fp, const char *fmt, ...)
{
  va_list ap;
  int len;

  va_start(ap, fmt);
  len = vfprintf(fp, fmt, ap);
  va_end(ap);
  return len;
}
