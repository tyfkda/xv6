#include "stdio.h"
#include "string.h"
#include "unistd.h"

int
vsprintf(char *out, const char *fmt, va_list ap)
{
  return vsnprintf(out, (size_t)-1, fmt, ap);
}

// Print to the given fd. Only understands %d, %x, %p, %s.
int
vfprintf(FILE* fp, const char* fmt, va_list ap)
{
  // TODO: directly output to fd, not use vsnprintf.
  char buf[1024];
  int len;

  len = vsnprintf(buf, sizeof(buf), fmt, ap);
  return write(fileno(fp), buf, len);
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
