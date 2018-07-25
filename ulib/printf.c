#include "file_def.h"
#include "stdio.h"
#include "user.h"

static FILE _stdin = {0};
static FILE _stdout = {1};
static FILE _stderr = {2};
FILE *stdin = &_stdin;
FILE *stdout = &_stdout;
FILE *stderr = &_stderr;

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
