#include "fcntl.h"
#include "file_def.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "unistd.h"

int fileno(const FILE *fp) {
  return fp->fd;
}

FILE* fopen(const char* fileName, const char* mode) {
  struct {
    const char* str;
    int flag;
  } static const kTable[] = {
    {"r", O_RDONLY},
    {"w", O_WRONLY | O_CREAT | O_TRUNC},
    {"a", O_WRONLY | O_CREAT | O_APPEND},
    {"rb", O_RDONLY},
    {"wb", O_WRONLY | O_CREAT | O_TRUNC},
    {"ab", O_WRONLY | O_CREAT | O_APPEND},
    {"r+", O_RDONLY},
    {"w+", O_WRONLY | O_CREAT},
    {"a+", O_WRONLY | O_CREAT | O_APPEND},
    {"r+b", O_RDONLY},
    {"w+b", O_WRONLY | O_CREAT},
    {"a+b", O_WRONLY | O_CREAT | O_APPEND},
    {"rb+", O_RDONLY},
    {"wb+", O_WRONLY | O_CREAT},
    {"ab+", O_WRONLY | O_CREAT | O_APPEND},
  };

  int flag = -1;
  for (int i = 0; i < sizeof(kTable) / sizeof(*kTable); ++i) {
    if (strcmp(kTable[i].str, mode) == 0) {
      flag = kTable[i].flag;
      break;
    }
  }
  if (flag == -1)
    return 0;

  int fd = open(fileName, flag);
  if (fd < 0) {
    return 0;
  }

  FILE* fp = malloc(sizeof(*fp));
  if (fp == 0) {
    close(fd);
    return 0;
  }

  fp->fd = fd;
  return fp;
}

int fclose(FILE* fp) {
  if (fp == 0 || fp->fd < 0)
    return EOF;
  close(fp->fd);
  fp->fd = -1;
  free(fp);
  return 0;
}

size_t
fwrite(const void* buffer, size_t size, size_t count, FILE* fp)
{
  return write(fp->fd, buffer, size * count);
}

size_t
fread(void* buffer, size_t size, size_t count, FILE* fp)
{
  return read(fp->fd, buffer, size * count);
}

int
fgetc(FILE* fp)
{
  unsigned char c;
  int len = read(fp->fd, &c, 1);
  return len == 1 ? c : EOF;
}

int
getc(FILE* fp)
{
  return fgetc(fp);
}

int
getchar(void)
{
  return fgetc(stdin);
}

char*
fgets(char *buf, size_t max, FILE* fp)
{
  int eof = 1;
  size_t i;
  for (i = 0; i < max - 1; ){
    int c = fgetc(fp);
    if (c < 0)
      break;
    eof = 0;
    buf[i++] = c;
    if (c == '\n' || c == '\r')
      break;
  }
  buf[i] = '\0';
  return eof ? 0 : buf;
}

char*
gets_s(char *buf, size_t max)
{
  char* result = fgets(buf, max, stdin);
  if (result == 0)
    return 0;

  // Contrast to fgets, gets chomps tail '\n'
  int len = strlen(buf);
  if (len > 0 && (buf[len - 1] == '\n' || buf[len - 1] == '\r'))
    buf[len - 1] = '\0';
  return result;
}

int putchar(int c) {
  unsigned char buf = c;
  return fwrite(&buf, 1, 1, stdout);
}

ssize_t getline(char **pline, size_t *pcap, FILE *fp) {
  const size_t MINSIZE = 16;
  char* line = *pline;
  size_t cap = *pcap;

  ssize_t i = 0;
  for (;;) {
    int c = fgetc(fp);
    if (c == EOF)
      break;
    if (i + 1 >= cap) {  // +1 for '\0'
      size_t newcap = i + 2 < MINSIZE ? MINSIZE : (i + 1) * 2;
      char* newline = realloc(line, newcap);
      if (newline == 0) {
        i = -1;
        break;
      }
      line = newline;
      cap = newcap;
    }
    line[i++] = c;
    if (c == '\n')
      break;
  }

  if (i == 0) {  // EOF
    i = -1;
  } else {
  if (line != NULL)
    line[i] = '\0';
  }

  *pline = line;
  *pcap = cap;

  return i;
}

int sscanf(const char * restrict s_, const char * restrict format_, ...) {
  const unsigned char* s = (const unsigned char*)s_;
  const unsigned char* format = (const unsigned char*)format_;

  va_list ap;
  va_start(ap, format_);

  int count = 0;
  for (;;) {
    unsigned char c = *format++;
    if (c == '\0')
      break;
    if (c != '%') {
      if (*s++ != c)
        return count;
    } else {
      if (*format == '\0')
        break;
      switch (*format++) {
      case 'd':
        {
          long val = strtol((const char*)s, (char**)&s, 10);
          *va_arg(ap, int*) = (int)val;
          ++count;
        }
        break;
      default:
        // TODO: Fix
        break;
      }
    }
  }

  va_end(ap);
  return count;
}
