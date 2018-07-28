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

  int flag = 0;
  for (int i = 0; i < sizeof(kTable) / sizeof(*kTable); ++i) {
    if (strcmp(kTable[i].str, mode) == 0) {
      flag = kTable[i].flag;
      break;
    }
  }
  if (flag == 0)
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

uint
fwrite(const void* buffer, uint size, uint count, FILE* fp)
{
  return write(fp->fd, buffer, size * count);
}

uint
fread(void* buffer, uint size, uint count, FILE* fp)
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
getc(void)
{
  return fgetc(stdin);
}

int
getchar(void)
{
  return fgetc(stdin);
}

char*
fgets_s(char *buf, uint max, FILE* fp)
{
  int eof = 1;
  int i;
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
gets_s(char *buf, uint max)
{
  char* result = fgets_s(buf, max, stdin);
  if (result == 0)
    return 0;

  // Contrast to fgets, gets chomps tail '\n'
  int len = strlen(buf);
  if (len > 0 && (buf[len - 1] == '\n' || buf[len - 1] == '\r'))
    buf[len - 1] = '\0';
  return result;
}

int putchar(int c) {
  uchar buf = c;
  return fwrite(&buf, 1, 1, stdout);
}
