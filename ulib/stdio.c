#include "fcntl.h"
#include "file_def.h"
#include "stdio.h"
#include "user.h"

FILE* fopen(const char* fileName, const char* flag) {
  int o;
  if (flag[0] == 'w') {
    o = O_WRONLY | O_CREATE;
  } else {
    o = O_RDONLY;
  }

  int fd = open(fileName, o);
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
