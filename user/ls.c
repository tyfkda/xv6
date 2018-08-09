#include "dirent.h"
#include "fcntl.h"
#include "sys/stat.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "time.h"
#include "unistd.h"

#ifndef DIRSIZ
#define DIRSIZ  (16)
#endif

char*
fmtname(const char *path)
{
  static char buf[DIRSIZ+1];
  const char *p;

  // Find first character after last slash.
  for(p=path+strlen(path); p >= path && *p != '/'; p--)
    ;
  p++;

  // Return blank-padded name.
  size_t len = strlen(p);
  if(len >= DIRSIZ)
    return (char*)p;
  memmove(buf, p, len);
  memset(buf+len, ' ', DIRSIZ-len);
  return buf;
}

void dumpinfo(const char* name, const struct stat* st) {
  time_t mt = st->st_mtim.tv_sec;
  struct tm *t = localtime(&mt);
  printf("%s %4x %5d %8d  %04d/%02d/%02d %02d:%02d\n",
         name, st->st_mode, (int)st->st_ino, (int)st->st_size,
         t->tm_year + 1900, t->tm_mon + 1, t->tm_mday,
         t->tm_hour, t->tm_min);
}

void lsdir(const char *path)
{
  char buf[512], *p;
  struct stat st;
  size_t pathlen;

  pathlen = strlen(path);
  if(pathlen + 1 + DIRSIZ + 1 > sizeof buf){
    fprintf(stderr, "ls: path too long\n");
    return;
  }
  strcpy(buf, path);
  p = buf + pathlen;
  *p++ = '/';

  DIR *dir = opendir(path);
  if (dir == NULL) {
    fprintf(stderr, "opendir failed: %s\n", path);
    exit(1);
  }

  for (;;) {
    struct dirent* de = readdir(dir);
    if (de == NULL)
      break;

    strncpy(p, de->d_name, DIRSIZ);
    p[DIRSIZ] = 0;
    if(stat(buf, &st) < 0){
      fprintf(stderr, "ls: cannot stat %s\n", buf);
      continue;
    }
    dumpinfo(fmtname(buf), &st);
  }

  closedir(dir);
}

int
ls(const char *path)
{
  struct stat st;

  if(stat(path, &st) < 0){
    fprintf(stderr, "ls: cannot stat %s\n", path);
    return 1;
  }

  switch(st.st_mode & S_IFMT){
  case S_IFREG:
    dumpinfo(fmtname(path), &st);
    break;

  case S_IFDIR:
    lsdir(path);
    break;
  }
  return 0;
}

int
main(int argc, char *argv[])
{
  int i;
  int ret;

  if(argc < 2){
    return ls(".");
  }

  ret = 0;
  for(i=1; i<argc; i++) {
    ret = ls(argv[i]);
    if (ret != 0)
      break;
  }
  return ret;
}
