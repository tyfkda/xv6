#include "dirent.h"
#include "fcntl.h"
#include "stat.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "time.h"
#include "unistd.h"

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
  if(strlen(p) >= DIRSIZ)
    return (char*)p;
  memmove(buf, p, strlen(p));
  memset(buf+strlen(p), ' ', DIRSIZ-strlen(p));
  return buf;
}

void dumpinfo(const char* name, const struct stat* st) {
  time_t mt = st->mtime;
  struct tm *t = localtime(&mt);
  printf("%s %d %3d %6d  %04d/%02d/%02d %02d:%02d\n",
         name, st->type, st->ino, st->size,
         t->tm_year + 1900, t->tm_mon + 1, t->tm_mday,
         t->tm_hour, t->tm_min);
}

void lsdir(const char *path)
{
  char buf[512], *p;
  struct stat st;

  if(strlen(path) + 1 + DIRSIZ + 1 > sizeof buf){
    fprintf(stderr, "ls: path too long\n");
    return;
  }
  strcpy(buf, path);
  p = buf+strlen(buf);
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

    memmove(p, de->name, DIRSIZ);
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

  switch(st.type){
  case T_FILE:
    dumpinfo(fmtname(path), &st);
    break;

  case T_DIR:
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
