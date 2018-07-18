#include "fs.h"
#include "stat.h"
#include "stdio.h"
#include "time.h"
#include "user.h"

char*
fmtname(char *path)
{
  static char buf[DIRSIZ+1];
  char *p;

  // Find first character after last slash.
  for(p=path+strlen(path); p >= path && *p != '/'; p--)
    ;
  p++;

  // Return blank-padded name.
  if(strlen(p) >= DIRSIZ)
    return p;
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

int
ls(char *path)
{
  char buf[512], *p;
  int fd;
  struct dirent de;
  struct stat st;

  if((fd = open(path, 0)) < 0){
    fprintf(stderr, "ls: cannot open %s\n", path);
    return 1;
  }

  if(fstat(fd, &st) < 0){
    fprintf(stderr, "ls: cannot stat %s\n", path);
    close(fd);
    return 1;
  }

  switch(st.type){
  case T_FILE:
    dumpinfo(fmtname(path), &st);
    break;

  case T_DIR:
    if(strlen(path) + 1 + DIRSIZ + 1 > sizeof buf){
      fprintf(stderr, "ls: path too long\n");
      break;
    }
    strcpy(buf, path);
    p = buf+strlen(buf);
    *p++ = '/';
    while(read(fd, &de, sizeof(de)) == sizeof(de)){
      if(de.inum == 0)
        continue;
      memmove(p, de.name, DIRSIZ);
      p[DIRSIZ] = 0;
      if(stat(buf, &st) < 0){
        fprintf(stderr, "ls: cannot stat %s\n", buf);
        continue;
      }
      dumpinfo(fmtname(buf), &st);
    }
    break;
  }
  close(fd);
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
