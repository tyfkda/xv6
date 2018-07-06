#include "stat.h"
#include "stdio.h"
#include "user.h"
#include "fs.h"

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
    fprintf(stderr, "%s %d %d %d\n", fmtname(path), st.type, st.ino, st.size);
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
      printf("%s %d %d %d\n", fmtname(buf), st.type, st.ino, st.size);
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
