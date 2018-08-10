#include "dirent.h"
#include "fcntl.h"
#include "sys/stat.h"
#include "stdlib.h"
#include "unistd.h"

extern int _sysreaddir(int, void*);

struct __dirstream {
  int fd;

  struct dirent dbuf;
};

DIR *opendir(const char *path) {
  int fd = open(path, O_RDONLY);
  if (fd < 0)
    return NULL;
  return fdopendir(fd);
}

DIR *fdopendir(int fd) {
  struct stat st;
  if (fstat(fd, &st) < 0) {
    return NULL;
  }
  if (!S_ISDIR(st.st_mode)) {
    return NULL;
  }

  int fd2 = dup(fd);
  if (fd2 < 0) {
    return NULL;
  }
  close(fd);

  DIR *dir = malloc(sizeof(*dir));
  if (dir == NULL) {
    close(fd2);
    return NULL;
  }
  dir->fd = fd2;
  return dir;
}

int closedir(DIR *dir) {
  if (dir != NULL) {
    close(dir->fd);
    free(dir);
  }
  return 0;
}

struct dirent *readdir(DIR *dir) {
  for (;;) {
    int size = _sysreaddir(dir->fd, &dir->dbuf);
    if (size != sizeof(dir->dbuf))
      break;
    if (dir->dbuf.d_ino != 0)
      return &dir->dbuf;
  }
  return NULL;
}
