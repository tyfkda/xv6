#include "hostfsaux.h"
#include <dirent.h>
#include <fcntl.h>
#include <sys/stat.h>

int host_readopen(const char *path) {
  return open(path, O_RDONLY);
}

int host_readwriteopen(const char *path) {
  return open(path, O_RDWR);
}

int host_createopen(const char *path) {
  return open(path, O_RDWR|O_CREAT, 0666);
}

size_t host_read(int fd, void *buf, size_t size) {
  return read(fd, buf, size);
}

size_t host_write(int fd, const void *buf, size_t size) {
  return write(fd, buf, size);
}

off_t host_lseek(int fd, off_t offset, int whence) {
  return lseek(fd, offset, whence);
}

int host_close(int fd) {
  return close(fd);
}

int host_isdir(const char *path) {
  struct stat st;
  return stat(path, &st) == 0 &&
    (st.st_mode & S_IFMT) == S_IFDIR;
}

HOSTDIR *host_opendir(const char *path) {
  DIR *dir = opendir(path);
  return (HOSTDIR*)dir;
}

const char *host_readdir(HOSTDIR *dir) {
  struct dirent* dent = readdir((DIR*)dir);
  return dent != NULL ? dent->d_name : NULL;
}

void host_closedir(HOSTDIR *dir) {
  closedir((DIR*)dir);
}
