#include "fcntl.h"
#include "sys/stat.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "unistd.h"

int isDirectory(const char* fn) {
  struct stat st;
  if (stat(fn, &st) < 0) {
    fprintf(stderr, "stat failed [%s]\n", fn);
    exit(1);
  }
  return S_ISDIR(st.st_mode);
}

const char* getBasename(const char* fn) {
  const char* slash = strrchr(fn, '/');
  return slash != NULL ? slash + 1 : fn;
}

void cp(const char* srcFn, const char* dstFn) {
  int srcFd = open(srcFn, O_RDONLY);
  if (srcFd < 0) {
    fprintf(stderr, "cp: file not found [%s]\n", srcFn);
    exit(1);
  }

  int dstFd = open(dstFn, O_WRONLY | O_TRUNC | O_CREAT);
  if (dstFd < 0) {
    fprintf(stderr, "cp: open failed [%s]\n", dstFn);
    exit(1);
  }

  for (;;) {
    char buf[512];
    int n = read(srcFd, buf, sizeof(buf));
    if (n < 0) {
      fprintf(stderr, "cp: read error\n");
      exit(1);
    }
    if (n == 0)
      break;

    if (write(dstFd, buf, n) != n) {
      fprintf(stderr, "cp: write error\n");
      exit(1);
    }
  }

  close(dstFd);
  close(srcFd);
}

int main(int argc, char *argv[]) {
  if (argc < 3) {
    fprintf(stderr, "usage: src ... dest\n");
    return 1;
  }

  const char* dstFn = argv[argc - 1];
  int isDir = isDirectory(dstFn);
  for (int i = 1; i < argc - 1; ++i) {
    char buf[256];
    const char* srcFn = argv[i];
    if (isDir) {
      const char* basename = getBasename(srcFn);
      snprintf(buf, sizeof(buf), "%s/%s", dstFn, basename);
      dstFn = buf;
    }
    cp(srcFn, dstFn);
  }

  return 0;
}
