#include "fcntl.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "sys/stat.h"
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

void ln(const char* srcFn, const char* dstFn) {
  if (link(srcFn, dstFn) < 0) {
    fprintf(stderr, "%s <- %s failed to link\n", srcFn, dstFn);
    exit(1);
  }
}

void rm(const char* fn) {
  if (unlink(fn) < 0) {
    fprintf(stderr, "%s failed to delete\n", fn);
    exit(1);
  }
}

void mv(const char* srcFn, const char* dstFn) {
  ln(srcFn, dstFn);
  rm(srcFn);
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
    mv(srcFn, dstFn);
  }

  return 0;
}
