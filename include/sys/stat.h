#pragma once

#include "time.h"  // for struct timespec
#include "_ttype.h"

typedef unsigned int ino_t;

struct stat {
  short st_mode;          // Type of file
  int st_dev;             // File system's disk device
  ino_t st_ino;           // Inode number
  short st_nlink;         // Number of links to file
  unsigned int st_size;   // Size of file in bytes
  struct timespec st_mtim;   // Modified time
};

int mkdir(const char*, int);

// Mock POSIX
#define S_IFMT   (3)
#define S_IFDIR  (T_DIR)
#define S_IFREG  (T_FILE)
#define S_IFCHR  (T_DEV)

#define S_ISDIR(m)  (((m) & S_IFMT) == S_IFDIR)
#define S_ISREG(m)  (((m) & S_IFMT) == S_IFREG)
#define S_ISCHR(m)  (((m) & S_IFMT) == S_IFCHR)
