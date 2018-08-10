#pragma once

typedef struct __dirstream DIR;

// Directory is a file containing a sequence of dirent structures.
#define DIRSIZ 14

struct dirent {
  unsigned short d_ino;
  char d_name[DIRSIZ];
};

DIR* opendir(const char *path);
DIR* fdopendir(int fd);
int closedir(DIR*);
struct dirent *readdir(DIR*);
