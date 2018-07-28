#pragma once

// Directory is a file containing a sequence of dirent structures.
#define DIRSIZ 14

struct dirent {
  unsigned short inum;
  char name[DIRSIZ];
};
