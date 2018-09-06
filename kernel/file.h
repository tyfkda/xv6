#pragma once

#include "fs.h"  // NDIRECT
#include "types.h"


// in-memory copy of an inode
struct inode {
  uint dev;           // Device number
  uint inum;          // Inode number
  int ref;            // Reference count
  int valid;          // inode has been read from disk?

  short type;         // copy of disk inode
  short major;
  short minor;
  short nlink;
  uint mtime;         // Modified time
  uint size;
  uint addrs[NDIRECT+1];
};
