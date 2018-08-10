#pragma once

#define T_DIR  1   // Directory
#define T_FILE 2   // File
#define T_DEV  3   // Device

struct stat {
  short type;             // Type of file
  int st_dev;             // File system's disk device
  unsigned int st_ino;    // Inode number
  short st_nlink;         // Number of links to file
  unsigned int st_size;   // Size of file in bytes
  unsigned int st_mtim;   // Modified time
};
