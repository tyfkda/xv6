#pragma once

typedef struct __dirstream DIR;

struct dirent {
  unsigned short d_ino;
  char d_name[16];
};

#ifdef __cplusplus
extern "C" {
#endif

DIR* opendir(const char *path);
DIR* fdopendir(int fd);
int closedir(DIR*);
struct dirent *readdir(DIR*);

#ifdef __cplusplus
}  // extern "C"
#endif
