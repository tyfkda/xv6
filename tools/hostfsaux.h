#pragma once

#include <stddef.h>
#include <unistd.h>

typedef void *HOSTDIR;

int host_readopen(const char *path);
int host_readwriteopen(const char *path);
int host_createopen(const char *path);
size_t host_read(int fd, void *buf, size_t size);
size_t host_write(int fd, const void *buf, size_t size);
off_t host_lseek(int fd, off_t offset, int whence);
int host_close(int fd);

int host_isdir(const char *path);

HOSTDIR *host_opendir(const char *path);
const char *host_readdir(HOSTDIR *dir);
void host_closedir(HOSTDIR *dir);
