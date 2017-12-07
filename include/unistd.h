#pragma once

#include "stddef.h"  // size_t
#include "sys/types.h"  // pid_t

#define STDIN_FILENO   (0)
#define STDOUT_FILENO  (1)
#define STDERR_FILENO  (2)

struct stat;
struct rtcdate;

#ifdef __cplusplus
extern "C" {
#endif

// system calls
pid_t fork(void);
int pipe(int*);
int write(int, const void*, size_t);
int read(int, void*, int);
off_t lseek(int fd, off_t offset, int whence);
int close(int);
int execv(const char *, char *const []);
int execvp(const char *, char *const []);
int execve(const char*, char *const[], char *const []);
int open(const char*, int, ...);
int mknod(const char*, short, short);
int unlink(const char*);
int fstat(int fd, struct stat*);
int link(const char*, const char*);
int chdir(const char*);
int dup(int);
pid_t getpid(void);
void* sbrk(int);
int sleep(int);
int uptime(void);
int date(struct rtcdate *);
int isatty(int fd);

int arp(char*, char*, int);

// ulib.c
int stat(const char*, struct stat*);
char *getcwd(char *buffer, size_t size);

int ftruncate(int fd, unsigned int length);

#ifdef __cplusplus
}  // extern "C"
#endif
