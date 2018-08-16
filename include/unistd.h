#pragma once

#define STDIN_FILENO   (0)
#define STDOUT_FILENO  (1)
#define STDERR_FILENO  (2)

#ifndef NULL
#define NULL  ((void*)0)
#endif

struct stat;
struct rtcdate;

// system calls
int fork(void);
int wait(int*);
int pipe(int*);
int write(int, const void*, int);
int read(int, void*, int);
int close(int);
int kill(int);
int exec(char*, char**);
int execve(char*, char**, char**);
int open(const char*, int, ...);
int mknod(const char*, short, short);
int unlink(const char*);
int fstat(int fd, struct stat*);
int link(const char*, char*);
int mkdir(const char*);
int chdir(const char*);
int dup(int);
int getpid(void);
char* sbrk(int);
int sleep(int);
int uptime(void);
int date(struct rtcdate *);
int ioctl(int fd, int request, ...);
int isatty(int fd);

// ulib.c
int stat(const char*, struct stat*);

int isatty(int fd);

int ftruncate(int fd, unsigned int length);
