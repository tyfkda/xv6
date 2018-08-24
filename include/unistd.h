#pragma once

#define STDIN_FILENO   (0)
#define STDOUT_FILENO  (1)
#define STDERR_FILENO  (2)

enum {
  SEEK_SET,  // 0
  SEEK_CUR,  // 1
  SEEK_END,  // 2
};

struct stat;
struct rtcdate;

typedef long off_t;

// system calls
int fork(void);
int pipe(int*);
int write(int, const void*, int);
int read(int, void*, int);
off_t lseek(int fd, off_t offset, int whence);
int close(int);
int exec(const char *, char *const []);
int execve(const char*, char *const[], char *const []);
int open(const char*, int, ...);
int mknod(const char*, short, short);
int unlink(const char*);
int fstat(int fd, struct stat*);
int link(const char*, char*);
int chdir(const char*);
int dup(int);
int getpid(void);
void* sbrk(int);
int sleep(int);
int uptime(void);
int date(struct rtcdate *);
int isatty(int fd);

// ulib.c
int stat(const char*, struct stat*);

int ftruncate(int fd, unsigned int length);
