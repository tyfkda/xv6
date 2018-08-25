// init: The initial user-level program

#include "fcntl.h"
#include "stddef.h"  // for NULL
#include "sys/wait.h"
#include "unistd.h"

#define FDPUTS(fd, str)  write(fd, (str), sizeof(str) - 1)
#define exit     _sysexit

extern void _sysexit(int);

char *argv[] = { "/bin/sh", 0 };
char *environ[] = { "PATH=/bin", 0 };

void runsh(int dev, const char* devname) __attribute__((noreturn));
void
runsh(int dev, const char* devname)
{
  int pid, wpid;

  if(open(devname, O_RDWR) < 0){
    mknod(devname, dev, 1);
    open(devname, O_RDWR);
  }
  dup(0);  // stdout
  dup(0);  // stderr

  for(;;){
    FDPUTS(STDOUT_FILENO, "init: starting sh\n");
    pid = fork();
    if(pid < 0){
      FDPUTS(STDERR_FILENO, "init: fork failed\n");
      exit(1);
    }
    if(pid == 0){
      execve(argv[0], argv, environ);
      FDPUTS(STDERR_FILENO, "init: exec sh failed\n");
      exit(1);
    }
    while((wpid=wait(NULL)) >= 0 && wpid != pid)
      FDPUTS(STDERR_FILENO, "zombie!\n");
  }
}

static void mountProc(void) {
  int fd = open("proc", O_RDONLY);
  if (fd < 0) {
    mknod("proc", 3, 1);
  } else {
    close(fd);
  }
}

void
_start(int argc, char* argv[])
{
  mountProc();

  static const char* devnames[] = {
    "console",
    "uart",
  };

  for (int i = 0; i < 2; ++i) {
    int pid = fork();
    if (pid < 0) {
      FDPUTS(STDERR_FILENO, "init: fork failed\n");
      exit(1);
    }
    if (pid == 0)
      runsh(i + 1, devnames[i]);
  }
  for (int i = 0; i < 2; ++i) {
    wait(NULL);
  }
  exit(0);
}
