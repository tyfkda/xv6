// init: The initial user-level program

#include "types.h"
#include "stdio.h"
#include "user.h"
#include "fcntl.h"

char *argv[] = { "sh", 0 };

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
    printf("init: starting sh\n");
    pid = fork();
    if(pid < 0){
      fprintf(stderr, "init: fork failed\n");
      exit(1);
    }
    if(pid == 0){
      exec("sh", argv);
      fprintf(stderr, "init: exec sh failed\n");
      exit(1);
    }
    while((wpid=wait(0)) >= 0 && wpid != pid)
      fprintf(stderr, "zombie!\n");
  }
}

int
main(void)
{
  static const char* devnames[] = {
    "console",
    "uart",
  };

  for (int i = 0; i < 2; ++i) {
    int pid = fork();
    if (pid < 0) {
      fprintf(stderr, "init: fork failed\n");
      exit(1);
    }
    if (pid == 0)
      runsh(i + 1, devnames[i]);
  }
  for (int i = 0; i < 2; ++i) {
    wait(0);
  }
  return 0;
}
