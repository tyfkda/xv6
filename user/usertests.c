#include "dirent.h"
#include "fcntl.h"
#include "signal.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "sys/stat.h"
#include "sys/wait.h"
#include "unistd.h"
#include "../kernel/fs.h"
#include "../kernel/memlayout.h"
#include "../kernel/param.h"
#include "../kernel/traps.h"

#ifndef X64
#include "../kernel/syscall.h"
#endif

char buf[8192];

static void panic(const char *msg) {
  fprintf(stderr, "\x1b[31mPANIC! %s\x1b[0m\n", msg);
  exit(1);
}

// does chdir() call iput(p->cwd) in a transaction?
void
iputtest(void)
{
  printf("iput test\n");

  if(mkdir("iputdir", 0777) < 0){
    printf("mkdir failed\n");
    exit(1);
  }
  if(chdir("iputdir") < 0){
    printf("chdir iputdir failed\n");
    exit(1);
  }
  if(unlink("../iputdir") < 0){
    printf("unlink ../iputdir failed\n");
    exit(1);
  }
  if(chdir("/") < 0){
    printf("chdir / failed\n");
    exit(1);
  }
  printf("iput test ok\n");
}

// does exit() call iput(p->cwd) in a transaction?
void
exitiputtest(void)
{
  int pid;

  printf("exitiput test\n");

  pid = fork();
  if(pid < 0){
    printf("fork failed\n");
    exit(1);
  }
  if(pid == 0){
    if(mkdir("iputdir", 0777) < 0){
      printf("mkdir failed\n");
      exit(1);
    }
    if(chdir("iputdir") < 0){
      printf("child chdir failed\n");
      exit(1);
    }
    if(unlink("../iputdir") < 0){
      printf("unlink ../iputdir failed\n");
      exit(1);
    }
    exit(0);
  }
  wait(0);
  printf("exitiput test ok\n");
}

// does the error path in open() for attempt to write a
// directory call iput() in a transaction?
// needs a hacked kernel that pauses just after the namei()
// call in sys_open():
//    if((ip = namei(path)) == 0)
//      return -1;
//    {
//      int i;
//      for(i = 0; i < 10000; i++)
//        yield();
//    }
void
openiputtest(void)
{
  int pid;

  printf("openiput test\n");
  if(mkdir("oidir", 0777) < 0){
    printf("mkdir oidir failed\n");
    exit(1);
  }
  pid = fork();
  if(pid < 0){
    printf("fork failed\n");
    exit(1);
  }
  if(pid == 0){
    int fd = open("oidir", O_RDWR);
    if(fd >= 0){
      printf("open directory for write succeeded\n");
      exit(0);
    }
    exit(0);  // To handle error in parent, exits with success.
  }
  sleep(1);
  if(unlink("oidir") != 0){
    printf("unlink failed\n");
    exit(1);
  }
  wait(0);
  printf("openiput test ok\n");
}

// simple file system tests

void
opentest(void)
{
  int fd;

  printf("open test\n");
  fd = open("/bin/echo", 0);
  if(fd < 0){
    printf("open echo failed!\n");
    exit(1);
  }
  close(fd);
  fd = open("doesnotexist", 0);
  if(fd >= 0){
    printf("open doesnotexist succeeded!\n");
    exit(1);
  }
  printf("open test ok\n");
}

void
writetest(void)
{
  int fd;
  int i;

  printf("small file test\n");
  fd = open("small", O_CREAT|O_RDWR);
  if(fd >= 0){
    printf("creat small succeeded; ok\n");
  } else {
    printf("error: creat small failed!\n");
    exit(1);
  }
  for(i = 0; i < 100; i++){
    if(write(fd, "aaaaaaaaaa", 10) != 10){
      printf("error: write aa %d new file failed\n", i);
      exit(1);
    }
    if(write(fd, "bbbbbbbbbb", 10) != 10){
      printf("error: write bb %d new file failed\n", i);
      exit(1);
    }
  }
  printf("writes ok\n");
  close(fd);
  fd = open("small", O_RDONLY);
  if(fd >= 0){
    printf("open small succeeded ok\n");
  } else {
    printf("error: open small failed!\n");
    exit(1);
  }
  i = read(fd, buf, 2000);
  if(i == 2000){
    printf("read succeeded ok\n");
  } else {
    printf("read failed\n");
    exit(1);
  }
  close(fd);

  if(unlink("small") < 0){
    printf("unlink small failed\n");
    exit(1);
  }
  printf("small file test ok\n");
}

void
writetest1(void)
{
  int i, fd, n;

  printf("big files test\n");

  fd = open("big", O_CREAT|O_RDWR);
  if(fd < 0){
    printf("error: creat big failed!\n");
    exit(1);
  }

  for(i = 0; i < MAXFILE; i++){
    ((int*)buf)[0] = i;
    if(write(fd, buf, 512) != 512){
      printf("error: write big file failed: %d\n", i);
      exit(1);
    }
  }

  close(fd);

  fd = open("big", O_RDONLY);
  if(fd < 0){
    printf("error: open big failed!\n");
    exit(1);
  }

  n = 0;
  for(;;){
    i = read(fd, buf, 512);
    if(i == 0){
      if(n == MAXFILE - 1){
        printf("read only %d blocks from big", n);
        exit(1);
      }
      break;
    } else if(i != 512){
      printf("read failed %d\n", i);
      exit(1);
    }
    if(((int*)buf)[0] != n){
      printf("read content of block %d is %d\n",
             n, ((int*)buf)[0]);
      exit(1);
    }
    n++;
  }
  close(fd);
  if(unlink("big") < 0){
    printf("unlink big failed\n");
    exit(1);
  }
  printf("big files ok\n");
}

void
createtest(void)
{
  int i, fd;
  char name[3];

  printf("many creates, followed by unlink test\n");

  name[0] = 'a';
  name[2] = '\0';
  for(i = 0; i < 52; i++){
    name[1] = '0' + i;
    fd = open(name, O_CREAT|O_RDWR);
    close(fd);
  }
  name[0] = 'a';
  name[2] = '\0';
  for(i = 0; i < 52; i++){
    name[1] = '0' + i;
    unlink(name);
  }
  printf("many creates, followed by unlink; ok\n");
}

void lseektest()
{
  const char fileName[] = "lseektest";
  int fd = open(fileName, O_WRONLY | O_CREAT | O_TRUNC);
  if (fd < 0) {
    fprintf(stderr, "lseektest test: Create open failed\n");
    exit(1);
  }

  char buf[256];
  for (int i = 0; i < 256; ++i) {
    buf[i] = i;
  }
  if (write(fd, buf, 256) != 256) {
    fprintf(stderr, "lseektest test: Write failed\n");
    exit(1);
  }
  if (close(fd) < 0) {
    fprintf(stderr, "lseektest test: Close failed\n");
    exit(1);
  }

  fd = open(fileName, O_RDONLY);
  if (fd < 0) {
    fprintf(stderr, "lseektest test: Read open failed\n");
    exit(1);
  }

  // lseek from set
  off_t pos = lseek(fd, 10, SEEK_SET);
  if (pos != 10) {
    fprintf(stderr, "lseektest test: lseek not 10, %d\n", (int)pos);
    exit(1);
  }
  // lseek for cur
  pos = lseek(fd, 100, SEEK_CUR);
  if (pos != 110) {
    fprintf(stderr, "lseektest test: lseek not 110, %d\n", (int)pos);
    exit(1);
  }
  // lseek for end
  pos = lseek(fd, -100, SEEK_END);
  if (pos != 256 - 100) {
    fprintf(stderr, "lseektest test: lseek not 156, %d\n", (int)pos);
    exit(1);
  }
  if (read(fd, buf, 1) != 1 && buf[0] != 156) {
    fprintf(stderr, "lseektest test: Read failed\n");
    exit(1);
  }

  close(fd);
}

void truncatetest()
{
  const static char fileName[] = "trunc.txt";
  int fd, sz;

  unlink(fileName);

  // Prepare a file.
  fd = open(fileName, O_WRONLY | O_CREAT);
  if (fd < 0) {
    fprintf(stderr, "truncate test: Cannot open file\n");
    exit(1);
  }
  sz = write(fd, "1234567890", 10);
  if (sz != 10) {
    fprintf(stderr, "truncate test: Write size different: %d != 10\n", sz);
    exit(1);
  }
  close(fd);

  // Truncate it.
  fd = open(fileName, O_WRONLY | O_TRUNC);
  if (fd < 0) {
    fprintf(stderr, "truncate test: Cannot open file\n");
    exit(1);
  }
  sz = write(fd, "abcde", 5);
  if (sz != 5) {
    fprintf(stderr, "truncate test: Write size different: %d != 5\n", sz);
    exit(1);
  }
  close(fd);

  struct stat st;
  stat(fileName, &st);
  if (st.st_size != 5) {
    fprintf(stderr, "truncate test: fileSize is not truncated: %d\n", st.st_size);
    exit(1);
  }

  unlink(fileName);

  printf("truncate test: OK\n");
}

void appendtest()
{
  const static char fileName[] = "append.txt";
  int fd, sz;

  unlink(fileName);

  // Prepare a file.
  fd = open(fileName, O_WRONLY | O_CREAT);
  if (fd < 0) {
    fprintf(stderr, "append test: Cannot open file\n");
    exit(1);
  }
  sz = write(fd, "12345", 5);
  if (sz != 5) {
    fprintf(stderr, "append test: Write size different: %d != 10\n", sz);
    exit(1);
  }
  close(fd);

  // Truncate it.
  fd = open(fileName, O_WRONLY | O_APPEND);
  if (fd < 0) {
    fprintf(stderr, "append test: Cannot open file\n");
    exit(1);
  }
  sz = write(fd, "abcde", 5);
  if (sz != 5) {
    fprintf(stderr, "append test: Write size different: %d != 5\n", sz);
    exit(1);
  }
  close(fd);

  struct stat st;
  stat(fileName, &st);
  if (st.st_size != 10) {
    fprintf(stderr, "append test: fileSize is not truncated: %d\n", st.st_size);
    exit(1);
  }

  unlink(fileName);

  printf("append test: OK\n");
}

void dirtest(void)
{
  printf("mkdir test\n");

  if(mkdir("dir0", 0777) < 0){
    printf("mkdir failed\n");
    exit(1);
  }

  if(chdir("dir0") < 0){
    printf("chdir dir0 failed\n");
    exit(1);
  }

  if(chdir("..") < 0){
    printf("chdir .. failed\n");
    exit(1);
  }

  if(unlink("dir0") < 0){
    printf("unlink dir0 failed\n");
    exit(1);
  }
  printf("mkdir test ok\n");
}

void
exectest(void)
{
  char *argv[] = {"echo", "\x1b[32mALL", "TESTS", "PASSED\x1b[0m", 0};

  printf("exec test\n");
  if (fork() == 0) {
    if(execvp(argv[0], argv) < 0){
      exit(1);
    }
  }
  int code;
  wait(&code);
  if (code != 0) {
    printf("exec echo failed\n");
    exit(code);
  }
}

void execvetest() {
  const char tempfn[] = "exectestresult";
  unlink(tempfn);

  // Run "echo 'echo $PATH $FOO' | sh" and check the result.
  int p[2];
  if (pipe(p) < 0)
    panic("execvetest: pipe");
  if (fork() == 0) {
    // former child.
    close(1);
    dup(p[1]);
    close(p[0]);
    close(p[1]);

    char* argv[] = {"echo", "echo $PATH $FOO", NULL};
    execvp(argv[0], argv);
    exit(1);
  }
  if (fork() == 0) {
    // latter child.
    close(0);
    dup(p[0]);
    close(p[0]);
    close(p[1]);

    // Redirect stdout to a file.
    close(1);
    int fd = open(tempfn, O_WRONLY | O_CREAT | O_TRUNC);
    if (fd < 0)
      panic("execvetest: open");

    char* argv[] = {"/bin/sh", NULL};
    char* envp[] = {"PATH=/usr/local/bin:/bin", "FOO=foobar", NULL};
    execve(argv[0], argv, envp);
    exit(2);
  }

  close(p[0]);
  close(p[1]);
  int ec1, ec2;
  wait(&ec1);
  wait(&ec2);
  if (ec1 != 0 || ec2 != 0)
    panic("execvetest: child process failed");

  // Judge output
  int fd = open(tempfn, O_RDONLY);
  if (fd < 0)
    panic("execvetest: result not written");
  const char expected[] = "/usr/local/bin:/bin foobar\n";
  char buf[128];
  int size = read(fd, buf, sizeof(buf));
  if (size != sizeof(expected) - 1)
    panic("execvetest: size");
  if (strncmp(buf, expected, size) != 0)
    panic("execvetest: output different");
  close(fd);
  unlink(tempfn);

  // Check child environment variable not affected to the parent.
  if (strcmp(getenv("PATH"), "/bin") != 0 || getenv("FOO") != NULL)
    panic("execvetest: env");

  printf("execve test passed\n");
}

// simple fork and pipe read/write

void
pipe1(void)
{
  int fds[2], pid;
  int seq, i, n, cc, total;

  if(pipe(fds) != 0){
    printf("pipe() failed\n");
    exit(1);
  }
  pid = fork();
  seq = 0;
  if(pid == 0){
    close(fds[0]);
    for(n = 0; n < 5; n++){
      for(i = 0; i < 1033; i++)
        buf[i] = seq++;
      if(write(fds[1], buf, 1033) != 1033){
        printf("pipe1 oops 1\n");
        exit(1);
      }
    }
    exit(0);
  } else if(pid > 0){
    close(fds[1]);
    total = 0;
    cc = 1;
    while((n = read(fds[0], buf, cc)) > 0){
      for(i = 0; i < n; i++){
        if((buf[i] & 0xff) != (seq++ & 0xff)){
          printf("pipe1 oops 2\n");
          return;
        }
      }
      total += n;
      cc = cc * 2;
      if(cc > sizeof(buf))
        cc = sizeof(buf);
    }
    if(total != 5 * 1033){
      printf("pipe1 oops 3 total %d\n", total);
      exit(1);
    }
    close(fds[0]);
    wait(0);
  } else {
    printf("fork() failed\n");
    exit(1);
  }
  printf("pipe1 ok\n");
}

// meant to be run w/ at most two CPUs
void
preempt(void)
{
  int pid1, pid2, pid3;
  int pfds[2];

  printf("preempt: ");
  pid1 = fork();
  if(pid1 == 0)
    for(;;)
      ;

  pid2 = fork();
  if(pid2 == 0)
    for(;;)
      ;

  pipe(pfds);
  pid3 = fork();
  if(pid3 == 0){
    close(pfds[0]);
    if(write(pfds[1], "x", 1) != 1)
      printf("preempt write error");
    close(pfds[1]);
    for(;;)
      ;
  }

  close(pfds[1]);
  if(read(pfds[0], buf, sizeof(buf)) != 1){
    printf("preempt read error");
    return;
  }
  close(pfds[0]);
  printf("kill... ");
  kill(pid1, 0);
  kill(pid2, 0);
  kill(pid3, 0);
  printf("wait... ");
  wait(0);
  wait(0);
  wait(0);
  printf("preempt ok\n");
}

// try to find any races between exit and wait
void
exitwait(void)
{
  int i, pid;

  for(i = 0; i < 100; i++){
    pid = fork();
    if(pid < 0){
      printf("fork failed\n");
      return;
    }
    if(pid){
      if(wait(0) != pid){
        printf("wait wrong pid\n");
        return;
      }
    } else {
      exit(0);
    }
  }
  printf("exitwait ok\n");
}

void
mem(void)
{
  void *m1, *m2;
  int pid, ppid;

  printf("mem test\n");
  ppid = getpid();
  if((pid = fork()) == 0){
    m1 = 0;
    while((m2 = malloc(10001)) != 0){
      *(char**)m2 = m1;
      m1 = m2;
    }
    while(m1){
      m2 = *(char**)m1;
      free(m1);
      m1 = m2;
    }
    m1 = malloc(1024*20);
    if(m1 == 0){
      printf("couldn't allocate mem?!!\n");
      kill(ppid, 0);
      exit(1);
    }
    free(m1);
    printf("mem ok\n");
    exit(0);
  } else {
    wait(0);
  }
}

// More file system tests

// two processes write to the same file descriptor
// is the offset shared? does inode locking work?
void
sharedfd(void)
{
  int fd, pid, i, n, nc, np;
  char buf[10];

  printf("sharedfd test\n");

  unlink("sharedfd");
  fd = open("sharedfd", O_CREAT|O_RDWR);
  if(fd < 0){
    printf("fstests: cannot open sharedfd for writing");
    return;
  }
  pid = fork();
  memset(buf, pid==0?'c':'p', sizeof(buf));
  for(i = 0; i < 1000; i++){
    if(write(fd, buf, sizeof(buf)) != sizeof(buf)){
      printf("fstests: write sharedfd failed\n");
      break;
    }
  }
  if(pid == 0)
    exit(0);
  else
    wait(0);
  close(fd);
  fd = open("sharedfd", 0);
  if(fd < 0){
    printf("fstests: cannot open sharedfd for reading\n");
    return;
  }
  nc = np = 0;
  while((n = read(fd, buf, sizeof(buf))) > 0){
    for(i = 0; i < sizeof(buf); i++){
      if(buf[i] == 'c')
        nc++;
      if(buf[i] == 'p')
        np++;
    }
  }
  close(fd);
  unlink("sharedfd");
  if(nc == 10000 && np == 10000){
    printf("sharedfd ok\n");
  } else {
    printf("sharedfd oops %d %d\n", nc, np);
    exit(1);
  }
}

// four processes write different files at the same
// time, to test block allocation.
void
fourfiles(void)
{
  int fd, pid, i, j, n, total, pi;
  char *names[] = { "f0", "f1", "f2", "f3" };
  char *fname;

  printf("fourfiles test\n");

  for(pi = 0; pi < 4; pi++){
    fname = names[pi];
    unlink(fname);

    pid = fork();
    if(pid < 0){
      printf("fork failed\n");
      exit(1);
    }

    if(pid == 0){
      fd = open(fname, O_CREAT | O_RDWR);
      if(fd < 0){
        printf("create failed\n");
        exit(1);
      }

      memset(buf, '0'+pi, 512);
      for(i = 0; i < 12; i++){
        if((n = write(fd, buf, 500)) != 500){
          printf("write failed %d\n", n);
          exit(1);
        }
      }
      exit(0);
    }
  }

  for(pi = 0; pi < 4; pi++){
    wait(0);
  }

  for(i = 0; i < 2; i++){
    fname = names[i];
    fd = open(fname, 0);
    total = 0;
    while((n = read(fd, buf, sizeof(buf))) > 0){
      for(j = 0; j < n; j++){
        if(buf[j] != '0'+i){
          printf("wrong char\n");
          exit(1);
        }
      }
      total += n;
    }
    close(fd);
    if(total != 12*500){
      printf("wrong length %d\n", total);
      exit(1);
    }
    unlink(fname);
  }

  printf("fourfiles ok\n");
}

// four processes create and delete different files in same directory
void
createdelete(void)
{
  enum { N = 20 };
  int pid, i, fd, pi;
  char name[32];

  printf("createdelete test\n");

  for(pi = 0; pi < 4; pi++){
    pid = fork();
    if(pid < 0){
      printf("fork failed\n");
      exit(1);
    }

    if(pid == 0){
      name[0] = 'p' + pi;
      name[2] = '\0';
      for(i = 0; i < N; i++){
        name[1] = '0' + i;
        fd = open(name, O_CREAT | O_RDWR);
        if(fd < 0){
          printf("create failed\n");
          exit(1);
        }
        close(fd);
        if(i > 0 && (i % 2 ) == 0){
          name[1] = '0' + (i / 2);
          if(unlink(name) < 0){
            printf("unlink failed\n");
            exit(1);
          }
        }
      }
      exit(0);
    }
  }

  for(pi = 0; pi < 4; pi++){
    wait(0);
  }

  name[0] = name[1] = name[2] = 0;
  for(i = 0; i < N; i++){
    for(pi = 0; pi < 4; pi++){
      name[0] = 'p' + pi;
      name[1] = '0' + i;
      fd = open(name, 0);
      if((i == 0 || i >= N/2) && fd < 0){
        printf("oops createdelete %s didn't exist\n", name);
        exit(1);
      } else if((i >= 1 && i < N/2) && fd >= 0){
        printf("oops createdelete %s did exist\n", name);
        exit(1);
      }
      if(fd >= 0)
        close(fd);
    }
  }

  for(i = 0; i < N; i++){
    for(pi = 0; pi < 4; pi++){
      name[0] = 'p' + i;
      name[1] = '0' + i;
      unlink(name);
    }
  }

  printf("createdelete ok\n");
}

// can I unlink a file and still read it?
void
unlinkread(void)
{
  int fd, fd1;

  printf("unlinkread test\n");
  fd = open("unlinkread", O_CREAT | O_RDWR);
  if(fd < 0){
    printf("create unlinkread failed\n");
    exit(1);
  }
  write(fd, "hello", 5);
  close(fd);

  fd = open("unlinkread", O_RDWR);
  if(fd < 0){
    printf("open unlinkread failed\n");
    exit(1);
  }
  if(unlink("unlinkread") != 0){
    printf("unlink unlinkread failed\n");
    exit(1);
  }

  fd1 = open("unlinkread", O_CREAT | O_RDWR);
  write(fd1, "yyy", 3);
  close(fd1);

  if(read(fd, buf, sizeof(buf)) != 5){
    printf("unlinkread read failed");
    exit(1);
  }
  if(buf[0] != 'h'){
    printf("unlinkread wrong data\n");
    exit(1);
  }
  if(write(fd, buf, 10) != 10){
    printf("unlinkread write failed\n");
    exit(1);
  }
  close(fd);
  unlink("unlinkread");
  printf("unlinkread ok\n");
}

void
linktest(void)
{
  int fd;

  printf("linktest\n");

  unlink("lf1");
  unlink("lf2");

  fd = open("lf1", O_CREAT|O_RDWR);
  if(fd < 0){
    printf("create lf1 failed\n");
    exit(1);
  }
  if(write(fd, "hello", 5) != 5){
    printf("write lf1 failed\n");
    exit(1);
  }
  close(fd);

  if(link("lf1", "lf2") < 0){
    printf("link lf1 lf2 failed\n");
    exit(1);
  }
  unlink("lf1");

  if(open("lf1", 0) >= 0){
    printf("unlinked lf1 but it is still there!\n");
    exit(1);
  }

  fd = open("lf2", 0);
  if(fd < 0){
    printf("open lf2 failed\n");
    exit(1);
  }
  if(read(fd, buf, sizeof(buf)) != 5){
    printf("read lf2 failed\n");
    exit(1);
  }
  close(fd);

  if(link("lf2", "lf2") >= 0){
    printf("link lf2 lf2 succeeded! oops\n");
    exit(1);
  }

  unlink("lf2");
  if(link("lf2", "lf1") >= 0){
    printf("link non-existant succeeded! oops\n");
    exit(1);
  }

  if(link(".", "lf1") >= 0){
    printf("link . lf1 succeeded! oops\n");
    exit(1);
  }

  printf("linktest ok\n");
}

// test concurrent create/link/unlink of the same file
void
concreate(void)
{
  char file[3];
  int i, pid, n, fd;
  char fa[40];
  struct dirent *de;

  printf("concreate test\n");
  file[0] = 'C';
  file[2] = '\0';
  for(i = 0; i < 40; i++){
    file[1] = '0' + i;
    unlink(file);
    pid = fork();
    if(pid && (i % 3) == 1){
      link("C0", file);
    } else if(pid == 0 && (i % 5) == 1){
      link("C0", file);
    } else {
      fd = open(file, O_CREAT | O_RDWR);
      if(fd < 0){
        printf("concreate create %s failed\n", file);
        exit(1);
      }
      close(fd);
    }
    if(pid == 0)
      exit(0);
    else
      wait(0);
  }

  memset(fa, 0, sizeof(fa));
  DIR *dir = opendir(".");
  n = 0;
  while((de = readdir(dir)) != 0){
    if(de->d_name[0] == 'C' && de->d_name[2] == '\0'){
      i = de->d_name[1] - '0';
      if(i < 0 || i >= sizeof(fa)){
        printf("concreate weird file %s\n", de->d_name);
        exit(1);
      }
      if(fa[i]){
        printf("concreate duplicate file %s\n", de->d_name);
        exit(1);
      }
      fa[i] = 1;
      n++;
    }
  }
  closedir(dir);

  if(n != 40){
    printf("concreate not enough files in directory listing\n");
    exit(1);
  }

  for(i = 0; i < 40; i++){
    file[1] = '0' + i;
    pid = fork();
    if(pid < 0){
      printf("fork failed\n");
      exit(1);
    }
    if(((i % 3) == 0 && pid == 0) ||
       ((i % 3) == 1 && pid != 0)){
      close(open(file, 0));
      close(open(file, 0));
      close(open(file, 0));
      close(open(file, 0));
    } else {
      unlink(file);
      unlink(file);
      unlink(file);
      unlink(file);
    }
    if(pid == 0)
      exit(0);
    else
      wait(0);
  }

  printf("concreate ok\n");
}

// another concurrent link/unlink/create test,
// to look for deadlocks.
void
linkunlink()
{
  int pid, i;

  printf("linkunlink test\n");

  unlink("x");
  pid = fork();
  if(pid < 0){
    printf("fork failed\n");
    exit(1);
  }

  unsigned int x = (pid ? 1 : 97);
  for(i = 0; i < 100; i++){
    x = x * 1103515245 + 12345;
    if((x % 3) == 0){
      close(open("x", O_RDWR | O_CREAT));
    } else if((x % 3) == 1){
      link("cat", "x");
    } else {
      unlink("x");
    }
  }

  if(pid)
    wait(0);
  else
    exit(0);

  printf("linkunlink ok\n");
}

// directory that uses indirect blocks
void
bigdir(void)
{
  int i, fd;
  char name[10];

  printf("bigdir test\n");
  unlink("bd");

  fd = open("bd", O_CREAT);
  if(fd < 0){
    printf("bigdir create failed\n");
    exit(1);
  }
  close(fd);

  for(i = 0; i < 500; i++){
    name[0] = 'x';
    name[1] = '0' + (i / 64);
    name[2] = '0' + (i % 64);
    name[3] = '\0';
    if(link("bd", name) != 0){
      printf("bigdir link failed\n");
      exit(1);
    }
  }

  unlink("bd");
  for(i = 0; i < 500; i++){
    name[0] = 'x';
    name[1] = '0' + (i / 64);
    name[2] = '0' + (i % 64);
    name[3] = '\0';
    if(unlink(name) != 0){
      printf("bigdir unlink failed");
      exit(1);
    }
  }

  printf("bigdir ok\n");
}

void
subdir(void)
{
  int fd, cc;

  printf("subdir test\n");

  unlink("ff");
  if(mkdir("dd", 0777) != 0){
    printf("subdir mkdir dd failed\n");
    exit(1);
  }

  fd = open("dd/ff", O_CREAT | O_RDWR);
  if(fd < 0){
    printf("create dd/ff failed\n");
    exit(1);
  }
  write(fd, "ff", 2);
  close(fd);

  if(unlink("dd") >= 0){
    printf("unlink dd (non-empty dir) succeeded!\n");
    exit(1);
  }

  if(mkdir("/dd/dd", 0777) != 0){
    printf("subdir mkdir dd/dd failed\n");
    exit(1);
  }

  fd = open("dd/dd/ff", O_CREAT | O_RDWR);
  if(fd < 0){
    printf("create dd/dd/ff failed\n");
    exit(1);
  }
  write(fd, "FF", 2);
  close(fd);

  fd = open("dd/dd/../ff", 0);
  if(fd < 0){
    printf("open dd/dd/../ff failed\n");
    exit(1);
  }
  cc = read(fd, buf, sizeof(buf));
  if(cc != 2 || buf[0] != 'f'){
    printf("dd/dd/../ff wrong content\n");
    exit(1);
  }
  close(fd);

  if(link("dd/dd/ff", "dd/dd/ffff") != 0){
    printf("link dd/dd/ff dd/dd/ffff failed\n");
    exit(1);
  }

  if(unlink("dd/dd/ff") != 0){
    printf("unlink dd/dd/ff failed\n");
    exit(1);
  }
  if(open("dd/dd/ff", O_RDONLY) >= 0){
    printf("open (unlinked) dd/dd/ff succeeded\n");
    exit(1);
  }

  if(chdir("dd") != 0){
    printf("chdir dd failed\n");
    exit(1);
  }
  if(chdir("dd/../../dd") != 0){
    printf("chdir dd/../../dd failed\n");
    exit(1);
  }
  if(chdir("dd/../../../dd") != 0){
    printf("chdir dd/../../dd failed\n");
    exit(1);
  }
  if(chdir("./..") != 0){
    printf("chdir ./.. failed\n");
    exit(1);
  }

  fd = open("dd/dd/ffff", 0);
  if(fd < 0){
    printf("open dd/dd/ffff failed\n");
    exit(1);
  }
  if(read(fd, buf, sizeof(buf)) != 2){
    printf("read dd/dd/ffff wrong len\n");
    exit(1);
  }
  close(fd);

  if(open("dd/dd/ff", O_RDONLY) >= 0){
    printf("open (unlinked) dd/dd/ff succeeded!\n");
    exit(1);
  }

  if(open("dd/ff/ff", O_CREAT|O_RDWR) >= 0){
    printf("create dd/ff/ff succeeded!\n");
    exit(1);
  }
  if(open("dd/xx/ff", O_CREAT|O_RDWR) >= 0){
    printf("create dd/xx/ff succeeded!\n");
    exit(1);
  }
  if(open("dd", O_CREAT) >= 0){
    printf("create dd succeeded!\n");
    exit(1);
  }
  if(open("dd", O_RDWR) >= 0){
    printf("open dd rdwr succeeded!\n");
    exit(1);
  }
  if(open("dd", O_WRONLY) >= 0){
    printf("open dd wronly succeeded!\n");
    exit(1);
  }
  if(link("dd/ff/ff", "dd/dd/xx") == 0){
    printf("link dd/ff/ff dd/dd/xx succeeded!\n");
    exit(1);
  }
  if(link("dd/xx/ff", "dd/dd/xx") == 0){
    printf("link dd/xx/ff dd/dd/xx succeeded!\n");
    exit(1);
  }
  if(link("dd/ff", "dd/dd/ffff") == 0){
    printf("link dd/ff dd/dd/ffff succeeded!\n");
    exit(1);
  }
  if(mkdir("dd/ff/ff", 0777) == 0){
    printf("mkdir dd/ff/ff succeeded!\n");
    exit(1);
  }
  if(mkdir("dd/xx/ff", 0777) == 0){
    printf("mkdir dd/xx/ff succeeded!\n");
    exit(1);
  }
  if(mkdir("dd/dd/ffff", 0777) == 0){
    printf("mkdir dd/dd/ffff succeeded!\n");
    exit(1);
  }
  if(unlink("dd/xx/ff") == 0){
    printf("unlink dd/xx/ff succeeded!\n");
    exit(1);
  }
  if(unlink("dd/ff/ff") == 0){
    printf("unlink dd/ff/ff succeeded!\n");
    exit(1);
  }
  if(chdir("dd/ff") == 0){
    printf("chdir dd/ff succeeded!\n");
    exit(1);
  }
  if(chdir("dd/xx") == 0){
    printf("chdir dd/xx succeeded!\n");
    exit(1);
  }

  if(unlink("dd/dd/ffff") != 0){
    printf("unlink dd/dd/ff failed\n");
    exit(1);
  }
  if(unlink("dd/ff") != 0){
    printf("unlink dd/ff failed\n");
    exit(1);
  }
  if(unlink("dd") == 0){
    printf("unlink non-empty dd succeeded!\n");
    exit(1);
  }
  if(unlink("dd/dd") < 0){
    printf("unlink dd/dd failed\n");
    exit(1);
  }
  if(unlink("dd") < 0){
    printf("unlink dd failed\n");
    exit(1);
  }

  printf("subdir ok\n");
}

// test writes that are larger than the log.
void
bigwrite(void)
{
  int fd, sz;

  printf("bigwrite test\n");

  unlink("bigwrite");
  for(sz = 499; sz < 12*512; sz += 471){
    fd = open("bigwrite", O_CREAT | O_RDWR);
    if(fd < 0){
      printf("cannot create bigwrite\n");
      exit(1);
    }
    int i;
    for(i = 0; i < 2; i++){
      int cc = write(fd, buf, sz);
      if(cc != sz){
        printf("write(%d) ret %d\n", sz, cc);
        exit(1);
      }
    }
    close(fd);
    unlink("bigwrite");
  }

  printf("bigwrite ok\n");
}

void
bigfile(void)
{
  int fd, i, total, cc;

  printf("bigfile test\n");

  unlink("bigfile");
  fd = open("bigfile", O_CREAT | O_RDWR);
  if(fd < 0){
    printf("cannot create bigfile");
    exit(1);
  }
  for(i = 0; i < 20; i++){
    memset(buf, i, 600);
    if(write(fd, buf, 600) != 600){
      printf("write bigfile failed\n");
      exit(1);
    }
  }
  close(fd);

  fd = open("bigfile", 0);
  if(fd < 0){
    printf("cannot open bigfile\n");
    exit(1);
  }
  total = 0;
  for(i = 0; ; i++){
    cc = read(fd, buf, 300);
    if(cc < 0){
      printf("read bigfile failed\n");
      exit(1);
    }
    if(cc == 0)
      break;
    if(cc != 300){
      printf("short read bigfile\n");
      exit(1);
    }
    if(buf[0] != i/2 || buf[299] != i/2){
      printf("read bigfile wrong data\n");
      exit(1);
    }
    total += cc;
  }
  close(fd);
  if(total != 20*600){
    printf("read bigfile wrong total\n");
    exit(1);
  }
  unlink("bigfile");

  printf("bigfile test ok\n");
}

void
fourteen(void)
{
  int fd;

  // DIRSIZ is 14.
  printf("fourteen test\n");

  if(mkdir("12345678901234", 0777) != 0){
    printf("mkdir 12345678901234 failed\n");
    exit(1);
  }
  if(mkdir("12345678901234/123456789012345", 0777) != 0){
    printf("mkdir 12345678901234/123456789012345 failed\n");
    exit(1);
  }
  fd = open("123456789012345/123456789012345/123456789012345", O_CREAT);
  if(fd < 0){
    printf("create 123456789012345/123456789012345/123456789012345 failed\n");
    exit(1);
  }
  close(fd);
  fd = open("12345678901234/12345678901234/12345678901234", 0);
  if(fd < 0){
    printf("open 12345678901234/12345678901234/12345678901234 failed\n");
    exit(1);
  }
  close(fd);

  if(mkdir("12345678901234/12345678901234", 0777) == 0){
    printf("mkdir 12345678901234/12345678901234 succeeded!\n");
    exit(1);
  }
  if(mkdir("123456789012345/12345678901234", 0777) == 0){
    printf("mkdir 12345678901234/123456789012345 succeeded!\n");
    exit(1);
  }

  printf("fourteen ok\n");
}

void
rmdot(void)
{
  printf("rmdot test\n");
  if(mkdir("dots", 0777) != 0){
    printf("mkdir dots failed\n");
    exit(1);
  }
  if(chdir("dots") != 0){
    printf("chdir dots failed\n");
    exit(1);
  }
  if(unlink(".") == 0){
    printf("rm . worked!\n");
    exit(1);
  }
  if(unlink("..") == 0){
    printf("rm .. worked!\n");
    exit(1);
  }
  if(chdir("/") != 0){
    printf("chdir / failed\n");
    exit(1);
  }
  if(unlink("dots/.") == 0){
    printf("unlink dots/. worked!\n");
    exit(1);
  }
  if(unlink("dots/..") == 0){
    printf("unlink dots/.. worked!\n");
    exit(1);
  }
  if(unlink("dots") != 0){
    printf("unlink dots failed!\n");
    exit(1);
  }
  printf("rmdot ok\n");
}

void
dirfile(void)
{
  int fd;

  printf("dir vs file\n");

  fd = open("dirfile", O_CREAT);
  if(fd < 0){
    printf("create dirfile failed\n");
    exit(1);
  }
  close(fd);
  if(chdir("dirfile") == 0){
    printf("chdir dirfile succeeded!\n");
    exit(1);
  }
  fd = open("dirfile/xx", 0);
  if(fd >= 0){
    printf("create dirfile/xx succeeded!\n");
    exit(1);
  }
  fd = open("dirfile/xx", O_CREAT);
  if(fd >= 0){
    printf("create dirfile/xx succeeded!\n");
    exit(1);
  }
  if(mkdir("dirfile/xx", 0777) == 0){
    printf("mkdir dirfile/xx succeeded!\n");
    exit(1);
  }
  if(unlink("dirfile/xx") == 0){
    printf("unlink dirfile/xx succeeded!\n");
    exit(1);
  }
  if(link("README", "dirfile/xx") == 0){
    printf("link to dirfile/xx succeeded!\n");
    exit(1);
  }
  if(unlink("dirfile") != 0){
    printf("unlink dirfile failed!\n");
    exit(1);
  }

  fd = open(".", O_RDWR);
  if(fd >= 0){
    printf("open . for writing succeeded!\n");
    exit(1);
  }
  fd = open(".", 0);
  if(write(fd, "x", 1) > 0){
    printf("write . succeeded!\n");
    exit(1);
  }
  close(fd);

  printf("dir vs file OK\n");
}

// test that iput() is called at the end of _namei()
void
iref(void)
{
  int i, fd;

  printf("empty file name\n");

  // the 50 is NINODE
  for(i = 0; i < 50 + 1; i++){
    if(mkdir("irefd", 0777) != 0){
      printf("mkdir irefd failed\n");
      exit(1);
    }
    if(chdir("irefd") != 0){
      printf("chdir irefd failed\n");
      exit(1);
    }

    mkdir("", 0777);
    link("README", "");
    fd = open("", O_CREAT);
    if(fd >= 0)
      close(fd);
    fd = open("xx", O_CREAT);
    if(fd >= 0)
      close(fd);
    unlink("xx");
  }

  chdir("/");
  printf("empty file name OK\n");
}

// test that fork fails gracefully
// the forktest binary also does this, but it runs out of proc entries first.
// inside the bigger usertests binary, we run out of memory first.
void
forktest(void)
{
  int n, pid;

  printf("fork test\n");

  for(n=0; n<1000; n++){
    pid = fork();
    if(pid < 0)
      break;
    if(pid == 0)
      exit(0);
  }

  if(n == 1000){
    printf("fork claimed to work 1000 times!\n");
    exit(1);
  }

  for(; n > 0; n--){
    if(wait(0) < 0){
      printf("wait stopped early\n");
      exit(1);
    }
  }

  if(wait(0) != -1){
    printf("wait got too many\n");
    exit(1);
  }

  printf("fork test OK\n");
}

void
waitpidtest()
{
  const int N = 10;
  int pids[N];

  printf("waitpid test\n");

  for (int i = 0; i < N; ++i) {
    int pid = fork();
    if (pid < 0) {
      fprintf(stderr, "waitpidtest: fork failed\n");
      exit(1);
    }
    if (pid == 0)
      exit(i * 10);
    pids[i] = pid;
  }

  for (int i = N; --i >= 0; ) {
    int code;
    int r = waitpid(pids[i], &code, 0);
    if (r < 0) {
      fprintf(stderr, "waitpidtest: waitpid failed\n");
      exit(1);
    }
    if (r != pids[i]) {
      fprintf(stderr, "waitpidtest: unexpected pid: %d for %d\n", r, pids[i]);
      exit(1);
    }
    if (code != i * 10) {
      fprintf(stderr, "waitpidtest: unexpected exit code: %d for %d\n", code, i * 10);
      exit(1);
    }
  }

  if(wait(NULL) != -1){
    fprintf(stderr, "wait got too many\n");
    exit(1);
  }

  printf("waitpid test OK\n");
}

void
sbrktest(void)
{
  int fds[2], pid, pids[10], ppid;
  char *a, *b, *c, *lastaddr, *oldbrk, *p, scratch;
  uint amt;

  printf("sbrk test\n");
  oldbrk = sbrk(0);

  // can one sbrk() less than a page?
  a = sbrk(0);
  int i;
  for(i = 0; i < 5000; i++){
    b = sbrk(1);
    if(b != a){
      printf("sbrk test failed %d %p %p\n", i, a, b);
      exit(1);
    }
    *b = 1;
    a = b + 1;
  }
  pid = fork();
  if(pid < 0){
    printf("sbrk test fork failed\n");
    exit(1);
  }
  c = sbrk(1);
  c = sbrk(1);
  if(c != a + 1){
    printf("sbrk test failed post-fork\n");
    exit(1);
  }
  if(pid == 0)
    exit(0);
  wait(0);

  // can one grow address space to something big?
#define BIG (100*1024*1024)
  a = sbrk(0);
  amt = (BIG) - (uintp)a;
  p = sbrk(amt);
  if (p != a) {
    printf("sbrk test failed to grow big address space; enough phys mem?\n");
    exit(1);
  }
  lastaddr = (char*) (BIG-1);
  *lastaddr = 99;

  // can one de-allocate?
  a = sbrk(0);
  c = sbrk(-4096);
  if(c == (char*)0xffffffff){
    printf("sbrk could not deallocate\n");
    exit(1);
  }
  c = sbrk(0);
  if(c != a - 4096){
    printf("sbrk deallocation produced wrong address, a %p c %p\n", a, c);
    exit(1);
  }

  // can one re-allocate that page?
  a = sbrk(0);
  c = sbrk(4096);
  if(c != a || sbrk(0) != a + 4096){
    printf("sbrk re-allocation failed, a %p c %p\n", a, c);
    exit(1);
  }
  if(*lastaddr == 99){
    // should be zero
    printf("sbrk de-allocation didn't really deallocate\n");
    exit(1);
  }

  a = sbrk(0);
  c = sbrk(-((char*)sbrk(0) - oldbrk));
  if(c != a){
    printf("sbrk downsize failed, a %p c %p\n", a, c);
    exit(1);
  }

  // can we read the kernel's memory?
  for(a = (char*)(KERNBASE); a < (char*) (KERNBASE+2000000); a += 50000){
    ppid = getpid();
    pid = fork();
    if(pid < 0){
      printf("fork failed\n");
      exit(1);
    }
    if(pid == 0){
      printf("oops could read %p = %x\n", a, *a);
      kill(ppid, 0);
      exit(1);
    }
    wait(0);
  }

  // if we run the system out of memory, does it clean up the last
  // failed allocation?
  if(pipe(fds) != 0){
    printf("pipe() failed\n");
    exit(1);
  }
  for(i = 0; i < sizeof(pids)/sizeof(pids[0]); i++){
    if((pids[i] = fork()) == 0){
      // allocate a lot of memory
      sbrk(BIG - (uintp)sbrk(0));
      write(fds[1], "x", 1);
      // sit around until killed
      for(;;) sleep(1000);
    }
    if(pids[i] != -1)
      read(fds[0], &scratch, 1);
  }
  // if those failed allocations freed up the pages they did allocate,
  // we'll be able to allocate here
  c = sbrk(4096);
  for(i = 0; i < sizeof(pids)/sizeof(pids[0]); i++){
    if(pids[i] == -1)
      continue;
    kill(pids[i], 0);
    wait(0);
  }
  if(c == (char*)0xffffffff){
    printf("failed sbrk leaked memory\n");
    exit(1);
  }

  if((char*)sbrk(0) > oldbrk)
    sbrk(-((char*)sbrk(0) - oldbrk));

  printf("sbrk test OK\n");
}

void
validateint(int *p)
{
#ifndef X64
  int res;
  asm("mov %%esp, %%ebx\n\t"
      "mov %3, %%esp\n\t"
      "int %2\n\t"
      "mov %%ebx, %%esp" :
      "=a" (res) :
      "a" (SYS_sleep), "n" (T_SYSCALL), "c" (p) :
      "ebx");
#endif
}

void
validatetest(void)
{
  int pid;
  uintp p, hi;

  printf("validate test\n");
  hi = 1100*1024;

  for(p = 0; p <= (uintp)hi; p += 4096){
    if((pid = fork()) == 0){
      // try to crash the kernel by passing in a badly placed integer
      validateint((int*)p);
      exit(1);
    }
    sleep(0);
    sleep(0);
    kill(pid, 0);
    wait(0);

    // try to crash the kernel by passing in a bad string pointer
    if(link("nosuchfile", (char*)p) != -1){
      printf("link should not succeed\n");
      exit(1);
    }
  }

  printf("validate ok\n");
}

// does unintialized data start out zero?
char uninit[10000];
void
bsstest(void)
{
  int i;

  printf("bss test\n");
  for(i = 0; i < sizeof(uninit); i++){
    if(uninit[i] != '\0'){
      printf("bss test failed\n");
      exit(1);
    }
  }
  printf("bss test ok\n");
}

// does exec return an error if the arguments
// are larger than a page? or does it write
// below the stack and wreck the instructions/data?
void
bigargtest(void)
{
  int pid, fd;

  unlink("bigarg-ok");
  pid = fork();
  if(pid == 0){
#define BIGARG_STRLEN  ((0x4000) / (MAXARG - 2))
    static char *args[MAXARG];
    static char longstr[BIGARG_STRLEN];
    static const char str[] = "bigargs test: failed";
    char echo[] = "echo";
    int i;
    memset(longstr, '.', sizeof(longstr));
    longstr[BIGARG_STRLEN - 1] = '\0';
    memcpy(longstr, str, sizeof(str) - 1);  // Copy except last '\0'.
    for(i = 1; i < MAXARG-1; i++)
      args[i] = longstr;
    args[0] = echo;
    args[MAXARG-1] = NULL;
    printf("bigarg test\n");
    execvp(echo, args);
    printf("bigarg test ok\n");
    fd = open("bigarg-ok", O_CREAT);
    close(fd);
    exit(0);
  } else if(pid < 0){
    printf("bigargtest: fork failed\n");
    exit(1);
  }
  wait(0);
  fd = open("bigarg-ok", 0);
  if(fd < 0){
    printf("bigarg test failed!\n");
    exit(1);
  }
  close(fd);
  unlink("bigarg-ok");
}

// what happens when the file system runs out of blocks?
// answer: balloc panics, so this test is not useful.
void
fsfull()
{
  int nfiles;
  int fsblocks = 0;

  printf("fsfull test\n");

  for(nfiles = 0; ; nfiles++){
    char name[64];
    name[0] = 'f';
    name[1] = '0' + nfiles / 1000;
    name[2] = '0' + (nfiles % 1000) / 100;
    name[3] = '0' + (nfiles % 100) / 10;
    name[4] = '0' + (nfiles % 10);
    name[5] = '\0';
    printf("writing %s\n", name);
    int fd = open(name, O_CREAT|O_RDWR);
    if(fd < 0){
      printf("open %s failed\n", name);
      break;
    }
    int total = 0;
    while(1){
      int cc = write(fd, buf, 512);
      if(cc < 512)
        break;
      total += cc;
      fsblocks++;
    }
    printf("wrote %d bytes\n", total);
    close(fd);
    if(total == 0)
      break;
  }

  while(nfiles >= 0){
    char name[64];
    name[0] = 'f';
    name[1] = '0' + nfiles / 1000;
    name[2] = '0' + (nfiles % 1000) / 100;
    name[3] = '0' + (nfiles % 100) / 10;
    name[4] = '0' + (nfiles % 10);
    name[5] = '\0';
    unlink(name);
    nfiles--;
  }

  printf("fsfull test finished\n");
}

void
uio()
{
  #define RTC_ADDR 0x70
  #define RTC_DATA 0x71

  ushort port = 0;
  uchar val = 0;
  int pid;

  printf("uio test\n");
  pid = fork();
  if(pid == 0){
    port = RTC_ADDR;
    val = 0x09;  /* year */
    /* http://wiki.osdev.org/Inline_Assembly/Examples */
    asm volatile("outb %0,%1"::"a"(val), "d" (port));
    port = RTC_DATA;
    asm volatile("inb %1,%0" : "=a" (val) : "d" (port));
    printf("uio: uio succeeded; test FAILED\n");
    exit(1);
  } else if(pid < 0){
    printf("fork failed\n");
    exit(1);
  }
  wait(0);
  printf("uio test done\n");
}

void argptest()
{
  int fd;
  fd = open("/bin/init", O_RDONLY);
  if (fd < 0) {
    fprintf(stderr, "open failed\n");
    exit(1);
  }
  read(fd, sbrk(0) - 1, -1);
  close(fd);
  printf("arg test passed\n");
}

void pagefaulttest()
{
  printf("pagefault test\n");
  pid_t pid = fork();
  if (pid < 0)
    panic("fork failed");
  if (pid == 0) {
    printf("%x\n", *((char*)0));
    exit(0);
  }
  if (wait(NULL) == 0) {
    panic("pagefault test: Reading at 0x0 should not be permitted");
  }
  printf("pagefault test passed\n");
}

#pragma GCC push_options
#pragma GCC optimize("O0")
static int modifycodetestsub()
{
  // X32
  //    5380:	55                   	push   %ebp
  //    5381:	89 e5                	mov    %esp,%ebp
  //  return 42;
  //    5383:	b8 2a 00 00 00       	mov    $0x2a,%eax

  // X64
  //    4f50:	55                   	push   %rbp
  //    4f51:	48 89 e5             	mov    %rsp,%rbp
  //  return 42;
  //    4f54:	b8 2a 00 00 00       	mov    $0x2a,%eax

  return 42;
}
#pragma GCC pop_options

void modifycodetest()
{
  printf("modifycode test\n");

  pid_t pid = fork();
  if (pid < 0)
    panic("fork failed");
  if (pid == 0) {
#if X64
    int* p = (int*)(((char*)modifycodetestsub) + 5);
#else  // X32
    int* p = (int*)(((char*)modifycodetestsub) + 4);
#endif
    *p = 1234;
    int result = modifycodetestsub();
    printf("modifycode test: result=%d\n", result);
    exit(0);
  }
  int res = 0;
  if (wait(&res) < 0 || res == 0) {
    panic("modifycode test: Modifying code should not be permitted");
  }

  printf("modifycode test passed\n");
}

int
main(int argc, char *argv[])
{
  printf("usertests starting\n");

  if(open("usertests.ran", 0) >= 0){
    printf("already ran user tests -- rebuild fs.img\n");
    return 1;
  }
  close(open("usertests.ran", O_CREAT));

  argptest();
  createdelete();
  linkunlink();
  concreate();
  fourfiles();
  sharedfd();

  bigwrite();
  bigargtest();
  bsstest();
  sbrktest();
  validatetest();

  opentest();
  writetest();
  writetest1();
  createtest();
  lseektest();
  truncatetest();
  appendtest();

  openiputtest();
  exitiputtest();
  iputtest();

  mem();
  pipe1();
  preempt();
  exitwait();

  rmdot();
  fourteen();
  bigfile();
  subdir();
  linktest();
  unlinkread();
  dirfile();
  iref();
  forktest();
  waitpidtest();
  bigdir(); // slow

  pagefaulttest();
  modifycodetest();

  uio();

  execvetest();
  exectest();

  return 0;
}
