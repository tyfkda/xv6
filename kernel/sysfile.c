//
// File-system system calls.
// Mostly argument checking, since we don't trust
// user code, and calls into file.c and fs.c.
//

#include "types.h"
#include "defs.h"
#include "param.h"
#include "sys/stat.h"
#include "mmu.h"
#include "proc.h"
#include "fs.h"
#include "spinlock.h"
#include "sleeplock.h"
#include "file.h"
#include "fcntl.h"
#include "date.h"
#include "time.h"
#include "errno.h"
#include "dirent.h"

// Fetch the nth word-sized system call argument as a file descriptor
// and return both the descriptor and the corresponding struct file.
int
argfd(int n, int *pfd, struct file **pf)
{
  int fd;
  struct file *f;

  if(argint(n, &fd) < 0)
    return -1;
  if(fd < 0 || fd >= NOFILE || (f=myproc()->ofile[fd]) == 0)
    return -1;
  if(pfd)
    *pfd = fd;
  if(pf)
    *pf = f;
  return 0;
}

// Allocate a file descriptor for the given file.
// Takes over file reference from caller on success.
static int
fdalloc(struct file *f)
{
  int fd;
  struct proc *curproc = myproc();

  for(fd = 0; fd < NOFILE; fd++){
    if(curproc->ofile[fd] == 0){
      curproc->ofile[fd] = f;
      return fd;
    }
  }
  return -1;
}

int
sys_dup(void)
{
  struct file *f;
  int fd;

  if(argfd(0, 0, &f) < 0)
    return -1;
  if((fd=fdalloc(f)) < 0)
    return -1;
  filedup(f);
  return fd;
}

int
sys_read(void)
{
  struct file *f;
  int n;
  char *p;

  if(argfd(0, 0, &f) < 0 || argint(2, &n) < 0 || argptr(1, &p, n) < 0)
    return -1;
  return fileread(f, p, n);
}

int
sys_readdir(void)
{
  struct file *f;
  struct dirent *p;

  if(argfd(0, 0, &f) < 0 || argptr(1, (char**)&p, sizeof(struct dirent)) < 0)
    return -1;

  struct ddirent de;
  int size = filereaddir(f, &de);
  if (size != sizeof(de))
    return -1;

  memset(p, 0, sizeof(*p));
  p->d_ino = de.d_ino;
  strncpy(p->d_name, de.d_name, DIRSIZ);
  return 0;
}

int
sys_write(void)
{
  struct file *f;
  long n;
  char *p;

  if(argfd(0, 0, &f) < 0 || arglong(2, &n) < 0 || argptr(1, &p, n) < 0)
    return -1;
  return filewrite(f, p, n);
}

int
sys_close(void)
{
  int fd;
  struct file *f;

  if(argfd(0, &fd, &f) < 0)
    return -1;
  myproc()->ofile[fd] = 0;
  fileclose(f, 0);
  return 0;
}

int
sys_fstat(void)
{
  struct file *f;
  struct stat *st;

  if(argfd(0, 0, &f) < 0 || argptr(1, (void*)&st, sizeof(*st)) < 0)
    return -1;
  return filestat(f, st);
}

// Create the path new as a link to the same inode as old.
int
sys_link(void)
{
  char name[DIRSIZ];
  const char *new, *old;
  struct inode *dp, *ip;

  if(argcstr(0, &old) < 0 || argcstr(1, &new) < 0)
    return -1;

  begin_op();
  if((ip = namei(old)) == 0){
    end_op();
    return -1;
  }

  ilock(ip);
  if(ip->type == T_DIR){
    iunlockput(ip);
    end_op();
    return -1;
  }

  ip->nlink++;
  iupdate(ip);
  iunlock(ip);

  if((dp = nameiparent(new, name)) == 0)
    goto bad;
  ilock(dp);
  if(dp->dev != ip->dev || dirlink(dp, name, ip->inum) < 0){
    iunlockput(dp);
    goto bad;
  }
  iunlockput(dp);
  iput(ip);

  end_op();

  return 0;

bad:
  ilock(ip);
  ip->nlink--;
  iupdate(ip);
  iunlockput(ip);
  end_op();
  return -1;
}

// Is the directory dp empty except for "." and ".." ?
static int
isdirempty(struct inode *dp)
{
  int off;
  struct ddirent de;

  for(off=2*sizeof(de); off<dp->size; off+=sizeof(de)){
    if(readi(dp, (char*)&de, off, sizeof(de)) != sizeof(de))
      panic("isdirempty: readi");
    if(de.d_ino != 0)
      return 0;
  }
  return 1;
}

//PAGEBREAK!
int
sys_unlink(void)
{
  struct inode *ip, *dp;
  struct ddirent de;
  char name[DIRSIZ];
  const char *path;
  uint off;

  if(argcstr(0, &path) < 0)
    return -1;

  begin_op();
  if((dp = nameiparent(path, name)) == 0){
    end_op();
    return -1;
  }

  ilock(dp);

  // Cannot unlink "." or "..".
  if(namecmp(name, ".") == 0 || namecmp(name, "..") == 0)
    goto bad;

  if((ip = dirlookup(dp, name, &off)) == 0)
    goto bad;
  ilock(ip);

  if(ip->nlink < 1)
    panic("unlink: nlink < 1");
  if(ip->type == T_DIR && !isdirempty(ip)){
    iunlockput(ip);
    goto bad;
  }

  memset(&de, 0, sizeof(de));
  if(writei(dp, (char*)&de, off, sizeof(de)) != sizeof(de))
    panic("unlink: writei");
  if(ip->type == T_DIR){
    dp->nlink--;
    iupdate(dp);
  }
  iunlockput(dp);

  ip->nlink--;
  iupdate(ip);
  iunlockput(ip);

  end_op();

  return 0;

bad:
  iunlockput(dp);
  end_op();
  return -1;
}

static struct inode*
create(const char *path, short type, short major, short minor, int* perr)
{
  struct inode *ip, *dp;
  char name[DIRSIZ];

  if((dp = nameiparent(path, name)) == 0) {
    *perr = -ENOENT;
    return 0;
  }
  ilock(dp);

  if((ip = dirlookup(dp, name, 0)) != 0){
    iunlockput(dp);
    ilock(ip);
    if(type == T_FILE && ip->type != T_DIR)  // T_FILE or T_DEV
      return ip;
    iunlockput(ip);
    *perr = -EEXIST;
    return 0;
  }

  if((ip = ialloc(dp->dev, type)) == 0)
    panic("create: ialloc");

  ilock(ip);
  ip->major = major;
  ip->minor = minor;
  ip->nlink = 1;
  ip->mtime = cmosepochtime();
  iupdate(ip);

  if(type == T_DIR){  // Create . and .. entries.
    dp->nlink++;  // for ".."
    iupdate(dp);
    // No ip->nlink++ for ".": avoid cyclic ref count.
    if(dirlink(ip, ".", ip->inum) < 0 || dirlink(ip, "..", dp->inum) < 0)
      panic("create dots");
  }

  if(dirlink(dp, name, ip->inum) < 0)
    panic("create: dirlink");

  iunlockput(dp);

  return ip;
}

int
sys_open(void)
{
  const char *path;
  int fd, omode, errno;
  struct file *f;
  struct inode *ip;

  if(argcstr(0, &path) < 0 || argint(1, &omode) < 0)
    return -EINVAL;

  begin_op();

  if(omode & O_CREAT){
    ip = create(path, T_FILE, 0, 0, &errno);
    if(ip == 0){
      end_op();
      return errno;
    }
  } else {
    if((ip = namei(path)) == 0){
      end_op();
      return -ENOENT;
    }
    ilock(ip);
    if(ip->type == T_DIR && omode != O_RDONLY){
      iunlockput(ip);
      end_op();
      return -EISDIR;
    }
  }

  if((f = filealloc()) == 0 || (fd = fdalloc(f)) < 0){
    if(f)
      fileclose(f, 1);
    iunlockput(ip);
    end_op();
    return -EMFILE;
  }
  iunlock(ip);
  end_op();

  f->type = FD_INODE;
  f->ip = ip;
  f->off = 0;
  f->readable = !(omode & O_WRONLY);
  f->writable = (omode & O_WRONLY) || (omode & O_RDWR);
  if (f->writable) {
    if (omode & O_APPEND)
      f->off = ip->size;
    if (omode & O_TRUNC)
      ip->size = 0;
  }
  return fd;
}

int
sys_mkdir(void)
{
  const char *path;
  struct inode *ip;
  int errno = -EINVAL;

  begin_op();
  if(argcstr(0, &path) < 0 || (ip = create(path, T_DIR, 0, 0, &errno)) == 0){
    end_op();
    return errno;
  }
  iunlockput(ip);
  end_op();
  return 0;
}

int
sys_mknod(void)
{
  struct inode *ip;
  const char *path;
  int major, minor;
  int errno = -EINVAL;

  begin_op();
  if((argcstr(0, &path)) < 0 ||
     argint(1, &major) < 0 ||
     argint(2, &minor) < 0 ||
     (ip = create(path, T_DEV, major, minor, &errno)) == 0){
    end_op();
    return errno;
  }
  iunlockput(ip);
  end_op();
  return 0;
}

int
sys_chdir(void)
{
  const char *path;
  struct inode *ip;
  struct proc *curproc = myproc();

  begin_op();
  if(argcstr(0, &path) < 0 || (ip = namei(path)) == 0){
    end_op();
    return -1;
  }
  ilock(ip);
  if(ip->type != T_DIR){
    iunlockput(ip);
    end_op();
    return -1;
  }
  iunlock(ip);
  iput(curproc->cwd);
  end_op();
  curproc->cwd = ip;
  return 0;
}

int
sys_execve(void)
{
  const char *path;
  int i;
  uintp uargv, uarg;
  uintp uenvp, uenv;

  if(argcstr(0, &path) < 0 || arguintp(1, &uargv) < 0 || arguintp(2, &uenvp) < 0){
    return -1;
  }
  for(i=0;; i++){
    if(i >= MAXARG)
      return -1;
    if(fetchuintp(uargv+sizeof(uintp)*i, &uarg) < 0)
      return -1;
    if(uarg == 0){
      break;
    }
    const char* v;
    if(fetchstr(uarg, &v) < 0)
      return -1;
  }

  if (uenvp != 0) {
    for(i=0;; i++){
      if(i >= MAXENV)
        return -1;
      if(fetchuintp(uenvp+sizeof(uintp)*i, &uenv) < 0)
        return -1;
      if(uenv == 0){
        break;
      }
      const char* v;
      if(fetchstr(uenv, &v) < 0)
         return -1;
    }
  }

  return execve(path, (const char**)uargv, (const char**)uenvp);
}

int
sys_pipe(void)
{
  int *fd;
  struct file *rf, *wf;
  int fd0, fd1;

  if(argptr(0, (void*)&fd, 2*sizeof(fd[0])) < 0)
    return -1;
  if(pipealloc(&rf, &wf) < 0)
    return -1;
  fd0 = -1;
  if((fd0 = fdalloc(rf)) < 0 || (fd1 = fdalloc(wf)) < 0){
    if(fd0 >= 0)
      myproc()->ofile[fd0] = 0;
    fileclose(rf, 1);
    fileclose(wf, 1);
    return -1;
  }
  fd[0] = fd0;
  fd[1] = fd1;
  return 0;
}

int
sys_ftruncate(void)
{
  struct file *f;
  uint length;

  if(argfd(0, 0, &f) < 0 || argint(1, (int*)&length) < 0)
    return -1;
  return filetruncate(f, length);
}

int
sys_time(void)
{
  time_t *pt;

  if(arguintp(0, (uintp*)&pt) < 0){
    return -1;
  }
  *pt = cmosepochtime();
  return 0;
}

int
sys_isatty(void)
{
  struct file *f;

  if(argfd(0, 0, &f) < 0)
    return 0;
  return fileisatty(f);
}

long
sys_lseek(void)
{
  struct file *f;
  long offset;
  int whence;

  if(argfd(0, 0, &f) < 0 || arglong(1, &offset) < 0 || argint(2, &whence) < 0 ||
     !(0 <= whence && whence <= 2))
    return -1;
  return filelseek(f, offset, whence);
}
