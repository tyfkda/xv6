#include "defs.h"
#include "file.h"
#include "param.h"
#include "stat.h"

int
sys_ioctl(void)
{
  struct file *f;
  int request;
  uintp arg;

  if(argfd(0, 0, &f) < 0 || argint(1, &request) < 0 || arguintp(2, &arg) < 0)
    return -1;

  // file
  if(f->type != FD_INODE)
    return -1;

  // inode
  int result = -1;
  ilock(f->ip);
  struct inode *ip = f->ip;
  if(ip->type != T_DEV ||
     ip->major < 0 || ip->major >= NDEV || !devsw[ip->major].ioctl) {
    iunlock(f->ip);
  } else {
    iunlock(ip);
    result = devsw[ip->major].ioctl(request, arg);
  }

  return result;
}
