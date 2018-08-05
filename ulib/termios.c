#include "termios.h"
#include "string.h"
#include "sys/ioctl.h"
#include "unistd.h"

#include <stdio.h>

int tcgetattr(int fd, struct termios *p) {
  int flag = ioctl(fd, TCGETS);
  if (flag == -1)
    return -1;
  memset(p, 0, sizeof(*p));

  if ((flag & (1 << 0)) != 0)  // No buffering
    p->c_lflag &= ~ICANON;
  else
    p->c_lflag |= ICANON;

  if ((flag & (1 << 1)) != 0)  // No echo
    p->c_lflag &= ~ECHO;
  else
    p->c_lflag |= ECHO;

  return 0;
}

int tcsetattr(int fd, int when, const struct termios *p) {
  int cmd;
  switch (when) {
  case TCSANOW:
    cmd = TCSETS;
    break;
  case TCSADRAIN:
    cmd = TCSETSW;
    break;
  case TCSAFLUSH:
    cmd = TCSETSF;
    break;
  default:
    //return INLINE_SYSCALL_ERROR_RETURN_VALUE (EINVAL);
    return -1;
  }

  int flag = 0;
  if ((p->c_lflag & ICANON) == 0)
    flag |= 1 << 0;  // No buffering
  if ((p->c_lflag & ECHO) == 0)
    flag |= 1 << 1;  // No echo
  return ioctl(fd, cmd, flag);
}
