#include "termios.h"
#include "string.h"
#include "unistd.h"

int tcgetattr(int fd, struct termios *p) {
  memset(p, 0x00, sizeof(*p));
  p->c_lflag = ICANON | ECHO;
  return 0;
}

int tcsetattr(int fd, int when, const struct termios *p) {
  int flag = 0;
  if ((p->c_lflag & ICANON) == 0)
    flag |= 1 << 0;  // No buffering
  if ((p->c_lflag & ECHO) == 0)
    flag |= 1 << 1;  // No echo
  return ioctl(fd, 0, flag);
}
