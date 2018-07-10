#pragma once

struct winsize {
  unsigned short ws_row;
  unsigned short ws_col;
  unsigned short ws_xpixel;
  unsigned short ws_ypixel;
};


// /usr/include/asm-generic/ioctls.h
#define TCGETS          (0x5401)
#define TCSETS          (0x5402)
#define TCSETSW         (0x5403)
#define TCSETSF         (0x5404)
#define TIOCGWINSZ      (0x5413)

#ifdef __cplusplus
extern "C" {
#endif

int ioctl(int fd, int request, ...);

#ifdef __cplusplus
}  // extern "C"
#endif
