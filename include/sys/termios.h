#pragma once

// tcgetattr, tcsetattr
#define	TCSANOW		(0)
#define	TCSADRAIN	(1)
#define	TCSAFLUSH	(2)

// c_iflag
#define BRKINT  (0)
#define INPCK   (0)
#define ISTRIP  (0)
#define ICRNL   (0)
#define IXON    (0)

// c_oflag
#define OPOST   (0)

#define CS8     (8)

// c_cflag

// c_lflag
#define ICANON  (1 << 0)
#define ECHO    (1 << 1)
#define ISIG    (1 << 2)
#define IEXTEN  (1 << 3)

// c_cc[]
#define VMIN    (0)
#define VTIME   (1)

struct termios {
  int c_iflag;
  int c_oflag;
  int c_cflag;
  int c_lflag;
  int c_cc[4];  // TODO: Fix
};

typedef int speed_t;

int tcgetattr(int fd, struct termios *p);

int tcsetattr(int fd, int optional_actions, const struct termios *p);

int tcsendbreak(int fd, int duration);

int tcdrain(int fd);

int tcflush(int fd, int queue_selector);

int tcflow(int fd, int action);

void cfmakeraw(struct termios *p);

speed_t cfgetispeed(const struct termios *p);

speed_t cfgetospeed(const struct termios *p);

int cfsetispeed(struct termios *p, speed_t speed);

int cfsetospeed(struct termios *p, speed_t speed);

int cfsetspeed(struct termios *p, speed_t speed);
