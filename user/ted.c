#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>

#ifndef NULL
#define NULL   ((void*)0)
#endif
#ifndef FALSE
#define FALSE  (0)
#endif
#ifndef TRUE
#define TRUE   (1)
#endif

#define ESC  '\x1b'
#define C(x)  ((x)-'@')  // Control-x

void setCursorPosition(int col, int row) {
  printf("\x1b[%d;%dH", col, row);
}

int getCursorPosition(int* cols, int* rows) {
  printf("\x1b[6n");

  /* Read the response: ESC [ rows ; cols R */
  char buf[32];
  int i = 0;
  for (i = 0; i < sizeof(buf) - 1; ++i) {
    int c = getchar();
    if (c == EOF)
      break;
    buf[i] = c;
    if (c == 'R')
      break;
  }
  buf[i] = '\0';

  if (buf[0] != ESC || buf[1] != '[')
    return FALSE;
  const char* semi = strchr(&buf[2], ';');
  if (semi == NULL)
    return FALSE;
  *cols = atoi(&buf[2]);
  *rows = atoi(semi + 1);
  return TRUE;
}

int getWindowSize(int* cols, int* rows) {
  if (!getCursorPosition(cols, rows))
    return FALSE;
  printf("\x1b[999C\x1b[999B");
  return getCursorPosition(cols, rows);
}

typedef struct {
  int maxcols, maxrows;
  int cx, cy;  // Cursor position
} Window;

int init_editor(Window* win) {
  if (!getWindowSize(&win->maxcols, &win->maxrows)) {
    fprintf(stderr, "getWindowSize failed\n");
    return FALSE;
  }
  win->cx = win->cy = 0;

  printf("\x1b[2J"  // Clear screen.
         "\x1b[H");  // Move to the top left.

  return TRUE;
}

void edit(Window* win) {
  for (;;) {
    int c = getchar();
    if (c == EOF || c == '\0' || c == '\x03')  // Ctrl-C
      break;

    setCursorPosition(win->maxcols, 1);
    printf("[%02x]    ", c);
    setCursorPosition(win->cy + 1, win->cx + 1);

    switch (c) {
    case '\n':
    case '\r':
      if (win->cy < win->maxcols - 1) {
        putchar('\n');
        win->cx = 0;
        ++win->cy;
      }
      break;

    case C('F'):
      if (win->cx < win->maxrows - 1) {
        ++win->cx;
        //puts("\x1b[C");  // Cursor right
        setCursorPosition(win->cy + 1, win->cx + 1);
      }
      break;

    case C('B'):
      if (win->cx > 0) {
        --win->cx;
        //puts("\x1b[D");  // Cursor left
        setCursorPosition(win->cy + 1, win->cx + 1);
      }
      break;

    case C('P'):
      if (win->cy > 0) {
        --win->cy;
        //puts("\x1b[A");  // Cursor up
        setCursorPosition(win->cy + 1, win->cx + 1);
      }
      break;

    case C('N'):
      if (win->cy < win->maxcols - 1) {
        ++win->cy;
        //puts("\x1b[B");  // Cursor down
        setCursorPosition(win->cy + 1, win->cx + 1);
      }
      break;

    default:
      if (c >= ' ') {
        putchar(c);
        ++win->cx;
        if (win->cx >= win->maxrows) {
          --win->cx;
          //puts("\x1b[C");  // Cursor right
          setCursorPosition(win->cy + 1, win->cx + 1);
        }
      }
    }
  }
}

static struct termios save_term;

static void farewell() {
  if (tcsetattr(fileno(stdin), TCSANOW, &save_term) == -1) {
    //perror("tcsetattr(save_term) failure");
    //exit(EXIT_FAILURE);
  }
}

int main() {
  atexit(farewell);

  errno = 0;
  if (tcgetattr(fileno(stdin), &save_term) == -1) {
    perror("tcgetattr failure");
    exit(EXIT_FAILURE);
  }

  struct termios temp_term = save_term;
  //temp_term.c_iflag &= IGNCR;
  temp_term.c_lflag &= ~ICANON;
  temp_term.c_lflag &= ~ECHO;
  temp_term.c_lflag &= ~ISIG;
  temp_term.c_cc[VMIN] = 1;
  temp_term.c_cc[VTIME] = 0;

  if (tcsetattr(fileno(stdin), TCSANOW, &temp_term) == -1) {
    perror("tcsetattr(temp_term) failure");
    exit(EXIT_FAILURE);
  }

  Window win;
  if (!init_editor(&win)) {
    return 1;
  }
  edit(&win);

  return 0;
}
