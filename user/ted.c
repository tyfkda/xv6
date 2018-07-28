#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "unistd.h"

#define NULL   ((void*)0)
#define FALSE  (0)
#define TRUE   (1)

#define ESC  '\x1b'
#define C(x)  ((x)-'@')  // Control-x

int puts(const char* s) {
  return write(1, s, strlen(s));
}

int getCursorPosition(int* cols, int* rows) {
  printf("\x1b[6n");

  /* Read the response: ESC [ rows ; cols R */
  char buf[32];
  int i = 0;
  for (i = 0; i < sizeof(buf) - 1; ++i) {
    int c = getc();
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
  int col, row;
} Window;

int init_editor(Window* win) {
  if (!getWindowSize(&win->maxcols, &win->maxrows)) {
    fprintf(stderr, "getWindowSize failed\n");
    return FALSE;
  }
  win->col = win->row = 0;

  printf("\x1b[2J"  // Clear screen.
         "\x1b[H");  // Move to the top left.

  return TRUE;
}

void edit(Window* win) {
  for (;;) {
    unsigned char c;
    if (read(0, &c, 1) != 1)
      break;
    if (c == '\0' || c == '\x03')  // Ctrl-C
      break;

    switch (c) {
    case '\n':
    case '\r':
      if (win->col < win->maxcols - 1) {
        putchar('\n');
        win->row = 0;
        ++win->col;
      }
      break;

    case C('F'):
      if (win->row < win->maxrows) {
        ++win->row;
        printf("\x1b[D");  // Cursor left
      }
      break;

    case C('B'):
      if (win->col > 0) {
        --win->col;
        printf("\x1b[C");  // Cursor right
      }
      break;

    case C('P'):
      if (win->col > 0) {
        --win->col;
        printf("\x1b[A");  // Cursor up
      }
      break;

    case C('N'):
      if (win->col < win->maxcols - 1) {
        ++win->col;
        printf("\x1b[B");  // Cursor down
      }
      break;

    default:
      if (c >= ' ') {
        putchar(c);
        ++win->row;
        if (win->row >= win->maxrows) {
          win->row = 0;
          ++win->col;
          if (win->col >= win->maxcols) {
            // TODO: Scroll
          }
        }
      }
    }
  }
}

static void farewell() {
  ioctl(0, 0, 0);
}

int main() {
  atexit(farewell);
  ioctl(0, 0, -1);

  Window win;
  if (!init_editor(&win)) {
    return 1;
  }
  edit(&win);

  return 0;
}
