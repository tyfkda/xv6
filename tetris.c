#include <ncurses.h>
#include <stdbool.h>
#include <stdlib.h>

enum Col {
  C_FONT = 1,
  C_RED,
  C_GREEN,
  C_BLUE,
  C_YELLOW,
  C_MAGENTA,
  C_CYAN,
};

const int kColorTable[][2] = {
  {COLOR_BLACK, COLOR_WHITE},
  {COLOR_RED, COLOR_RED},
  {COLOR_GREEN, COLOR_GREEN},
  {COLOR_BLUE, COLOR_BLUE},
  {COLOR_YELLOW, COLOR_YELLOW},
  {COLOR_MAGENTA, COLOR_MAGENTA},
  {COLOR_CYAN, COLOR_CYAN},
  {COLOR_WHITE, COLOR_WHITE},
};

const int PIECE_COUNT = 7;

enum Piece {
  P_Z,
  P_S,
  P_J,
  P_O,
  P_T,
  P_I,
  P_L,
};

typedef struct {
  const char pattern[4][4];
} PieceData;

const PieceData kPieceData[7] = {
  {
    {
      "    ",
      "@@  ",
      " @@ ",
      "    ",
    },
  },
  {
    {
      "    ",
      " @@ ",
      "@@  ",
      "    ",
    },
  },
  {
    {
      "  @ ",
      "  @ ",
      " @@ ",
      "    ",
    },
  },
  {
    {
      "    ",
      " @@ ",
      " @@ ",
      "    ",
    },
  },
  {
    {
      "    ",
      " @  ",
      "@@@ ",
      "    ",
    },
  },
  {
    {
      " @  ",
      " @  ",
      " @  ",
      " @  ",
    },
  },
  {
    {
      " @  ",
      " @  ",
      " @@ ",
      "    ",
    },
  },
};

void drawPiece(int x, int y, enum Piece piece) {
  const char block[] = "  ";
  const PieceData* data = &kPieceData[piece];
  const char (*pat)[4] = data->pattern;
  attrset(COLOR_PAIR(piece + C_RED));
  for (int i = 0; i < 4; ++i) {
    for (int j = 0; j < 4; ++j) {
      if (pat[i][j] == ' ')
        continue;
      mvprintw(y + i, (x + j) * 2, block);
    }
  }
}


int row, col;

void init() {
  initscr();
  if (!has_colors()) {
    endwin();
    fprintf(stderr, "Your terminal does not support color\n");
    exit(1);
  }
  start_color();

  noecho();  // No echo.
  cbreak();  // Canonical mode: no buffering.
  curs_set(0);  // Hide cursor.
  getmaxyx(stdscr, row, col);
  mvprintw(0, 10, "(%dx%d)", row, col);
  keypad(stdscr, TRUE);  // Accept arrow keys.
  nodelay(stdscr, TRUE);  // Don't wait getch.

  for (int i = 0; i < sizeof(kColorTable) / sizeof(*kColorTable); ++i) {
    int bg = kColorTable[i][0];
    int font = kColorTable[i][1];
    init_pair(i + 1, font, bg);
  }

  bkgd(COLOR_PAIR(C_FONT));
  attrset(COLOR_PAIR(C_RED));
  clear();
}

void term() {
  endwin();
}

const int W = 10, H = 20;

void drawFrame() {
  attrset(COLOR_PAIR(C_FONT));
  for (int i = 0; i < H; ++i) {
    mvprintw(i, 0, "||");
    mvprintw(i, (W + 1) * 2, "||");
  }
  for (int i = 0; i < W + 2; ++i) {
    mvprintw(H, i * 2, "==");
  }
}

void loop() {
  int x = 10, y = 10;
  enum Piece piece = P_Z;

  for (;;) {
    int c = getch();
    if (c == 'q')
      break;

    int nx = x, ny = y;
    switch (c) {
      case KEY_LEFT:
      case 'a':
        if (--nx < 0)
          nx = 0;
        break;
      case KEY_RIGHT:
      case 'd':
        if (++nx >= col)
          nx = col - 1;
        break;
      case KEY_UP:
      case 'w':
        if (--ny < 0)
          ny = 0;
        break;
      case KEY_DOWN:
      case 's':
        if (++ny >= row)
          ny = row - 1;
        break;

      case 'n':
        if (++piece >= PIECE_COUNT) {
          piece = 0;
        }
        break;
    }
    if (x != nx || y != ny) {
      x = nx;
      y = ny;
    }

    clear();
    drawFrame();
    attrset(COLOR_PAIR(C_RED));
    //mvprintw(y, x * 2, "  ");
    //mvprintw(row - 1, 0, "key: %d  ", c);
    drawPiece(x, y, piece);
    refresh();
  }
}

int main() {
  init();
  loop();
  term();
  return 0;
}
