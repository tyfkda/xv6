#include "ctype.h"

int isdigit(int c) {
  return '0' <= c && c <= '9';
}

int isprint(int c) {
  return c >= ' ' && c <= 0x7f;
}

int isspace(int c) {
  return c == ' ' || c == '\t';
}
