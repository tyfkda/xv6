#include "ctype.h"

int isdigit(int c) {
  return '0' <= c && c <= '9';
}

int isxdigit(int c) {
  return ('0' <= c && c <= '9') || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f');
}

int isalpha(int c) {
  return ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z');
}

int isalnum(int c) {
  return isalpha(c) || isdigit(c);
}

int isprint(int c) {
  return c >= ' ' && c <= 0x7f;
}

int isspace(int c) {
  return (c == ' ' || c == '\t' || c == '\n' || c == '\r' ||
          c == '\f' || c == '\v');
}

int tolower(int c) {
  return ('A' <= c && c <= 'Z') ? c + ('a' - 'A') : c;
}
