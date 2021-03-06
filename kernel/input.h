#pragma once

#include "types.h"
#include "file.h"

#define BACKSPACE 0x100

#define INPUT_BUF 128
struct input {
  struct spinlock lock;
  uchar buf[INPUT_BUF];
  uint r;  // Read index
  uint w;  // Write index
  uint e;  // Edit index
  uint c;  // Cursor index

  // Flags
  int nobuffering: 1;  // No line buffering?
  int noechoback: 1;   // No echo back?
};

void inputinit(struct input *input);
int inputread(struct input *input, void *dst, int n);
int inputwrite(struct input *input, void *dst, int n);
void inputintr(struct input *input, int (*getc)(void), void (*putc)(int));
