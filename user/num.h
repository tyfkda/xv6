#pragma once

#include <stdbool.h>
#include <stdint.h>  // intptr_t

enum NumType {
  NUM_CHAR,  // Small number type should be earlier.
  NUM_SHORT,
  NUM_INT,
  NUM_LONG,
  NUM_ENUM,
};

typedef union {
  intptr_t ival;
} Num;

bool can_cast_num(enum NumType dst, enum NumType src, bool is_explicit);
