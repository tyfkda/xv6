#pragma once

#include <stdarg.h>

#include "types.h"

typedef struct FILE FILE;

extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;

int printf(const char*, ...);
int fprintf(FILE*, const char*, ...);
int sprintf(char*, const char*, ...);
int snprintf(char*, uint n, const char*, ...);
int vfprintf(FILE*, const char*, va_list);
int vsprintf(char*, const char*, va_list);
int vsnprintf(char*, uint n, const char*, va_list);
