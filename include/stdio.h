#pragma once

#include <stdarg.h>

#include "types.h"

#define EOF  (-1)

typedef struct FILE FILE;

extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;

FILE* fopen(const char* fileName, const char* flag);
int fclose(FILE* fp);

int printf(const char*, ...);
int fprintf(FILE*, const char*, ...);
int sprintf(char*, const char*, ...);
int snprintf(char*, uint n, const char*, ...);
int vfprintf(FILE*, const char*, va_list);
int vsprintf(char*, const char*, va_list);
int vsnprintf(char*, uint n, const char*, va_list);

int getc(void);
int fgetc(FILE* fp);
char* gets_s(char*, uint max);
char* fgets_s(char*, uint max, FILE* fp);
