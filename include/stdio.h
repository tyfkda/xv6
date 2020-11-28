#pragma once

#include "stdarg.h"
#include "stddef.h"  // size_t
#include "sys/types.h"  // ssize_t

#define EOF  (-1)

enum {
  SEEK_SET,  // 0
  SEEK_CUR,  // 1
  SEEK_END,  // 2
};

#ifdef __cplusplus
#define RESTRICT  __restrict__
#else
#define RESTRICT  restrict
#endif

typedef struct FILE FILE;

extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;

#ifdef __cplusplus
extern "C" {
#endif

FILE* fopen(const char* fileName, const char* mode);
int fclose(FILE* fp);
size_t fwrite(const void* buffer, size_t size, size_t count, FILE* fp);
size_t fread(void* buffer, size_t size, size_t count, FILE* fp);
long ftell(FILE *fp);

int fileno(const FILE *);

int getc(FILE* fp);
int getchar(void);
int fgetc(FILE* fp);
int fputc(int c, FILE* fp);
char* gets_s(char*, size_t max);
char* fgets(char*, size_t max, FILE* fp);
int putchar(int);

int printf(const char*, ...);
int fprintf(FILE*, const char*, ...);
int sprintf(char*, const char*, ...);
int snprintf(char*, size_t n, const char*, ...);
int vfprintf(FILE*, const char*, va_list);
int vsprintf(char*, const char*, va_list);
int vsnprintf(char*, size_t n, const char*, va_list);

int sscanf(const char * RESTRICT s, const char * RESTRICT format, ...);
ssize_t getline(char **pline, size_t *pcap, FILE *fp);

void perror(const char*);

#ifdef __cplusplus
}  // extern "C"
#endif
