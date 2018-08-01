#pragma once

#include <stdarg.h>
#include "stddef.h"

#define EOF  (-1)

typedef struct FILE FILE;

extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;

FILE* fopen(const char* fileName, const char* mode);
int fclose(FILE* fp);
size_t fwrite(const void* buffer, size_t size, size_t count, FILE* fp);
size_t fread(void* buffer, size_t size, size_t count, FILE* fp);

int fileno(const FILE *);

int getc(void);
int getchar(void);
int fgetc(FILE* fp);
char* gets_s(char*, size_t max);
char* fgets_s(char*, size_t max, FILE* fp);
int putchar(int);

int printf(const char*, ...);
int fprintf(FILE*, const char*, ...);
int sprintf(char*, const char*, ...);
int snprintf(char*, size_t n, const char*, ...);
int vfprintf(FILE*, const char*, va_list);
int vsprintf(char*, const char*, va_list);
int vsnprintf(char*, size_t n, const char*, va_list);

int sscanf(const char * restrict s, const char * restrict format, ...);
ssize_t getline(char **pline, size_t *pcap, FILE *fp);

void perror(const char*);
