#pragma once

#include "stddef.h"

#define EXIT_SUCCESS  (0)
#define EXIT_FAILURE  (1)

#ifndef __RESTRICT
#ifdef __cplusplus
#define __RESTRICT  __restrict__
#else
#define __RESTRICT  restrict
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

void exit(int) __attribute__((noreturn));
int atexit(void (*function)(void));

void* malloc(size_t);
void* calloc(size_t, size_t);
void free(void*);
void* realloc(void*, size_t);

int atoi(const char*);

char *getenv(const char *);
int setenv(const char *, const char *, int );
int unsetenv(const char *);

unsigned long strtoul(const char * __RESTRICT nptr, char ** __RESTRICT endptr, int base);
double strtod(const char *s, char **endptr);


void qsort(void *base, size_t num, size_t size,
           int (*compare)(const void*, const void*));

int mkstemp(char *tmpl);
int mkstemps(char *tmpl, int suffixlen);

#ifdef __cplusplus
}  // extern "C"
#endif
