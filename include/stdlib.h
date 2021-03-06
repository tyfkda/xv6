#pragma once

#include "stddef.h"

#define EXIT_SUCCESS  (0)
#define EXIT_FAILURE  (1)

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
void qsort(void *base, size_t nmemb, size_t size, int (*compare)(const void *, const void *));

char *getenv(const char *);
int setenv(const char *, const char *, int );
int unsetenv(const char *);

long strtol(const char *p, char **pp, int base);
unsigned long strtoul(const char *p, char **pp, int base);

#ifdef __cplusplus
}  // extern "C"
#endif
