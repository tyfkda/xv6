#pragma once

#include "stddef.h"

#define EXIT_SUCCESS  (0)
#define EXIT_FAILURE  (1)

int exit(int);
int atexit(void (*function)(void));

void* malloc(size_t);
void* calloc(size_t, size_t);
void free(void*);
void* realloc(void*, size_t);

int atoi(const char*);

char *getenv(const char *);
int setenv(const char *, const char *, int );
int unsetenv(const char *);
