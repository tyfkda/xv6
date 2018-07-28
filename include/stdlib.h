#pragma once

#include "types.h"

#define EXIT_SUCCESS  (0)
#define EXIT_FAILURE  (1)

int exit(int);
int atexit(void (*function)(void));

void* malloc(uint);
void* calloc(uint);
void free(void*);

int atoi(const char*);
