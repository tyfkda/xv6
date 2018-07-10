#pragma once

#include "types.h"

int exit(int);
int atexit(void (*function)(void));

void* malloc(uint);
void* calloc(uint);
void free(void*);

int atoi(const char*);
