#pragma once

#include "stddef.h"

char* strcpy(char*, const char*);
void *memmove(void*, const void*, int);
char* strchr(const char*, char);
int strcmp(const char*, const char*);
int strncmp(const char*, const char*, size_t);
size_t strlen(const char*);
char* strdup(const char*);
void* memset(void*, int, size_t);
