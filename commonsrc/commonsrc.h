#pragma once

#include "stddef.h"

// string.c
char* safestrcpy(char*, const char*, int);
char* strncpy(char*, const char*, size_t);
char* strchr(const char*, char);
char* strrchr(const char*, char);
int strncmp(const char*, const char*, size_t);
size_t strlen(const char*);

int memcmp(const void *v1, const void *v2, size_t n);
void* memcpy(void *dst, const void *src, size_t n);
void* memmove(void*, const void*, size_t);
void* memset(void*, int, size_t);

int atoi(const char *s);
