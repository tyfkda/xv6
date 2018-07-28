#pragma once

#include "stddef.h"

// string.c
int memcmp(const void*, const void*, size_t);
void* memmove(void*, const void*, size_t);
void* memset(void*, int, size_t);
char* safestrcpy(char*, const char*, int);
int strlen(const char*);
int strncmp(const char*, const char*, size_t);
char* strncpy(char*, const char*, size_t);
int strlen(const char*);
char* strchr(const char*, char);

int atoi(const char *s);
