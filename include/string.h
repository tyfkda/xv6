#pragma once

#include "stddef.h"

char* strcpy(char*, const char*);
char* strncpy(char *s, const char *t, size_t n);
char* strchr(const char*, char);
int strcmp(const char*, const char*);
int strncmp(const char*, const char*, size_t);
size_t strlen(const char*);
char* strdup(const char*);

int memcmp(const void *v1, const void *v2, size_t n);
void* memcpy(void *dst, const void *src, size_t n);
void* memmove(void*, const void*, size_t);
void* memset(void*, int, size_t);