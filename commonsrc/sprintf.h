#pragma once

#include <stdarg.h>
#include "stddef.h"

int sprintf(char*, const char*, ...);
int snprintf(char*, size_t n, const char*, ...);
int vsprintf(char*, const char*, va_list);
int vsnprintf(char*, size_t n, const char*, va_list);
