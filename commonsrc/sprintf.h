#pragma once

#include <stdarg.h>
#include "types.h"

int sprintf(char*, const char*, ...);
int snprintf(char*, uint n, const char*, ...);
int vsprintf(char*, const char*, va_list);
int vsnprintf(char*, uint n, const char*, va_list);
