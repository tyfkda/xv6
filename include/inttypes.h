#pragma once

#ifndef NULL
#define NULL  ((void*)0)
#endif

#ifdef X64
typedef short uint16_t;
typedef int uint32_t;
typedef unsigned long uint64_t;
#else
#endif

typedef long intptr_t;
