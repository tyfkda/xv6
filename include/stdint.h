#pragma once

typedef char  int8_t;
typedef short int16_t;
typedef int   int32_t;
typedef long  int64_t;

typedef unsigned char  uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int   uint32_t;
typedef unsigned long  uint64_t;

#if X64
typedef long intptr_t;
typedef unsigned long uintptr_t;
#else
typedef int  intpptr_t;
typedef unsigned int  uintptr_t;
#endif
