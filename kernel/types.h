#pragma once

typedef unsigned int   uint;
typedef unsigned short ushort;
typedef unsigned char  uchar;

typedef unsigned int  uint32;
typedef unsigned long uint64;

#if X64
typedef unsigned long uint64_t;
typedef unsigned long uintp;
#else
typedef unsigned long long uint64_t;
typedef unsigned int  uintp;
#endif

typedef uintp pde_t;

typedef uint uint32_t;
typedef ushort uint16_t;
typedef uchar uint8_t;
