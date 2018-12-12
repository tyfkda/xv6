#pragma once

typedef unsigned int   uint;
typedef unsigned short ushort;
typedef unsigned char  uchar;

typedef unsigned int  uint32;
typedef unsigned long uint64;

#if X64
typedef unsigned long uintp;
#else
typedef unsigned int  uintp;
#endif

typedef uintp pde_t;



typedef uchar uint8_t;
typedef ushort uint16_t;
typedef uint32 uint32_t;
