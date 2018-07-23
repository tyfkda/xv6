#pragma once

//#ifdef X64

typedef __builtin_va_list __gnuc_va_list;
typedef __gnuc_va_list va_list;

#define va_start(v,l)   __builtin_va_start(v,l)
#define va_end(v)       __builtin_va_end(v)
#define va_arg(v,l)     __builtin_va_arg(v,l)
#define va_copy(d,s)    __builtin_va_copy(d,s)

//#else

//typedef char* va_list;
//
//#define va_start(ap, last)  ((ap) = ((char*) &(last)) + ((sizeof(last) + 3) & ~3))
//#define va_end(ap)  ((void)0)
//#define va_arg(ap, type)  (ap += (sizeof(type) + 3) & ~3, *((type*) ((ap) - ((sizeof(type) + 3) & ~3))))
//
//#endif
