#pragma once

#ifndef NULL
#define NULL  ((void*)0)
#endif

//typedef long ssize_t;
typedef unsigned long size_t;

// Added
typedef int wint_t;

#ifndef __cplusplus
typedef int wchar_t;
#endif

#ifdef X64
typedef long ptrdiff_t;
#else
typedef int ptrdiff_t;
#endif

#ifndef NULL
#define NULL  ((void*)0)
#endif
