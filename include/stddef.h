#pragma once

#ifndef NULL
#define NULL  ((void*)0)
#endif

#ifdef X64
typedef long ssize_t;
typedef unsigned long size_t;
#else
typedef int ssize_t;
typedef unsigned int size_t;
#endif
