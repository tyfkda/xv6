#pragma once

#ifndef NULL
#define NULL  ((void*)0)
#endif

#ifdef X64
typedef unsigned long size_t;
#else
typedef unsigned int size_t;
#endif
