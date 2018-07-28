#pragma once

#if X64
typedef long intptr_t;
typedef unsigned long uintptr_t;
#else
typedef int  intpptr_t;
typedef unsigned int  uintpptr_t;
#endif
