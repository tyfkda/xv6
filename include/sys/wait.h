#pragma once

#include "unistd.h"  // for pid_t

#ifdef __cplusplus
extern "C" {
#endif

pid_t wait(int*);
pid_t waitpid(int, int*, int);

#ifdef __cplusplus
}  // extern "C"
#endif
