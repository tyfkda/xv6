#pragma once

#include "unistd.h"  // for pid_t

pid_t wait(int*);
pid_t waitpid(int, int*, int);
