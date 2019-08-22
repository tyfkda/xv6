#pragma once

#ifdef X64
typedef long ssize_t;
#else
typedef int ssize_t;
#endif

typedef long off_t;
typedef int pid_t;
