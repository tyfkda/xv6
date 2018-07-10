#pragma once

#ifdef X64
typedef long jmp_buf[9];
#else
typedef int jmp_buf[6];
#endif

#ifdef __cplusplus
extern "C" {
#endif

int setjmp(jmp_buf);
void longjmp(jmp_buf, int);

#ifdef __cplusplus
}  // extern "C"
#endif
