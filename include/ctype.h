#pragma once

#ifdef __cplusplus
extern "C" {
#endif

int isdigit(int);
int isxdigit(int);
int isalpha(int);
int isalnum(int);
int isprint(int);
int isspace(int);

int tolower(int c);

#ifdef __cplusplus
}  // extern "C"
#endif
