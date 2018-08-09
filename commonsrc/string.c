#include "commonsrc.h"
#include "../kernel/x86.h"

void*
memset(void *dst, int c, size_t n)
{
  if ((uintp)dst%4 == 0 && n%4 == 0){
    c &= 0xFF;
    stosl(dst, (c<<24)|(c<<16)|(c<<8)|c, n/4);
  } else
    stosb(dst, c, n);
  return dst;
}

int
memcmp(const void *v1, const void *v2, size_t n)
{
  const uchar *s1, *s2;

  s1 = v1;
  s2 = v2;
  while(n-- > 0){
    if(*s1 != *s2)
      return *s1 - *s2;
    s1++, s2++;
  }

  return 0;
}

void*
memmove(void *dst, const void *src, size_t n)
{
  const char *s;
  char *d;

  s = src;
  d = dst;
  if(s < d && s + n > d){
    s += n;
    d += n;
    while(n-- > 0)
      *--d = *--s;
  } else
    while(n-- > 0)
      *d++ = *s++;

  return dst;
}

// memcpy exists to placate GCC.  Use memmove.
void*
memcpy(void *dst, const void *src, size_t n)
{
  return memmove(dst, src, n);
}

int
strncmp(const char *p, const char *q, size_t n)
{
  while(n > 0 && *p == *q && *p != '\0')
    n--, p++, q++;
  return n == 0 ? 0 : (uchar)*p - (uchar)*q;
}

char*
strncpy(char *s, const char *t, size_t n)
{
  char *os;

  os = s;
  for (; n > 0 && (*s++ = *t++) != '\0'; --n)
    ;
  return os;
}

// Like strncpy but guaranteed to NUL-terminate.
char*
safestrcpy(char *s, const char *t, int n)
{
  char *os;

  os = s;
  if(n <= 0)
    return os;
  while(--n > 0 && (*s++ = *t++) != 0)
    ;
  *s = 0;
  return os;
}

int
strlen(const char *s)
{
  int n;

  for(n = 0; s[n]; n++)
    ;
  return n;
}

char*
strchr(const char *s, char c)
{
  for(; *s != '\0'; ++s)
    if(*s == c)
      return (char*)s;
  return 0;
}

char*
strrchr(const char *s, char c)
{
  char* last = 0;
  for(; *s != '\0'; ++s)
    if(*s == c)
      last = (char*)s;
  return last;
}

int
atoi(const char *s)
{
  int n = 0;
  for (; '0' <= *s && *s <= '9'; ++s)
    n = n * 10 + (*s - '0');
  return n;
}
