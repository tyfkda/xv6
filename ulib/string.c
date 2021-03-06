#include "string.h"

#include "ctype.h"  // tolower
#include "stdlib.h"  // for malloc
#include "../kernel/x86.h"  // for stosl, stosb

char*
strcpy(char *s, const char *t)
{
  char *os;

  os = s;
  while((*s++ = *t++) != '\0')
    ;
  return os;
}

char*
strstr(const char* p, const char* q)
{
  size_t len = strlen(q);
  for (; *p != '\0'; ++p) {
    if (strncmp(p, q, len) == 0)
      return (char*)p;
  }
  return 0;
}

char*
strdup(const char *s)
{
  size_t len;
  char *t;

  len = strlen(s);
  t = malloc(len + 1);
  if (t)
    strcpy(t, s);
  return t;
}

static int parse_sign(const char **pp) {
  const char *p = *pp;
  char c = *p;
  int negative = c == '-';
  if (c == '+' || c == '-')
    *pp = p + 1;
  return negative;
}

static unsigned long strtoul_sub(const char *p, char **pp, int base) {
  char digimax = '0' + (base <= 10 ? base : 10);
  char hexmax = 'a' - 10 + base;
  unsigned long result = 0;
  for (;; ++p) {
    char c = *p;
    int n;
    if ('0' <= c && c < digimax)
      n = c - '0';
    else {
      c = tolower(c);
      if ('a' <= c && c < hexmax)
        n = c - 'a' + 10;
      else
        break;
    }
    result = result * base + n;
  }

  if (pp != 0)
    *pp = (char*)p;

  return result;
}

long strtol(const char *p, char **pp, int base) {
  const char *orig = p;
  int neg = parse_sign(&p);
  char *q;
  long result = strtoul_sub(p, &q, base);
  if (q == p)
    q = (char*)orig;
  if (neg)
    result = -result;

  if (pp != 0)
    *pp = q;

  return result;
}

unsigned long strtoul(const char *p, char **pp, int base) {
  const char *orig = p;
  if (*p == '+')
    ++p;
  char *q;
  unsigned long result = strtoul_sub(p, &q, base);
  if (q == p)
    q = (char*)orig;

  if (pp != 0)
    *pp = q;

  return result;
}

void*
memset(void *dst, int c, size_t n)
{
  if ((size_t)dst%4 == 0 && n%4 == 0){
    c &= 0xFF;
    stosl(dst, (c<<24)|(c<<16)|(c<<8)|c, n/4);
  } else
    stosb(dst, c, n);
  return dst;
}

int
memcmp(const void *v1, const void *v2, size_t n)
{
  const unsigned char *s1, *s2;

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
strcmp(const char *p, const char *q)
{
  while(*p != '\0' && *p == *q)
    p++, q++;
  return (unsigned char)*p - (unsigned char)*q;
}

int
strncmp(const char *p, const char *q, size_t n)
{
  while(n > 0 && *p == *q && *p != '\0')
    n--, p++, q++;
  return n == 0 ? 0 : (unsigned char)*p - (unsigned char)*q;
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

size_t
strlen(const char *s)
{
  const char *p;

  for(p = s; *p != '\0'; ++p)
    ;
  return p - s;
}

char*
strchr(const char *s, int c)
{
  char cc = c;
  for(; *s != '\0'; ++s)
    if(*s == cc)
      return (char*)s;
  return 0;
}

char*
strrchr(const char *s, int c)
{
  char cc = c;
  char* last = 0;
  for(; *s != '\0'; ++s)
    if(*s == cc)
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

// Strings

int strcasecmp(const char *p, const char *q) {
  for (;; ++p, ++q) {
    unsigned char c1 = *p;
    unsigned char c2 = *q;
    int d = c1 - c2;
    if (d != 0)
      return d;
    if (c1 == 0)
      break;
  }
  return 0;
}

int strncasecmp(const char *p, const char *q, size_t n) {
  for (; n > 0; --n, ++p, ++q) {
    int c1 = tolower((unsigned char)*p);
    int c2 = tolower((unsigned char)*q);
    int d = c1 - c2;
    if (d != 0)
      return d;
    if (c1 == 0)
      break;
  }
  return 0;
}
