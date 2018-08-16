#include "string.h"
#include "stdlib.h"  // for malloc

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

long
strtol(const char *p, char **pp, int base)
{
  long result = 0;
  if (base <= 10) {
    for (;;) {
      char c = *p++;
      if ('0' <= c && c < ('0'+ base))
        result = result * base + (c - '0');
      else
        break;
    }
  } else {
    for (;;) {
      char c = *p++;
      if ('0' <= c && c < '9')
        result = result * base + (c - '0');
      else if ('A' <= c && c < ('A' - 10 + base))
        result = result * base + (c - 'A');
      else if ('a' <= c && c < ('a' - 10 + base))
        result = result * base + (c - 'a');
      else
        break;
    }
  }

  if (pp != 0)
    *pp = (char*)(p - 1);

  return result;
}
