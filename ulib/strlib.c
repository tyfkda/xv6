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

int
strcmp(const char *p, const char *q)
{
  while(*p && *p == *q)
    p++, q++;
  return (unsigned char)*p - (unsigned char)*q;
}

char*
strdup(const char *s)
{
  int len;
  char *t;

  len = strlen(s);
  t = malloc(len + 1);
  if (t)
    strcpy(t, s);
  return t;
}
