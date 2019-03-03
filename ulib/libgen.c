#include "libgen.h"
#include "string.h"  // strrchr

char *dirname(char *path) {
  static char dot[] = ".";

  if (path == NULL)
    return dot;

  char *last = NULL;
  for (char *p = path;; ++p) {
    char c = *p;
    if (c == '\0')
      break;
    if (c == '/') {
      if (p[1] == '\0') {
        if (p == path)  // Root.
          return path;
        *p = '\0';
        break;
      }
      last = p;
    }
  }
  if (last == NULL)
    return dot;
  if (last == path)
    ++last;
  *last = '\0';
  return path;
}
