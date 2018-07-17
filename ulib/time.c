#include "time.h"

extern int _systime(time_t *pt);

time_t time(time_t *pt) {
  time_t t;
  if (pt == 0)
    pt = &t;
  if (_systime(pt) != 0)
    return (time_t)(-1);
  return *pt;
}
