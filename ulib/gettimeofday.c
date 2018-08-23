#include "sys/time.h"
#include "unistd.h"  // for NULL

extern int _systime(time_t *);

int gettimeofday(struct timeval *tv, void *_tz) {
  time_t t;
  if (_systime(&t) < 0)
    return (time_t)-1;

  tv->tv_sec = t;
  tv->tv_usec = 0;

  if (_tz != NULL) {
    struct timezone *tz = _tz;
    tz->tz_minuteswest = 0;
    tz->tz_dsttime = 0;
  }
  return 0;
}
