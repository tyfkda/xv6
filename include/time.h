#pragma once

typedef unsigned long time_t;

struct tm {
  int tm_sec;   // Second [0, 60/61]
  int tm_min;   // Minute [0, 59]
  int tm_hour;  // Hour   [0, 23]
  int tm_mday;  // Day    [1, 31]
  int tm_mon;   // Month  [0, 11]
  int tm_year;  // Year   - 1900
  int tm_wday;  // Day of the week
  int tm_yday;  // Day of the year
  int tm_isdst; // Summer time?
};

time_t time(time_t *tloc);
struct tm *localtime(time_t *tloc);
