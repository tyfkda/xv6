#include "time.h"

// TODO: Use environment variable
static int timezone_offset = +9 * (60 * 60);

int isLeapYear(int year) {
  if (year < 1 || year > 9999)
    return 0;
  if (year % 4 != 0)
    return 0;
  if (year % 100 == 0)
    return year % 400 == 0;
  return 1;
}

void extractDate(time_t t, int* year, int* month, int* day, int* yday,
                 int* hour, int* minute, int* second) {
  // Ref.
  // Unix Epoch Time Conversion To Human Date and Time
  // https://www.experts-exchange.com/questions/28018974/Unix-Epoch-Time-Conversion-To-Human-Date-and-Time.html

  static const int kDays[][13] = {
    {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365},
    {0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366},  // Leap year.
  };
  const int BASE_YEAR = 1970;

  if (second != 0)
    *second = t % 60;
  if (minute != 0)
    *minute = (t / 60) % 60;
  if (hour != 0)
    *hour = (t / (60 * 60)) % 24;

  int days = t / (24 * 60 * 60);
  int leapDays = (days + 672) / 1461;
  int y = (days - leapDays) / 365;
  int daysInYear = days - (y * 365 + (y + 1) / 4);
  if (year != 0)
    *year = y + BASE_YEAR;
  const int* pDays = kDays[isLeapYear(y + BASE_YEAR)];
  int m;
  for (m = 1; m <= 12; ++m) {
    if (daysInYear < pDays[m])
      break;
  }
  if (month != 0)
    *month = m;
  if (day != 0)
    *day = daysInYear - pDays[m - 1] + 1;
  if (yday != 0)
    *yday = daysInYear;
}

// Zeller's congruence
static int zeller(int y, int m, int d) {
  // Ref.
  // Zeller's congruence - Wikipedia
  // https://en.wikipedia.org/wiki/Zeller%27s_congruence

  if (m <= 2) {
    // Treat January and Febrary as a previous year's 13, 14th.
    m += 12;
    y -= 1;
  }
  return (d + (13 * (m + 1)) / 5 + y + y / 4 - y / 100 + y / 400 + 6) % 7;
}

struct tm *localtime(time_t *pt) {
  static struct tm t;
  time_t lt = *pt + timezone_offset;
  extractDate(lt, &t.tm_year, &t.tm_mon, &t.tm_mday, &t.tm_yday,
              &t.tm_hour, &t.tm_min, &t.tm_sec);
  t.tm_year -= 1900;
  t.tm_mon -= 1;
  t.tm_wday = zeller(t.tm_year + 1900, t.tm_mon + 1, t.tm_mday);
  t.tm_isdst = 0;  // TODO
  return &t;
}
