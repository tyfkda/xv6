#include "stdio.h"
#include "time.h"
#include "user.h"

int main() {
  static const char* kDayOfWeek[] = {
    "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat",
  };

  time_t t;
  if (time(&t) == (time_t)-1) {
    fprintf(stderr, "Failed to get time\n");
    return 1;
  }

  struct tm* t2 = localtime(&t);
  printf("%04d/%02d/%02d[%s] %02d:%02d:%02d (%d)\n",
         t2->tm_year + 1900, t2->tm_mon + 1, t2->tm_mday, kDayOfWeek[t2->tm_wday],
         t2->tm_hour, t2->tm_min, t2->tm_sec, (int)t);

  return 0;
}
