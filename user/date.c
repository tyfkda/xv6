#include "date.h"
#include "stdio.h"
#include "user.h"

int main() {
  struct rtcdate d;
  date(&d);
  printf("%d/%d/%d %d:%d:%d\n", d.year, d.month, d.day, d.hour, d.minute, d.second);
  return 0;
}