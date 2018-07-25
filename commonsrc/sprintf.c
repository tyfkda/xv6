#include "sprintf.h"
#include "commonsrc.h"

static char kHexDigits[] = "0123456789abcdef";
static char kUpperHexDigits[] = "0123456789ABCDEF";

static int
putstr(char *out, int o, int n, const char *s)
{
  while (*s != '\0' && o < n)
    out[o++] = *s++;
  return o;
}

static int
putpadding(char *out, int o, int n, int m, char padding)
{
  if (m > n - o)
    m = n - o;
  for (; m > 0; --m)
    out[o++] = padding;
  return o;
}

// Output is not '\0' terminated.
static int
snprintuint(const char* digits, char *out, uint n, uint x, int base,
            int order, int padding)
{
  char buf[16];
  int i, o;

  i = 0;
  do{
    buf[i++] = digits[x % base];
    x /= base;
  }while(x != 0);

  if (i < order) {
    memset(buf + i, padding, order - i);
    i = order;
  }

  for (o = 0; --i >= 0 && o < n; ++o)
    out[o] = buf[i];

  return o;
}

// Only understands %d, %x, %X, %p, %s, %c and "+-0~9".
// '\0' is not put at the end if the buffer is smaller than output.
int
vsnprintf(char *out, uint n, const char *fmt, va_list ap)
{
  int c, i;
  int o;

  for(i = o = 0; fmt[i] != '\0' && o < n; i++){
    c = fmt[i] & 0xff;
    if(c != '%'){
      out[o++] = c;
      continue;
    }

    // Handle '%'
    char padding = ' ';
    int order = 0;
    int sign = 0;
    int leftalign = 0;
    c = fmt[++i] & 0xff;
    if (c == '+') {
      sign = 1;
      c = fmt[++i] & 0xff;
    } else if (c == '-') {
      leftalign = 1;
      c = fmt[++i] & 0xff;
    }
    if (c == '0') {
      padding = '0';
      c = fmt[++i] & 0xff;
    }
    if (c >= '1' && c <= '9') {
      order = c - '0';
      while (c = fmt[++i], c >= '0' && c <= '9')
        order = order * 10 + (c - '0');
    }

    if(c == 'd'){
      int x = va_arg(ap, int);
      if (sign) {
        c = '+';
        if (x < 0) {
          x = -x;
          c = '-';
        }
        out[o++] = c;
        if (o >= n)
          break;
        if (order > 1)
          --order;
      } else if (x < 0) {
        x = -x;
        out[o++] = '-';
        if (o >= n)
          break;
      }
      o += snprintuint(kHexDigits, out + o, n - o, x, 10, order, padding);
    } else if(c == 'x') {
      o += snprintuint(kHexDigits, out + o, n - o, va_arg(ap, int), 16,
                       order, padding);
    } else if(c == 'X') {
      o += snprintuint(kUpperHexDigits, out + o, n - o, va_arg(ap, int), 16,
                       order, padding);
    } else if(c == 'p') {
      o += snprintuint(kHexDigits, out + o, n - o, (uintp)va_arg(ap, void*), 16,
                       order, padding);
    } else if(c == 's'){
      const char *s = va_arg(ap, const char*);
      if(s == 0)
        s = "(null)";
      uint len = strlen(s);
      if (order <= 0 || len >= order) {
        o = putstr(out, o, n, s);
      } else if (leftalign) {
        o = putstr(out, o, n, s);
        o = putpadding(out, o, n, order - len, ' ');
      } else {
        o = putpadding(out, o, n, order - len, ' ');
        o = putstr(out, o, n, s);
      }
    } else if(c == 'c'){
      out[o++] = va_arg(ap, uint);
    } else if(c == '%'){
      out[o++] = c;
    } else {
      // Unknown % sequence.  Print it to draw attention.
      out[o++] = '%';
      if (o >= n)
        break;
      if (c != '\0')
        out[o++] = c;
    }
  }

  if (o < n)
    out[o] = '\0';
  return o;
}

int
snprintf(char *out, uint n, const char *fmt, ...)
{
  va_list ap;
  int len;
  va_start(ap, fmt);
  len = vsnprintf(out, n, fmt, ap);
  va_end(ap);
  return len;
}

int
sprintf(char *out, const char *fmt, ...)
{
  va_list ap;
  int len;
  va_start(ap, fmt);
  len = vsnprintf(out, (uint)-1, fmt, ap);
  va_end(ap);
  return len;
}
