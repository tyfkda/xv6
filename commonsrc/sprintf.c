#include "commonsrc.h"
#include "sprintf.h"
#include "stdint.h"  // uintptr_t

#define MIN(a, b)  ((a) < (b) ? (a) : (b))

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
snprintuint(const char* digits, char *out, unsigned int n, unsigned int x, int base,
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

static int
sprintsign(char *out, int negative, int force, int *porder)
{
  int o = 0;
  if (negative) {
    out[o++] = '-';
  } else if (force) {
    out[o++] = '+';
  }
  if (*porder > 1 && o > 0)
    *porder -= o;
  return o;
}

// Only understands %d, %x, %X, %p, %s, %c and "+-0~9".
// '\0' is not put at the end if the buffer is smaller than output.
int
vsnprintf(char *out, size_t n, const char *fmt_, va_list ap)
{
  const unsigned char *fmt = (const unsigned char*)fmt_;
  int c, i;
  int o;

  for(i = o = 0; fmt[i] != '\0' && o < n; i++){
    c = fmt[i];
    if(c != '%'){
      out[o++] = c;
      continue;
    }

    // Handle '%'
    char padding = ' ';
    int order = 0, suborder = 0;
    int sign = 0;
    int leftalign = 0;
    c = fmt[++i];
    if (c == '+') {
      sign = 1;
      c = fmt[++i];
    } else if (c == '-') {
      leftalign = 1;
      c = fmt[++i];
    }
    if (c == '0') {
      padding = '0';
      c = fmt[++i];
    }
    if (c >= '1' && c <= '9') {
      order = c - '0';
      while (c = fmt[++i], c >= '0' && c <= '9')
        order = order * 10 + (c - '0');
    }
    if (c == '.') {
      while (c = fmt[++i], c >= '0' && c <= '9') {
        suborder = suborder * 10 + (c - '0');
      }
    }

    if(c == 'd'){
      int x = va_arg(ap, int);
      o += sprintsign(out + o, x < 0, sign, &order);
      unsigned int ux = x < 0 ? -x : x;
      o += snprintuint(kHexDigits, out + o, n - o, ux, 10, order, padding);
    } else if(c == 'x') {
      o += snprintuint(kHexDigits, out + o, n - o, va_arg(ap, int), 16,
                       order, padding);
    } else if(c == 'X') {
      o += snprintuint(kUpperHexDigits, out + o, n - o, va_arg(ap, int), 16,
                       order, padding);
    } else if(c == 'p') {
      o += snprintuint(kHexDigits, out + o, n - o, (uintptr_t)va_arg(ap, void*), 16,
                       order, padding);
    } else if(c == 's'){
      // ("%5", "foo")         = "  foo"
      // ("%-5", "foo")        = "foo  "
      // ("%5", "foobarbaz")   = "foobarbaz"
      // ("%.3", "foobarbaz")  = "foo"
      // ("%5.7", "foobarbaz") = "foobarb"
      // ("%5.3", "foobarbaz") = "  foo"

      const char *s = va_arg(ap, const char*);
      if(s == 0)
        s = "(null)";
      size_t len = strlen(s);
      if (suborder > 0)
        len = MIN(len, suborder);
      if (order <= 0 || len >= order) {
        o = putstr(out, o, MIN(n, o + len), s);
      } else {
        if (leftalign) {
          o = putstr(out, o, MIN(n, o + len), s);
          o = putpadding(out, o, n, order - len, ' ');
        } else {
          o = putpadding(out, o, n, order - len, ' ');
          o = putstr(out, o, MIN(n, o + len), s);
        }
      }
    } else if(c == 'c'){
      out[o++] = va_arg(ap, unsigned int);
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
snprintf(char *out, size_t n, const char *fmt, ...)
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
  len = vsnprintf(out, (size_t)-1, fmt, ap);
  va_end(ap);
  return len;
}
