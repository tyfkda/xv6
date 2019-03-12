#include <stdio.h>

void xxd(FILE *fp) {
  unsigned char buf[256];
  char line1[128];
  char line2[32];
  size_t adr = 0;
  for (;;) {
    size_t readsize = fread(buf, 1, sizeof(buf), fp);
    for (size_t i = 0; i < readsize; i += 16) {
      char *p = line1, *q = line2;
      p += sprintf(p, "%08x:", (int)adr);
      int n = readsize - i;
      if (n > 16)
        n = 16;
      for (int j = 0; j < n; ++j) {
        unsigned char c = buf[i + j];
        if ((j & 1) == 0)
          p += sprintf(p, " ");
        p += sprintf(p, "%02x", c);
        q += sprintf(q, "%c", 0x20 <= c && c < 0x7f ? c : '.');
      }
      *q = '\0';
      printf("%-50s %s\n", line1, line2);
      adr += 16;
    }

    if (readsize < sizeof(buf))
      break;
  }
}

int main(int argc, char *argv[]) {
  if (argc <= 1) {
    xxd(stdin);
  } else {
    for (int i = 1; i < argc; ++i) {
      FILE *fp = fopen(argv[i], "rb");
      if (fp == NULL) {
        fprintf(stderr, "Cannot open file: %s\n", argv[i]);
        return 1;
      }
      xxd(fp);
      fclose(fp);
    }
  }
  return 0;
}
