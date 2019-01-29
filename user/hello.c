#include "../kernel/syscall.h"

const int STDOUT_FILENO = 1;

typedef unsigned long size_t;

enum Syscall {
  WRITE = SYS_write,
  EXIT = SYS_exit,
};

void write(int fd, const char *str, size_t size) {
  asm volatile("mov %0, %%rdi;"
               "mov %1, %%rsi;"
               "mov %2, %%rdx;"
               : : "g"((long)fd), "g"(str), "g"(size));
  asm volatile("mov %0, %%eax;"
               "int $64;"
               : : "g"(WRITE));
}

void myexit(int code) {
  asm volatile("mov %0, %%rdi;"
               "mov %1, %%eax;"
               "int $64;"
               : : "g"((long)code), "g"(EXIT));
}

void _start() {
  static const char str[] = "Hello\n";
  write(STDOUT_FILENO, str, sizeof(str) - 1);
  myexit(123);
}
