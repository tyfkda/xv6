#include "errno.h"
#include "stdio.h"

static const char* kErrorMessages[] = {
  "Success",
  "Operaton not permitted",
  "No such file or directory",
  "No such process",
  "Interrupted system call",
  "I/O erorr",
  "No such device or address",
  "Argument list too long",
  "Exec format error",
  "Bad file number",
  "No child processes",
  "Try again",
  "Out of memory",
  "Permission denied",
  "Bad address",
  "Block device required",
  "Device or resource busy",
  "File exists",
  "Cross-device link",
  "No such device",
  "Not a directory",
  "Is a directory",
  "Invalid argument",
  "File table overflow",
  "Too many open files",
  "Not a typewriter",
};

const char *strerror(int errnum) {
  if (errnum > 0 && errnum < sizeof(kErrorMessages) / sizeof(*kErrorMessages))
    return kErrorMessages[errnum];
  return "???";
}

void perror(const char* message) {
  fprintf(stderr, "%s: %s\n", message, strerror(errno));
}
