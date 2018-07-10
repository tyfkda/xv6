#include "stdlib.h"

extern "C" {
  void _Unwind_Resume();
  void __cxa_atexit();
  void __dso_handle();
  void __gxx_personality_v0();
}  // extern "C"


void _Unwind_Resume() {
  // Dummy implementation
}

void __cxa_atexit() {
  // Dummy implementation
}

void __dso_handle() {
  // Dummy implementation
}

void __gxx_personality_v0() {
  // Dummy implementation
}

void* operator new(unsigned long size) {
  return malloc(size);
}

void operator delete(void* adr) {
  free(adr);
}

void operator delete(void* adr, unsigned long size) {
  free(adr);
}
