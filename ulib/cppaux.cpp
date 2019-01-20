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

#if 0
void __dso_handle() {
  // Dummy implementation
}
#endif

void __gxx_personality_v0() {
  // Dummy implementation
}

void* operator new(size_t size) {
  return malloc(size);
}

void operator delete(void* adr) {
  free(adr);
}

void operator delete(void* adr, size_t size) {
  free(adr);
}
