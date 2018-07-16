#include "stdio.h"
#include "stdlib.h"

static const unsigned char prog[] = {
#ifdef X64
  0x55,                   //	push   %rbp
  0xb8, 0x7b, 0x00, 0x00, 0x00,       //	mov    $0x7b,%eax
  0x48, 0x89, 0xe5,             //	mov    %rsp,%rbp
  0x5d,                   //	pop    %rbp
  0xc3,                   //	retq
  0x0f, 0x1f, 0x44, 0x00, 0x00,       //	nopl   0x0(%rax,%rax,1)
#else
  0x55,                   //	push   %ebp
  0xb8, 0x7b, 0x00, 0x00, 0x00,       //	mov    $0x7b,%eax
  0x89, 0xe5,                //	mov    %esp,%ebp
  0x5d,                   //	pop    %ebp
  0xc3,                   //	ret
  0x66, 0x90,                //	xchg   %ax,%ax
  0x66, 0x90,                //	xchg   %ax,%ax
  0x66, 0x90,                //	xchg   %ax,%ax
#endif
};

int main() {
  void* mem = malloc(sizeof(prog));
  if (mem == 0) {
    fprintf(stderr, "Alloc memory failed: %d bytes\n", (int)sizeof(prog));
    exit(1);
  }

  int(*f)() = (int(*)())mem;
  printf("Call dynamic function...\n");
  int result = f();
  printf("Success! result=%d\n", result);

  return 0;
}
