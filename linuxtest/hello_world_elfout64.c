// elfを生成する、64bit版

#include <elf.h>
#include <unistd.h>  // write

#include <stdio.h>

#define PAGE_ALIGN(adr) ((adr) & ~(0x1000 - 1)) // 16進下3桁を切り捨てるだけ
#define LOAD_ADDRESS    PAGE_ALIGN(0x12345678)  // 0x12345000にロード

__asm__ ("start_:             \r\n"
         "  mov $1,  %eax       \r\n" // eax: system call number (__NR_write)
         "  mov $1,  %rdi       \r\n" // ebx: fd (stdout)
         "  lea msg(%rip), %rsi \r\n" // ecx: addr
         "  mov $end_ - msg, %rdx \r\n" // edx: len
         "  syscall             \r\n"
         "  mov $60,  %eax      \r\n" // eax: system call number (__NR_exit)
         "  mov $0,  %rdi       \r\n" // ebx: exit code
         "  syscall             \r\n"
         "msg:\r\n"
         "  .ascii \"hello world!\" \r\n"
         "  .byte 10 \r\n"
         "end_:                   ");
extern char *start_, *end_;

void out_elf_header() {
  Elf64_Ehdr ehdr = {
    .e_ident     = { ELFMAG0, ELFMAG1, ELFMAG2 ,ELFMAG3,
                     ELFCLASS64, ELFDATA2LSB, EV_CURRENT, ELFOSABI_SYSV },
    .e_type      = ET_EXEC,
    .e_machine   = EM_X86_64,
    .e_version   = EV_CURRENT,
    .e_entry     = LOAD_ADDRESS + sizeof(Elf64_Ehdr) + sizeof(Elf64_Phdr),
    .e_phoff     = sizeof(Elf64_Ehdr),
    .e_shoff     = 0, // dummy
    .e_flags     = 0x0,
    .e_ehsize    = sizeof(Elf64_Ehdr),
    .e_phentsize = sizeof(Elf64_Phdr),
    .e_phnum     = 1,
    .e_shentsize = 0, // dummy
    .e_shnum     = 0,
    .e_shstrndx  = 0, // dummy
  };

  write(1, &ehdr, sizeof(Elf64_Ehdr));
}

void out_program_header() {
  uintptr_t code_len = (uintptr_t)&end_ - (uintptr_t)&start_;
  Elf64_Phdr phdr = {
    .p_type   = PT_LOAD,
    .p_offset = 0x0,
    .p_vaddr  = LOAD_ADDRESS,
    .p_paddr  = 0, // dummy
    .p_filesz = sizeof(Elf64_Ehdr) + sizeof(Elf64_Phdr) + code_len,
    .p_memsz  = sizeof(Elf64_Ehdr) + sizeof(Elf64_Phdr) + code_len,
    .p_flags  = PF_R | PF_X,
    .p_align  = 0x1000,
  };

  write(1, &phdr, sizeof(Elf64_Phdr));
}

void out_code() {
  uintptr_t code_len = (uintptr_t)&end_ - (uintptr_t)&start_;
  write(1, &start_, code_len);
}

int main() {
  //fprintf(stderr, "sizeof(elf64_ehdr)=%d, sizeof(elf64_phdr)=%d\n", (int)sizeof(Elf64_Ehdr), (int)sizeof(Elf64_Phdr));

  out_elf_header();
  out_program_header();
  out_code();
  return 0;
}
