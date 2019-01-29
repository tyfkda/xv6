// elfを生成する、64bit版

#include <unistd.h>  // write

#include <stdio.h>

#if defined(LINUX)

#include <elf.h>

#define SYSCALL  "syscall\r\n"
#define SYSCALL_WRITE  "1"
#define SYSCALL_EXIT   "60"

#define START_ADDRESS    0x12345678

#else

#include "../kernel/types.h"
#include "../kernel/elf.h"

typedef struct elfhdr Elf64_Ehdr;
typedef struct proghdr Elf64_Phdr;

#define ELFCLASS32  (1)
#define ELFCLASS64  (2)

#define ELFDATA2LSB  (1)

// Version
#define EV_CURRENT  (1)

#define ELFOSABI_SYSV  (0)

// Machine
#define EM_386     (3)   // Intel 80386
#define EM_X86_64  (62)  // AMD x86-64 architecture

#define ET_EXEC  (2)  // Executable file

typedef uintp  uintptr_t;

#define SYSCALL  "int $64\r\n"
#define SYSCALL_WRITE  "16"
#define SYSCALL_EXIT   "2"

#define START_ADDRESS    0x1000
#endif


#define PAGE_ALIGN(adr) ((adr) & ~(0x1000 - 1)) // 16進下3桁を切り捨てるだけ
#define LOAD_ADDRESS    PAGE_ALIGN(START_ADDRESS)  // 0x12345000にロード
#define STRING_LEN 13

#define TO_STR(s)  TO_STR_(s)
#define TO_STR_(s) #s

#define ECX \
  TO_STR(LOAD_ADDRESS + 64 + 56) // LOAD_ADDRESS + sizeof(Elf64_Ehdr) + sizeof(Elf64_Phdr)

__asm__ ("start_:             \n"
         "  mov $" SYSCALL_WRITE ",  %eax       \n" // eax: system call number (__NR_write)
         "  mov $1,  %rdi       \n" // ebx: fd (stdout)
         "  mov $" ECX ", %rsi  \n" // ecx: addr
         "  mov $13, %rdx       \n" // edx: len
         SYSCALL
         "  mov $" SYSCALL_EXIT ",  %eax      \n" // eax: system call number (__NR_exit)
         "  mov $0,  %rdi       \n" // ebx: exit code
         SYSCALL
         "end_:                   ");
extern char *start_, *end_;

void out_elf_header() {
  Elf64_Ehdr ehdr = {
    .e_ident     = { ELFMAG0, ELFMAG1, ELFMAG2 ,ELFMAG3,
                     ELFCLASS64, ELFDATA2LSB, EV_CURRENT, ELFOSABI_SYSV },
    .e_type      = ET_EXEC,
    .e_machine   = EM_X86_64,
    .e_version   = EV_CURRENT,
    .e_entry     = LOAD_ADDRESS + sizeof(Elf64_Ehdr) + sizeof(Elf64_Phdr) + STRING_LEN,
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
    .p_offset = 0,
    .p_vaddr  = LOAD_ADDRESS,
    .p_paddr  = 0, // dummy
    .p_filesz = sizeof(Elf64_Ehdr) + sizeof(Elf64_Phdr) + STRING_LEN + code_len,
    .p_memsz  = sizeof(Elf64_Ehdr) + sizeof(Elf64_Phdr) + STRING_LEN + code_len,
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
  out_elf_header();
  out_program_header();
  write(1, "hello world!\n", 13);
  out_code();
  return 0;
}
