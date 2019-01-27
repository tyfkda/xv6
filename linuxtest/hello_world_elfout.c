// hello worldなELFバイナリを出力するCのプログラム（の一番単純な奴） - memologue
// http://d.hatena.ne.jp/yupo5656/20061112/p2

#include <elf.h>
#include <unistd.h>  // write

#define PAGE_ALIGN(adr) ((adr) & ~(0x1000 - 1)) // 16進下3桁を切り捨てるだけ
#define LOAD_ADDRESS    PAGE_ALIGN(0x12345678)  // 0x12345000にロード
#define STRING_LEN 13

#define TO_STR(s)  TO_STR_(s)
#define TO_STR_(s) #s

#define ECX \
  TO_STR(LOAD_ADDRESS + 52 + 32) // LOAD_ADDRESS + sizeof(Elf32_Ehdr) + sizeof(Elf32_Phdr)

__asm__ ("start_:              \r\n"
         "movl $4,  %eax      \r\n" // eax: system call number (__NR_write)
         "movl $1,  %ebx      \r\n" // ebx: fd (stdout)
         "movl $" ECX ", %ecx \r\n" // ecx: addr
         "movl $13, %edx      \r\n" // edx: len
         "int  $0x80          \r\n"
         "movl $1,  %eax      \r\n" // eax: system call number (__NR_exit)
         "movl $0,  %ebx      \r\n" // ebx: exit code
         "int  $0x80          \r\n"
         "end_:                   ");
extern char *start_, *end_;

void out_elf_header() {
  Elf32_Ehdr ehdr = {
    .e_ident     = { ELFMAG0, ELFMAG1, ELFMAG2 ,ELFMAG3,
                     ELFCLASS32, ELFDATA2LSB, EV_CURRENT, ELFOSABI_SYSV },
    .e_type      = ET_EXEC,
    .e_machine   = EM_386,
    .e_version   = EV_CURRENT,
    .e_entry     = LOAD_ADDRESS + sizeof(Elf32_Ehdr) + sizeof(Elf32_Phdr) + STRING_LEN,
    .e_phoff     = sizeof(Elf32_Ehdr),
    .e_shoff     = 0, // dummy
    .e_flags     = 0x0,
    .e_ehsize    = sizeof(Elf32_Ehdr),
    .e_phentsize = sizeof(Elf32_Phdr),
    .e_phnum     = 1,
    .e_shentsize = 0, // dummy
    .e_shnum     = 0,
    .e_shstrndx  = 0, // dummy
  };

  write(1, &ehdr, sizeof(Elf32_Ehdr));
}

void out_program_header() {
  uintptr_t code_len = (uintptr_t)&end_ - (uintptr_t)&start_;
  Elf32_Phdr phdr = {
    .p_type   = PT_LOAD,
    .p_offset = 0x0,
    .p_vaddr  = LOAD_ADDRESS,
    .p_paddr  = 0, // dummy
    .p_filesz = sizeof(Elf32_Ehdr) + sizeof(Elf32_Phdr) + STRING_LEN + code_len,
    .p_memsz  = sizeof(Elf32_Ehdr) + sizeof(Elf32_Phdr) + STRING_LEN + code_len,
    .p_flags  = PF_R | PF_X,
    .p_align  = 0x1000,
  };

  write(1, &phdr, sizeof(Elf32_Phdr));
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
