#include <elf.h>
#include <unistd.h>  // write

#define PAGE_ALIGN(adr) ((adr) & ~(0x1000 - 1))
#define LOAD_ADDRESS    PAGE_ALIGN(0x12345678)

__asm__ (
  "start_:                 \n"
  // Write
  "  mov $1, %eax          \n" // eax: system call number (__NR_write)
  "  mov $1, %rdi          \n" // rdi: fd (stdout)
  "  lea start_-120(%rip), %rsi    \n" // rsi: addr (120 = sizeof(Elf64_Ehdr) + sizeof(Elf64_Phdr))
  "  mov $end_ - start_ + 120, %rdx \n" // rdx: len
  "  syscall               \n"
  // Exit
  "  mov $60, %eax         \n" // eax: system call number (__NR_exit)
  "  mov $0, %rdi          \n" // rdi: exit code
  "  syscall               \n"
  "end_:\n");
extern char *start_, *end_;

void out_elf_header(void) {
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

void out_program_header(size_t code_len) {
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

void out_code(const void *code, size_t code_len) {
  write(1, code, code_len);
}

int main() {
  size_t code_len = (uintptr_t)&end_ - (uintptr_t)&start_;

  out_elf_header();
  out_program_header(code_len);
  out_code(&start_, code_len);

  return 0;
}
