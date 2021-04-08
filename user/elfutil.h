// ELF format utility

#pragma once

#if !defined(__XV6) && !defined(__linux__)

#define ELF_NOT_SUPPORTED

#endif

#ifndef ELF_NOT_SUPPORTED

#include <stdint.h>  // ssize_t
#include <stdio.h>  // FILE

#if defined(__XV6)
// XV6
#include "../kernel/types.h"
#include "../kernel/elf.h"

#elif defined(__linux__)
// Linux
#include <elf.h>

#endif

#if !defined(__NO_ELF_OBJ)
#include "table.h"

// String table for ELF.
typedef struct {
  Table offsets;
  size_t size;
} Strtab;

void strtab_init(Strtab *strtab);
size_t strtab_add(Strtab *strtab, const Name *name);
void *strtab_dump(Strtab *strtab);

//

typedef struct {
  Strtab strtab;
  Table indices;
  Elf64_Sym *buf;
  int count;
} Symtab;

void symtab_init(Symtab *symtab);
Elf64_Sym *symtab_add(Symtab *symtab, const Name *name);
#endif  // !defined(__NO_ELF_OBJ)


//

void out_elf_header(FILE *fp, uintptr_t entry, int phnum, int shnum);
void out_program_header(FILE *fp, int sec, uintptr_t offset, uintptr_t vaddr, size_t filesz,
                        size_t memsz);

#endif  // !ELF_NOT_SUPPORTED
