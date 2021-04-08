#define __NO_FLONUM
 #define __NO_ELF_OBJ

#include <assert.h>
#include <ctype.h>
#include <stdint.h>  // uintptr_t
#include <stdio.h>
#include <stdlib.h>  // malloc, calloc
#include <string.h>
#include <strings.h>  // strncasecmp
#include <unistd.h>

#include "asm_x86.h"
#include "elfutil.h"
#include "gen.h"
#include "ir_asm.h"
#include "parse_asm.h"
#include "table.h"
#include "util.h"

#define PROG_START   (0x100)

#if defined(__XV6)
// XV6
#include "../kernel/syscall.h"
#include "../kernel/traps.h"

#define START_ADDRESS    0x1000

#elif defined(__linux__)
// Linux

#include <sys/stat.h>

#define START_ADDRESS    (0x01000000 + PROG_START)

#else

#define UNSUPPORTED

#endif

#if defined(UNSUPPORTED)
int main() {
  fprintf(stderr, "AS: unsupported environment\n");
  return 1;
}

#else

#define LOAD_ADDRESS    START_ADDRESS
#define DATA_ALIGN      (0x1000)

void parse_file(FILE *fp, const char *filename, Vector **section_irs, Table *label_table) {
  ParseInfo info;
  info.filename = filename;
  info.lineno = 1;
  for (;; ++info.lineno) {
    char *rawline = NULL;
    size_t capa = 0;
    ssize_t len = getline(&rawline, &capa, fp);
    if (len == -1)
      break;
    info.rawline = rawline;

    Vector *irs = section_irs[current_section];
    Line *line = parse_line(&info);
    if (line == NULL)
      continue;

    if (line->label != NULL) {
      vec_push(irs, new_ir_label(line->label));

      if (!add_label_table(label_table, line->label, current_section, true, false))
        err = true;
    }

    if (line->dir == NODIRECTIVE) {
      Code code;
      if (assemble_inst(&line->inst, &info, &code)) {
        if (code.len > 0)
          vec_push(irs, new_ir_code(&code));
      }
    } else {
      handle_directive(&info, line->dir, section_irs, label_table);
    }
  }
}

static void put_padding(FILE *fp, uintptr_t start) {
  long cur = ftell(fp);
  if (start > (size_t)cur) {
    size_t size = start - (uintptr_t)cur;
    char *buf = calloc(1, size);
    fwrite(buf, size, 1, fp);
    free(buf);
  }
}

static void drop_all(FILE *fp) {
  for (;;) {
    char buf[4096];
    size_t size = fread(buf, 1, sizeof(buf), fp);
    if (size < sizeof(buf))
      break;
  }
}

#if !defined(__NO_ELF_OBJ)
static void putnum(FILE *fp, unsigned long num, int bytes) {
  for (int i = 0; i < bytes; ++i) {
    fputc(num, fp);
    num >>= 8;
  }
}

static int output_obj(const char *ofn, Table *label_table, Vector *unresolved) {
  size_t codesz, rodatasz, datasz, bsssz;
  get_section_size(SEC_CODE, &codesz, NULL);
  get_section_size(SEC_RODATA, &rodatasz, NULL);
  get_section_size(SEC_DATA, &datasz, NULL);
  get_section_size(SEC_BSS, &bsssz, NULL);

  // Construct symtab and strtab.
  Symtab symtab;
  symtab_init(&symtab);
  {
    // UND
    Elf64_Sym *sym;
    sym = symtab_add(&symtab, alloc_name("", NULL, false));
    sym->st_info = ELF64_ST_INFO(STB_LOCAL, STT_NOTYPE);
    // SECTION
    for (int i = 0; i < 4; ++i) {
      sym = symtab_add(&symtab, alloc_name("", NULL, false));
      sym->st_info = ELF64_ST_INFO(STB_LOCAL, STT_SECTION);
      sym->st_shndx = i + 1;  // Section index.
    }

    // Label symbols
    const Name *name;
    LabelInfo *info;
    for (int it = 0; (it = table_iterate(label_table, it, &name, (void**)&info)) != -1; ) {
      if (!(info->flag & LF_GLOBAL) || !(info->flag & LF_DEFINED))
        continue;
      sym = symtab_add(&symtab, name);
      sym->st_info = ELF64_ST_INFO(STB_GLOBAL, STT_NOTYPE);
      sym->st_value = info->address - section_start_addresses[info->section];
      sym->st_shndx = info->section + 1;  // Symbol index for Local section.
    }
  }

  FILE *ofp;
  if (ofn == NULL) {
    ofp = stdout;
  } else {
    ofp = fopen(ofn, "wb");
    if (ofp == NULL) {
      fprintf(stderr, "Failed to open output file: %s\n", ofn);
      if (!isatty(STDIN_FILENO))
        drop_all(stdin);
      return 1;
    }
  }

  uintptr_t entry = 0;
  int phnum = 0;
  int shnum = 11;
  out_elf_header(ofp, entry, phnum, shnum);

  uintptr_t addr = sizeof(Elf64_Ehdr);
  uintptr_t code_ofs = addr;
  output_section(ofp, SEC_CODE);
  uintptr_t rodata_ofs = addr += codesz;
  if (rodatasz > 0) {
    rodata_ofs = ALIGN(rodata_ofs, 0x10);
    put_padding(ofp, rodata_ofs);
    output_section(ofp, SEC_RODATA);
    addr = rodata_ofs + rodatasz;
  }
  uintptr_t data_ofs = addr;
  if (datasz > 0) {
    data_ofs = ALIGN(data_ofs, 0x10);
    put_padding(ofp, data_ofs);
    output_section(ofp, SEC_DATA);
    addr = data_ofs + datasz;
  }
  uintptr_t bss_ofs = addr;
  if (bsssz > 0) {
    bss_ofs = ALIGN(bss_ofs, 0x10);
    put_padding(ofp, bss_ofs);
    addr = bss_ofs;
  }

  int rela_counts[SECTION_COUNT];
  memset(rela_counts, 0x00, sizeof(rela_counts));
  for (int i = 0; i < unresolved->len; ++i) {
    UnresolvedInfo *u = unresolved->data[i];
    assert(u->src_section >= 0 && u->src_section < SECTION_COUNT);
    ++rela_counts[u->src_section];
  }

  Elf64_Rela *rela_bufs[SECTION_COUNT];
  for (int i = 0; i < SECTION_COUNT; ++i) {
    int count = rela_counts[i];
    rela_bufs[i] = count <= 0 ? NULL : calloc(count, sizeof(*rela_bufs[0]));
  }
  memset(rela_counts, 0x00, sizeof(rela_counts));  // Reset count.

  for (int i = 0; i < unresolved->len; ++i) {
    UnresolvedInfo *u = unresolved->data[i];
    Elf64_Rela *rela = &rela_bufs[u->src_section][rela_counts[u->src_section]++];
    switch (u->kind) {
    case UNRES_EXTERN:
    case UNRES_EXTERN_PC32:
      {
        Elf64_Sym *sym = symtab_add(&symtab, u->label);
        sym->st_info = ELF64_ST_INFO(STB_GLOBAL, STT_NOTYPE);
        size_t index = sym - symtab.buf;

        rela->r_offset = u->offset;
        rela->r_info = ELF64_R_INFO(index, u->kind == UNRES_EXTERN_PC32 ? R_X86_64_PC32 : R_X86_64_PLT32);
        rela->r_addend = u->add;
      }
      break;
    case UNRES_OTHER_SECTION:
      {
        LabelInfo *label = table_get(label_table, u->label);
        assert(label != NULL);
        int rodata_index = label->section + 1;  // Symtab index for .rodata section = section number + 1
        rela->r_offset = u->offset;
        rela->r_info = ELF64_R_INFO(rodata_index, R_X86_64_PC32);
        rela->r_addend = u->add;
      }
      break;
    case UNRES_ABS64:
      {
        LabelInfo *label = table_get(label_table, u->label);
        if (label == NULL || label->flag & LF_GLOBAL) {
          Elf64_Sym *sym = symtab_add(&symtab, u->label);
          sym->st_info = ELF64_ST_INFO(STB_GLOBAL, STT_NOTYPE);
          size_t index = sym - symtab.buf;

          rela->r_offset = u->offset;
          rela->r_info = ELF64_R_INFO(index, R_X86_64_64);
          rela->r_addend = u->add;
        } else {
          rela->r_offset = u->offset;
          rela->r_info = ELF64_R_INFO(label->section + 1, R_X86_64_64);
          rela->r_addend = u->add + (label->address - section_start_addresses[label->section]);
        }
      }
      break;
    default: assert(false); break;
    }
  }

  uintptr_t rela_ofss[SECTION_COUNT];
  for (int i = 0; i < SEC_BSS; ++i) {
    rela_ofss[i] = addr = ALIGN(addr, 0x10);
    put_padding(ofp, addr);
    if (rela_counts[i] > 0) {
      fwrite(rela_bufs[i], sizeof(*rela_bufs[i]), rela_counts[i], ofp);
      addr += sizeof(*rela_bufs[i]) * rela_counts[i];
    }
  }

  uintptr_t symtab_ofs = addr + unresolved->len * sizeof(Elf64_Rela);
  put_padding(ofp, symtab_ofs);
  fwrite(symtab.buf, sizeof(*symtab.buf), symtab.count, ofp);

  uintptr_t strtab_ofs = symtab_ofs + sizeof(*symtab.buf) * symtab.count;
  fwrite(strtab_dump(&symtab.strtab), symtab.strtab.size, 1, ofp);

  // Set up shstrtab.
  Strtab shstrtab;
  strtab_init(&shstrtab);

  // Output section headers.
  {
    Elf64_Shdr nulsec = {
      .sh_name = strtab_add(&shstrtab, alloc_name("", NULL, false)),
      .sh_type = SHT_NULL,
      .sh_addralign = 1,
    };
    Elf64_Shdr textsec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".text", NULL, false)),
      .sh_type = SHT_PROGBITS,
      .sh_flags = SHF_EXECINSTR | SHF_ALLOC,
      .sh_addr = 0,
      .sh_offset = code_ofs,
      .sh_size = codesz,
      .sh_link = 0,
      .sh_info = 0,
      .sh_addralign = MAX(section_aligns[SEC_CODE], 1),
      .sh_entsize = 0,
    };
    Elf64_Shdr rodatasec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".rodata", NULL, false)),
      .sh_type = SHT_PROGBITS,
      .sh_flags = SHF_ALLOC,
      .sh_addr = 0,
      .sh_offset = rodata_ofs,
      .sh_size = rodatasz,
      .sh_link = 0,
      .sh_info = 0,
      .sh_addralign = MAX(section_aligns[SEC_RODATA], 1),
      .sh_entsize = 0,
    };
    Elf64_Shdr datasec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".data", NULL, false)),
      .sh_type = SHT_PROGBITS,
      .sh_flags = SHF_WRITE | SHF_ALLOC,
      .sh_addr = 0,
      .sh_offset = data_ofs,
      .sh_size = datasz,
      .sh_link = 0,
      .sh_info = 0,
      .sh_addralign = MAX(section_aligns[SEC_DATA], 1),
      .sh_entsize = 0,
    };
    Elf64_Shdr bsssec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".bss", NULL, false)),
      .sh_type = SHT_NOBITS,
      .sh_flags = SHF_WRITE | SHF_ALLOC,
      .sh_addr = 0,
      .sh_offset = bss_ofs,
      .sh_size = bsssz,
      .sh_link = 0,
      .sh_info = 0,
      .sh_addralign = MAX(section_aligns[SEC_BSS], 1),
      .sh_entsize = 0,
    };
    Elf64_Shdr relatextsec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".rela.text", NULL, false)),
      .sh_type = SHT_RELA,
      .sh_flags = SHF_INFO_LINK,
      .sh_addr = 0,
      .sh_offset = rela_ofss[SEC_CODE],
      .sh_size = sizeof(Elf64_Rela) * rela_counts[SEC_CODE],
      .sh_link = 9,  // Index of symtab
      .sh_info = 1,  // Index of text
      .sh_addralign = 8,
      .sh_entsize = sizeof(Elf64_Rela),
    };
    Elf64_Shdr relarodatasec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".rela.rodata", NULL, false)),
      .sh_type = SHT_RELA,
      .sh_flags = SHF_INFO_LINK,
      .sh_addr = 0,
      .sh_offset = rela_ofss[SEC_RODATA],
      .sh_size = sizeof(Elf64_Rela) * rela_counts[SEC_RODATA],
      .sh_link = 9,  // Index of symtab
      .sh_info = 2,  // Index of rodata
      .sh_addralign = 8,
      .sh_entsize = sizeof(Elf64_Rela),
    };
    Elf64_Shdr reladatasec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".rela.data", NULL, false)),
      .sh_type = SHT_RELA,
      .sh_flags = SHF_INFO_LINK,
      .sh_addr = 0,
      .sh_offset = rela_ofss[SEC_DATA],
      .sh_size = sizeof(Elf64_Rela) * rela_counts[SEC_DATA],
      .sh_link = 9,  // Index of symtab
      .sh_info = 3,  // Index of data
      .sh_addralign = 8,
      .sh_entsize = sizeof(Elf64_Rela),
    };
    Elf64_Shdr strtabsec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".strtab", NULL, false)),
      .sh_type = SHT_STRTAB,
      .sh_flags = 0,
      .sh_addr = 0,
      .sh_offset = strtab_ofs,
      .sh_size = symtab.strtab.size,
      .sh_link = 0,
      .sh_info = 0,
      .sh_addralign = 1,
      .sh_entsize = 0,
    };
    Elf64_Shdr symtabsec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".symtab", NULL, false)),
      .sh_type = SHT_SYMTAB,
      .sh_flags = 0,
      .sh_addr = 0,
      .sh_offset = symtab_ofs,
      .sh_size = sizeof(*symtab.buf) * symtab.count,
      .sh_link = 8,  // Index of strtab
      .sh_info = 5,  // Number of local symbols
      .sh_addralign = 8,
      .sh_entsize = sizeof(Elf64_Sym),
    };
    Elf64_Shdr shstrtabsec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".shstrtab", NULL, false)),
      .sh_type = SHT_STRTAB,
      .sh_flags = 0,
      .sh_addr = 0,
      .sh_offset = 0,  // Dummy
      .sh_size = 0,    // Dummy
      .sh_link = 0,
      .sh_info = 0,
      .sh_addralign = 1,
      .sh_entsize = 0,
    };

    long shstrtab_ofs;
    {
      void *buf = strtab_dump(&shstrtab);
      long cur = ftell(ofp);
      shstrtab_ofs = ALIGN(cur, 0x10);
      put_padding(ofp, shstrtab_ofs);
      fwrite(buf, shstrtab.size, 1, ofp);
    }
    shstrtabsec.sh_offset = shstrtab_ofs;
    shstrtabsec.sh_size = shstrtab.size;

    long cur = ftell(ofp);
    long sh_ofs = ALIGN(cur, 0x10);
    put_padding(ofp, sh_ofs);

    fwrite(&nulsec, sizeof(nulsec), 1, ofp);
    fwrite(&textsec, sizeof(textsec), 1, ofp);
    fwrite(&rodatasec, sizeof(rodatasec), 1, ofp);
    fwrite(&datasec, sizeof(datasec), 1, ofp);
    fwrite(&bsssec, sizeof(bsssec), 1, ofp);
    fwrite(&relatextsec, sizeof(relatextsec), 1, ofp);
    fwrite(&relarodatasec, sizeof(relarodatasec), 1, ofp);
    fwrite(&reladatasec, sizeof(reladatasec), 1, ofp);
    fwrite(&strtabsec, sizeof(strtabsec), 1, ofp);
    fwrite(&symtabsec, sizeof(symtabsec), 1, ofp);
    fwrite(&shstrtabsec, sizeof(shstrtabsec), 1, ofp);

    // Write section table offset.
    fseek(ofp, 0x28, SEEK_SET);
    putnum(ofp, sh_ofs, 8);
  }

  return 0;
}
#endif

static int output_exe(const char *ofn, Table *label_table) {
  size_t codesz, rodatasz, datasz, bsssz;
  uintptr_t codeloadadr, dataloadadr;
  get_section_size(SEC_CODE, &codesz, &codeloadadr);
  get_section_size(SEC_RODATA, &rodatasz, NULL);
  get_section_size(SEC_DATA, &datasz, &dataloadadr);
  get_section_size(SEC_BSS, &bsssz, NULL);

  LabelInfo *entry = table_get(label_table, alloc_name("_start", NULL, false));
  if (entry == NULL)
    error("Cannot find label: `%s'", "_start");

  int phnum = datasz > 0 || bsssz > 0 ? 2 : 1;

  FILE *fp;
  if (ofn == NULL) {
    fp = stdout;
  } else {
    fp = fopen(ofn, "wb");
    if (fp == NULL) {
      fprintf(stderr, "Failed to open output file: %s\n", ofn);
      if (!isatty(STDIN_FILENO))
        drop_all(stdin);
      return 1;
    }
  }

  size_t rodata_align = MAX(section_aligns[SEC_RODATA], 1);
  size_t code_rodata_sz = ALIGN(codesz, rodata_align) + rodatasz;
  out_elf_header(fp, entry->address, phnum, 0);
  out_program_header(fp, 0, PROG_START, codeloadadr, code_rodata_sz, code_rodata_sz);
  if (phnum > 1) {
    size_t bss_align = MAX(section_aligns[SEC_BSS], 1);
    size_t datamemsz = ALIGN(datasz, bss_align) + bsssz;
    out_program_header(fp, 1, ALIGN(PROG_START + code_rodata_sz, DATA_ALIGN), dataloadadr, datasz,
                       datamemsz);
  }

  uintptr_t addr = PROG_START;
  put_padding(fp, addr);
  output_section(fp, SEC_CODE);
  addr += codesz;
  if (rodatasz > 0) {
    size_t rodata_align = MAX(section_aligns[SEC_RODATA], 1);
    addr = ALIGN(addr, rodata_align);
    put_padding(fp, addr);
    output_section(fp, SEC_RODATA);
    addr += rodatasz;
  }
  if (datasz > 0) {
    addr = ALIGN(addr, DATA_ALIGN);
    put_padding(fp, addr);
    output_section(fp, SEC_DATA);
    addr += datasz;
  }
  fclose(fp);

#if !defined(__XV6) && defined(__linux__)
  if (chmod(ofn, 0755) == -1) {
    perror("chmod failed\n");
    return 1;
  }
#endif
  return 0;
}

// ================================================

int main(int argc, char *argv[]) {
  const char *ofn = NULL;
  bool out_obj = false;
  int iarg;

  for (iarg = 1; iarg < argc; ++iarg) {
    char *arg = argv[iarg];
    if (*arg != '-')
      break;

    if (starts_with(arg, "-o")) {
      ofn = strdup_(arg + 2);
#if !defined(__NO_ELF_OBJ)
    } else if (strcmp(arg, "-c") == 0) {
      out_obj = true;
#endif
    } else if (strcmp(arg, "--version") == 0) {
      show_version("as");
      return 0;
    } else {
      fprintf(stderr, "Unknown option: %s\n", arg);
      return 1;
    }
  }

  if (ofn == NULL) {
    if (out_obj) {
      if (iarg < argc)
        ofn = change_ext(argv[iarg], "o");
    } else {
      ofn = "a.out";
    }
  }

  // ================================================
  // Run own assembler

  Vector *section_irs[SECTION_COUNT];
  Table label_table;
  table_init(&label_table);
  for (int i = 0; i < SECTION_COUNT; ++i)
    section_irs[i] = new_vector();

  if (iarg < argc) {
    for (int i = iarg; i < argc; ++i) {
      FILE *fp = fopen(argv[i], "r");
      if (fp == NULL)
        error("Cannot open %s\n", argv[i]);
      parse_file(fp, argv[i], section_irs, &label_table);
      fclose(fp);
      if (err)
        break;
    }
  } else {
    parse_file(stdin, "*stdin*", section_irs, &label_table);
  }

  if (!out_obj)
    section_aligns[SEC_DATA] = DATA_ALIGN;

  Vector *unresolved = out_obj ? new_vector() : NULL;
  if (!err) {
    bool settle1, settle2;
    do {
      settle1 = calc_label_address(LOAD_ADDRESS, section_irs, &label_table);
      settle2 = resolve_relative_address(section_irs, &label_table, unresolved);
    } while (!(settle1 && settle2));
    emit_irs(section_irs, &label_table);
  }

  if (err) {
    return 1;
  }

  fix_section_size(LOAD_ADDRESS);

  int result;
#if !defined(__NO_ELF_OBJ)
  if (out_obj) {
    result = output_obj(ofn, &label_table, unresolved);
  } else
#endif
  {
    result = output_exe(ofn, &label_table);
  }

  return result;
}
#endif
#include "asm_x86.h"

#include <assert.h>
#include <stdlib.h>  // exit
#include <string.h>  // memcpy

#include "inst.h"
#include "parse_asm.h"
#include "util.h"

#define ARRAY_SIZE(array)  (sizeof(array) / sizeof(*(array)))

#ifndef PUT_CODE
#define PUT_CODE(p, ...)  do { unsigned char buf[] = {__VA_ARGS__}; memcpy(p, buf, sizeof(buf)); } while (0)
#endif

#ifndef __NO_FLONUM
static unsigned char *put_code_filtered(unsigned char *p, const short *buf, size_t count) {
  for (size_t i = 0; i < count; ++i) {
    short c = *buf++;
    if (c >= 0)
      *p++ = c;
  }
  return p;
}
#endif

void make_code(Inst *inst, Code *code, unsigned char *buf, int len) {
  assert(len <= (int)sizeof(code->buf));
  code->inst = inst;
  code->len = len;
  memcpy(code->buf, buf, len);
}

static char opr_regno(const Reg *reg) {
  return reg->no | (reg->x << 3);
}

static bool opr_reg8(const Reg *reg) {
  assert(reg->size == REG8);
  return opr_regno(reg) < 4;
}

static bool assemble_error(const ParseInfo *info, const char *message) {
  parse_error(info, message);
  return false;
}

static unsigned char *put_rex0(unsigned char *p, enum RegSize size, int sno, int dno,
                               unsigned char opcode) {
  if (size == REG16)
    *p++ = 0x66;
  if (sno >= 8 || dno >= 8 ||
      (size == REG8 && (sno >= 4 || dno >= 4)) ||
      size == REG64)
    *p++ = 0x40 | ((dno & 8) >> 3) | ((sno & 8) >> 1) | (size != REG64 ? 0 : 8);
  *p++ = opcode;
  return p;
}

static unsigned char *put_rex1(unsigned char *p, enum RegSize size, int rex_prefix, int dno,
                               unsigned char opcode) {
  p = put_rex0(p, size, 0, dno, opcode);
  *p++ = rex_prefix | (dno & 7);
  return p;
}

static unsigned char *put_rex2(unsigned char *p, enum RegSize size, int sno, int dno,
                               unsigned char opcode) {
  p = put_rex0(p, size, sno, dno, opcode);
  *p++ = 0xc0 | ((sno & 7) << 3) | (dno & 7);
  return p;
}

static unsigned char *put_rex_indirect(
    unsigned char *p, enum RegSize size, int reg, int indirect_reg,
    unsigned char opcode, unsigned char op2, long offset)
{
  p = put_rex0(p, size, reg, indirect_reg,
               size == REG8 ? opcode : (unsigned char)(opcode + 1));
  int d = reg & 7;
  int s = indirect_reg & 7;
  unsigned char code = (offset == 0 && s != RBP - RAX) ? op2 : is_im8(offset) ? (unsigned char)0x40 : (unsigned char)0x80;
  *p++ = code | s | (d << 3);
  if (s == RSP - RAX)
    *p++ = 0x24;

  if (offset == 0 && s != RBP - RAX) {
    ;
  } else if (is_im8(offset)) {
    *p++ = IM8(offset);
  } else if (is_im32(offset)) {
    PUT_CODE(p, IM32(offset));
    p += 4;
  }
  return p;
}

static unsigned char *put_rex_indirect_with_index(
    unsigned char *p, int base_reg, int index_reg, int dst_reg,
    unsigned char opcode, unsigned char op2, long offset, int scale)
{
  *p++ = 0x48 | ((base_reg & 8) >> 3) | ((index_reg & 8) >> 2) | ((dst_reg & 8) >> 1);
  *p++ = opcode | 1;

  int b = base_reg & 7;
  int i = index_reg & 7;
  int d = dst_reg & 7;

  static const char kScaleTable[] = {-1, 0, 1, -1, 2, -1, -1, -1, 3};
  char scale_bit = kScaleTable[scale];

  unsigned char code = (offset == 0 && b != RBP - RAX) ? op2 : is_im8(offset) ? (unsigned char)0x40 : (unsigned char)0x80;
  *p++ = code | (d << 3) | 0x04;
  *p++ = b | (i << 3) | (scale_bit << 6);
  //if (b == RSP - RAX)
  //  *p++ = 0x24;

  if (offset == 0 && b != RBP - RAX) {
    ;
  } else if (is_im8(offset)) {
    *p++ = IM8(offset);
  } else if (is_im32(offset)) {
    PUT_CODE(p, IM32(offset));
    p += 4;
  }
  return p;
}

static unsigned char *put_rex_imm_indirect(
    unsigned char *p, enum RegSize size, int reg, int indirect_reg,
    unsigned char opcode, unsigned char op2, long value, long offset)
{
  p = put_rex_indirect(p, size, reg, indirect_reg, opcode, op2, offset);
  if (is_im8(value)) {
    *p++ = IM8(value);
  } else {
    PUT_CODE(p, IM32(value));
    p += 4;
  }
  return p;
}

static bool assemble_mov(Inst *inst, const ParseInfo *info, Code *code) {
  unsigned char *p = code->buf;

  if (inst->src.type == REG && inst->dst.type == REG) {
    if (inst->dst.reg.size != inst->src.reg.size)
      return assemble_error(info, "Different source and destination register size");

    enum RegSize size = inst->src.reg.size;
    p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
                 size == REG8 ? 0x88 : 0x89);
  } else if (inst->src.type == IMMEDIATE && inst->dst.type == REG) {
    enum RegSize size = inst->dst.reg.size;
    if (size == REG64 && is_im32(inst->src.immediate)) {
      int d = inst->dst.reg.no;
      int pre = !inst->dst.reg.x ? 0x48 : 0x49;
      MAKE_CODE(inst, code, pre, 0xc7, 0xc0 | d, IM32(inst->src.immediate));
      return true;
    }

    p = put_rex0(p, size, 0, opr_regno(&inst->dst.reg),
                 0xb0 | (size == REG8 ? 0 : 8) | inst->dst.reg.no);
    switch (size) {
    case REG8:  PUT_CODE(p, IM8(inst->src.immediate)); break;
    case REG16: PUT_CODE(p, IM16(inst->src.immediate)); break;
    case REG32: PUT_CODE(p, IM32(inst->src.immediate)); break;
    case REG64: PUT_CODE(p, IM64(inst->src.immediate)); break;
    default: assert(false); break;
    }
    p += 1 << size;
  } else if (inst->src.type == INDIRECT && inst->dst.type == REG) {
    if (inst->src.indirect.offset->kind == EX_FIXNUM) {
      if (inst->src.indirect.reg.no != RIP) {
        long offset = inst->src.indirect.offset->fixnum;
        enum RegSize size = inst->dst.reg.size;
        p = put_rex_indirect(
            p, size,
            opr_regno(&inst->dst.reg),
            opr_regno(&inst->src.indirect.reg),
            0x8a, 0x00, offset);
      }
    }
  } else if (inst->src.type == REG && inst->dst.type == INDIRECT) {
    if (inst->dst.indirect.offset->kind == EX_FIXNUM) {
      if (inst->dst.indirect.reg.no != RIP) {
        long offset = inst->dst.indirect.offset->fixnum;
        enum RegSize size = inst->src.reg.size;
        p = put_rex_indirect(
            p, size,
            opr_regno(&inst->src.reg),
            opr_regno(&inst->dst.indirect.reg),
            0x88, 0x00, offset);
      }
    }
  }

  if (p > code->buf) {
    code->inst = inst;
    code->len = p - code->buf;
    assert((size_t)code->len <= sizeof(code->buf));
    return true;
  }

  return assemble_error(info, "Illegal operand");
}

#ifndef __NO_FLONUM
static unsigned char *assemble_movsd(Inst *inst, Code *code, bool single) {
  unsigned char *p = code->buf;
  unsigned char prefix = single ? 0xf3 : 0xf2;
  if (inst->src.type == REG_XMM && inst->dst.type == REG_XMM) {
    unsigned char sno = inst->src.regxmm - XMM0;
    unsigned char dno = inst->dst.regxmm - XMM0;
    short buf[] = {
      prefix,
      sno >= 8 || dno >= 8 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) : -1,
      0x0f,
      0x10,
      (unsigned char)0xc0 | ((dno & 7) << 3) | (sno & 7),
    };
    p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  } else if (inst->src.type == INDIRECT && inst->dst.type == REG_XMM) {
    if (inst->src.indirect.offset->kind == EX_FIXNUM) {
      if (inst->src.indirect.reg.no != RIP) {
        long offset = inst->src.indirect.offset->fixnum;
        unsigned char sno = opr_regno(&inst->src.indirect.reg);
        unsigned char dno = inst->dst.regxmm - XMM0;
        int d = dno & 7;
        int s = sno & 7;
        unsigned char code = (offset == 0 && s != RBP - RAX) ? (unsigned char)0x00 : is_im8(offset) ? (unsigned char)0x40 : (unsigned char)0x80;

        short buf[] = {
          prefix,
          sno >= 8 || dno >= 8 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) : -1,
          0x0f,
          0x10,
          code | s | (d << 3),
          s == RSP - RAX ? 0x24 : -1,
        };
        p = put_code_filtered(p, buf, ARRAY_SIZE(buf));

        if (offset == 0 && s != RBP - RAX) {
          ;
        } else if (is_im8(offset)) {
          *p++ = IM8(offset);
        } else if (is_im32(offset)) {
          PUT_CODE(p, IM32(offset));
          p += 4;
        }
      }
    }
  } else if (inst->src.type == REG_XMM && inst->dst.type == INDIRECT) {
    if (inst->dst.indirect.offset->kind == EX_FIXNUM) {
      if (inst->dst.indirect.reg.no != RIP) {
        long offset = inst->dst.indirect.offset->fixnum;
        unsigned char sno = inst->src.regxmm - XMM0;
        unsigned char dno = opr_regno(&inst->dst.indirect.reg);
        int d = dno & 7;
        int s = sno & 7;
        unsigned char code = (offset == 0 && d != RBP - RAX) ? (unsigned char)0x00 : is_im8(offset) ? (unsigned char)0x40 : (unsigned char)0x80;

        short buf[] = {
          prefix,
          sno >= 8 || dno >= 8 ? (unsigned char)0x40 | ((dno & 8) >> 3) | ((sno & 8) >> 1) : -1,
          0x0f,
          0x11,
          code | d | (s << 3),
          d == RSP - RAX ? 0x24 : -1,
        };
        p = put_code_filtered(p, buf, ARRAY_SIZE(buf));

        if (offset == 0 && d != RBP - RAX) {
          ;
        } else if (is_im8(offset)) {
          *p++ = IM8(offset);
        } else if (is_im32(offset)) {
          PUT_CODE(p, IM32(offset));
          p += 4;
        }
      }
    }
  }

  return p;
}

static unsigned char *assemble_bop_sd(Inst *inst, Code *code, bool single, unsigned char op) {
  unsigned char *p = code->buf;
  unsigned char prefix = single ? 0xf3 : 0xf2;
  if (inst->src.type == REG_XMM && inst->dst.type == REG_XMM) {
    unsigned char sno = inst->src.regxmm - XMM0;
    unsigned char dno = inst->dst.regxmm - XMM0;
    short buf[] = {
      prefix,
      sno >= 8 || dno >= 8 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) : -1,
      0x0f,
      op,
      (unsigned char)0xc0 | ((dno & 7) << 3) | (sno & 7),
    };
    p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  }

  return p;
}

static unsigned char *assemble_ucomisd(Inst *inst, Code *code, bool single) {
  unsigned char *p = code->buf;
  if (inst->src.type == REG_XMM && inst->dst.type == REG_XMM) {
    unsigned char sno = inst->src.regxmm - XMM0;
    unsigned char dno = inst->dst.regxmm - XMM0;
    short buf[] = {
      single ? -1 : 0x66,
      sno >= 8 || dno >= 8 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) : -1,
      0x0f,
      0x2e,
      (unsigned char)0xc0 | ((dno & 7) << 3) | (sno & 7),
    };
    p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  }
  return p;
}

static unsigned char *assemble_cvtsi2sd(Inst *inst, Code *code, bool single) {
  unsigned char *p = code->buf;
  unsigned char prefix = single ? 0xf3 : 0xf2;
  if (inst->src.type == REG && inst->dst.type == REG_XMM) {
    unsigned char sno = opr_regno(&inst->src.reg);
    unsigned char dno = inst->dst.regxmm - XMM0;
    short buf[] = {
      prefix,
      sno >= 8 || dno >= 8 || inst->src.reg.size == REG64 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) | (inst->src.reg.size == REG64 ? 8 : 0) : -1,
      0x0f,
      0x2a,
      (unsigned char)0xc0 | ((dno & 7) << 3) | (sno & 7),
    };
    p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  }
  return p;
}

static unsigned char *assemble_cvttsd2si(Inst *inst, Code *code, bool single) {
  unsigned char *p = code->buf;
  unsigned char prefix = single ? 0xf3 : 0xf2;
  if (inst->src.type == REG_XMM && inst->dst.type == REG) {
    unsigned char sno = inst->src.regxmm - XMM0;
    unsigned char dno = opr_regno(&inst->dst.reg);
    short buf[] = {
      prefix,
      sno >= 8 || dno >= 8 || inst->dst.reg.size == REG64 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) | (inst->dst.reg.size == REG64 ? 8 : 0) : -1,
      0x0f,
      0x2c,
      (unsigned char)0xc0 | ((dno & 7) << 3) | (sno & 7),
    };
    p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  }
  return p;
}

static unsigned char *assemble_cvtsd2ss(Inst *inst, Code *code, bool single) {
  unsigned char *p = code->buf;
  unsigned char prefix = single ? 0xf3 : 0xf2;
  if (inst->src.type == REG_XMM && inst->dst.type == REG_XMM) {
    unsigned char sno = inst->src.regxmm - XMM0;
    unsigned char dno = inst->dst.regxmm - XMM0;
    short buf[] = {
      prefix,
      sno >= 8 || dno >= 8 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) : -1,
      0x0f,
      0x5a,
      (unsigned char)0xc0 | ((dno & 7) << 3) | (sno & 7),
    };
    p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
  }
  return p;
}
#endif

bool assemble_inst(Inst *inst, const ParseInfo *info, Code *code) {
  unsigned char *p = code->buf;

  code->flag = 0;
  code->len = 0;

  switch(inst->op) {
  case NOOP:
    return true;
  case MOV:
    return assemble_mov(inst, info, code);
  case MOVSX:
  case MOVZX:
    if (inst->src.type == REG && inst->dst.type == REG) {
      int s = inst->src.reg.no;
      int d = inst->dst.reg.no;
      unsigned char op = inst->op == MOVZX ? 0xb6 : 0xbe;
      switch (inst->src.reg.size) {
      case REG8:
        switch (inst->dst.reg.size) {
        case REG16:
          if (opr_reg8(&inst->src.reg) && !inst->dst.reg.x) {
            MAKE_CODE(inst, code, 0x66, 0x0f, op, 0xc0 + s + d * 8);
          } else {
            int pre = (!inst->src.reg.x ? 0x40 : 0x41) + (!inst->dst.reg.x ? 0 : 4);
            MAKE_CODE(inst, code, 0x66, pre, 0x0f, op, 0xc0 + s + d * 8);
          }
          return true;
        case REG32:
          if (opr_reg8(&inst->src.reg) && !inst->dst.reg.x) {
            MAKE_CODE(inst, code, 0x0f, op, 0xc0 + s + d * 8);
          } else {
            int pre = (!inst->src.reg.x ? 0x40 : 0x41) + (!inst->dst.reg.x ? 0 : 4);
            MAKE_CODE(inst, code, pre, 0x0f, op, 0xc0 + s + d * 8);
          }
          return true;
        case REG64:
          {
            int pre = (!inst->src.reg.x ? 0x48 : 0x49) + (!inst->dst.reg.x ? 0 : 4);
            MAKE_CODE(inst, code, pre, 0x0f, op, 0xc0 + s + d * 8);
            return true;
          }
        default:
          break;
        }
        break;
      case REG16:
        switch (inst->dst.reg.size) {
        case REG32:
          if (!inst->src.reg.x && !inst->dst.reg.x) {
            MAKE_CODE(inst, code, 0x0f, op | 1, 0xc0 + s + d * 8);
          } else {
            int pre = (!inst->src.reg.x ? 0x40 : 0x41) + (!inst->dst.reg.x ? 0 : 4);
            MAKE_CODE(inst, code, pre, 0x0f, op | 1, 0xc0 + s + d * 8);
          }
          return true;
        case REG64:
          {
            int pre = (!inst->src.reg.x ? 0x48 : 0x49) + (!inst->dst.reg.x ? 0 : 4);
            MAKE_CODE(inst, code, pre, 0x0f, op | 1, 0xc0 + s + d * 8);
            return true;
          }
        default:
          break;
        }
        break;
      case REG32:
        // "MOVZX %32bit, %64bit" doesn't exist!
        if (inst->dst.reg.size == REG64 && inst->op == MOVSX) {
          int pre = (!inst->src.reg.x ? 0x48 : 0x49) + (!inst->dst.reg.x ? 0 : 4);
          MAKE_CODE(inst, code, pre, 0x63, 0xc0 + s + d * 8);
          return true;
        }
        break;
      default:
        break;
      }
      return assemble_error(info, "Illegal operand");
    }
    break;
  case LEA:
    if (inst->src.type == INDIRECT && inst->dst.type == REG) {
      if (inst->dst.reg.size != REG64)
        return assemble_error(info, "64 bit register expected for destination");

      assert(inst->src.indirect.offset != NULL);
      if (inst->src.indirect.reg.no != RIP) {
        if (inst->src.indirect.offset->kind == EX_FIXNUM) {
          long offset = inst->src.indirect.offset->fixnum;
          enum RegSize size = inst->dst.reg.size;
          p = put_rex_indirect(
              p, size,
              opr_regno(&inst->dst.reg),
              opr_regno(&inst->src.indirect.reg),
              0x8c, 0x00, offset);
        }
      } else {
        if (inst->src.indirect.offset->kind != EX_FIXNUM) {
          int pre = !inst->dst.reg.x ? 0x48 : 0x4c;
          int d = inst->dst.reg.no;
          MAKE_CODE(inst, code, pre, 0x8d, 0x05 | (d << 3), IM32(0));
          return true;
        }
      }
    } else if (inst->src.type == INDIRECT_WITH_INDEX && inst->dst.type == REG) {
      if (inst->dst.reg.size != REG64)
        return assemble_error(info, "64 bit register expected for destination");

      Expr *offset_expr = inst->src.indirect_with_index.offset;
      Expr *scale_expr = inst->src.indirect_with_index.scale;
      if ((offset_expr == NULL || offset_expr->kind == EX_FIXNUM) &&
          (scale_expr == NULL || scale_expr->kind == EX_FIXNUM)) {
        long offset = offset_expr != NULL ? offset_expr->fixnum : 0;
        long scale = scale_expr != NULL ? scale_expr->fixnum : 1;
        if (is_im32(offset) && 1 <= scale && scale <= 8 && IS_POWER_OF_2(scale)) {
          const Reg *base_reg = &inst->src.indirect_with_index.base_reg;
          const Reg *index_reg = &inst->src.indirect_with_index.index_reg;
          p = put_rex_indirect_with_index(
              p,
              opr_regno(base_reg),
              opr_regno(index_reg),
              opr_regno(&inst->dst.reg),
              0x8c, 0x00, offset, scale);
        }
      }
    }
    break;
  case ADD:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.reg.size != inst->src.reg.size)
        return assemble_error(info, "Different source and destination register size");

      enum RegSize size = inst->src.reg.size;
      p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x00 : 0x01);
    } else if (inst->src.type == IMMEDIATE && inst->dst.type == REG) {
      long value = inst->src.immediate;
      if (is_im32(value)) {
        bool im8 = is_im8(value);
        enum RegSize size = inst->dst.reg.size;
        int d = opr_regno(&inst->dst.reg);
        if (d == RAX - RAX && (size == REG8 || !is_im8(value)))
          p = put_rex0(p, size, 0, d, im8 ? 0x04 : 0x05);
        else
          p = put_rex1(p, size, 0xc0, d, im8 ? 0x83 : 0x81);

        if (im8) {
          *p++ = IM8(value);
        } else {
          switch (size) {
          case REG8:   *p++ = IM8(value); break;
          case REG16:  PUT_CODE(p, IM16(value)); p += 2; break;
          case REG32:
          case REG64:
            PUT_CODE(p, IM32(value));
            p += 4;
            break;
          default:  assert(false); break;
          }
        }
      }
    } else if (inst->src.type == INDIRECT && inst->dst.type == REG) {
      if (inst->src.indirect.offset->kind == EX_FIXNUM &&
          inst->src.indirect.reg.no != RIP) {
        long offset = inst->src.indirect.offset->fixnum;
        enum RegSize size = inst->dst.reg.size;
        p = put_rex_indirect(
            p, size,
            opr_regno(&inst->dst.reg),
            opr_regno(&inst->src.indirect.reg),
            0x02, 0x00, offset);
      }
    }
    break;
  case ADDQ:
    if (inst->src.type == IMMEDIATE && inst->dst.type == INDIRECT) {
      if (inst->dst.indirect.offset->kind == EX_FIXNUM) {
        long value = inst->src.immediate;
        if (is_im32(value)) {
          long offset = inst->dst.indirect.offset->fixnum;
          enum RegSize size = REG64;  // caz ADDQ
          p = put_rex_imm_indirect(
              p, size,
              0, opr_regno(&inst->dst.indirect.reg),
              (is_im8(value) ? 0x83 : 0x81), 0x00, value, offset);
        }
      }
    }
    break;
  case SUB:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.reg.size != inst->src.reg.size)
        return assemble_error(info, "Different source and destination register size");

      enum RegSize size = inst->src.reg.size;
      p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x28 : 0x29);
    } else if (inst->src.type == IMMEDIATE && inst->dst.type == REG) {
      long value = inst->src.immediate;
      if (is_im32(value)) {
        bool im8 = is_im8(value);
        enum RegSize size = inst->dst.reg.size;
        int d = opr_regno(&inst->dst.reg);
        if (d == RAX - RAX && (size == REG8 || !is_im8(value)))
          p = put_rex0(p, size, 0, d, im8 ? 0x2c : 0x2d);
        else
          p = put_rex1(p, size, 0xe8, d, im8 ? 0x83 : 0x81);

        if (im8) {
          *p++ = IM8(value);
        } else {
          switch (size) {
          case REG8:   *p++ = IM8(value); break;
          case REG16:  PUT_CODE(p, IM16(value)); p += 2; break;
          case REG32:
          case REG64:
            PUT_CODE(p, IM32(value));
            p += 4;
            break;
          default:  assert(false); break;
          }
        }
      }
    }
    break;
  case SUBQ:
    if (inst->src.type == IMMEDIATE && inst->dst.type == INDIRECT) {
      if (inst->dst.indirect.offset->kind == EX_FIXNUM) {
        long value = inst->src.immediate;
        if (is_im32(value)) {
          long offset = inst->dst.indirect.offset->fixnum;
          enum RegSize size = REG64;  // caz SUBQ
          p = put_rex_imm_indirect(
              p, size,
              0, opr_regno(&inst->dst.indirect.reg),
              (is_im8(value) ? 0x83 : 0x81), 0x28, value, offset);
        }
      }
    }
    break;
  case MUL:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    if (inst->src.type == REG) {
      enum RegSize size = inst->src.reg.size;
      p = put_rex0(p, size, 0, opr_regno(&inst->src.reg),
                   0xf6 | (size == REG8 ? 0 : 1));
      *p++ = 0xe0 | inst->src.reg.no;
    }
    break;
  case DIV:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    if (inst->src.type == REG) {
      enum RegSize size = inst->src.reg.size;
      p = put_rex0(p, size, 0, opr_regno(&inst->src.reg),
                   0xf6 | (size == REG8 ? 0 : 1));
      *p++ = 0xf0 | inst->src.reg.no;
    }
    break;
  case IDIV:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    if (inst->src.type == REG) {
      enum RegSize size = inst->src.reg.size;
      p = put_rex0(p, size, 0, opr_regno(&inst->src.reg),
                   0xf6 | (size == REG8 ? 0 : 1));
      *p++ = 0xf8 | inst->src.reg.no;
    }
    break;
  case NEG:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    if (inst->src.type == REG) {
      p = put_rex1(p, inst->src.reg.size,
                   0xd8, opr_regno(&inst->src.reg), 0xf7);
    }
    break;
  case NOT:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    if (inst->src.type == REG) {
      p = put_rex1(p, inst->src.reg.size,
                   0xd0, opr_regno(&inst->src.reg), 0xf7);
    }
    break;
  case INC:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    if (inst->src.type == REG) {
      p = put_rex1(p, inst->src.reg.size,
                   0xc0, opr_regno(&inst->src.reg), 0xff);
    }
    break;
  case INCB:
  case INCW:
  case INCL:
  case INCQ:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    if (inst->src.type == INDIRECT &&
        inst->src.indirect.reg.no != RIP) {
      enum RegSize size = inst->op + (REG8 - INCB);
      long offset = inst->src.indirect.offset->fixnum;
      p = put_rex_indirect(
          p, size,
          0, opr_regno(&inst->src.indirect.reg),
          0xfe, 0x00, offset);
    }
    break;
  case DEC:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    if (inst->src.type == REG) {
      p = put_rex1(p, inst->src.reg.size,
                   0xc8, opr_regno(&inst->src.reg), 0xff);
    }
    break;
  case DECB:
  case DECW:
  case DECL:
  case DECQ:
    if (inst->src.type == NOOPERAND)
      return assemble_error(info, "Illegal operand");

    if (inst->src.type == INDIRECT &&
        inst->src.indirect.reg.no != RIP) {
      enum RegSize size = inst->op + (REG8 - DECB);
      long offset = inst->src.indirect.offset->fixnum;
      p = put_rex_indirect(
          p, size,
          1, opr_regno(&inst->src.indirect.reg),
          0xfe, 0x08, offset);
    }
    break;
  case AND:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.reg.size != inst->src.reg.size)
        return assemble_error(info, "Different source and destination register size");

      enum RegSize size = inst->dst.reg.size;
      p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x20 : 0x21);
    } else if (inst->src.type == IMMEDIATE && inst->dst.type == REG) {
      long value = inst->src.immediate;
      enum RegSize size = inst->dst.reg.size;
      if (is_im8(value) && (size != REG8 || opr_regno(&inst->dst.reg) != AL - AL)) {
        p = put_rex1(p, size,
                     0xe0, opr_regno(&inst->dst.reg),
                     0x83);
        *p++ = IM8(value);
      } else if (size <= REG32 || is_im32(value)) {
        if (opr_regno(&inst->dst.reg) == RAX - RAX) {
          p = put_rex0(p, size,
                       0, opr_regno(&inst->dst.reg),
                       size == REG8 ? 0x24 : 0x25);
        } else {
          p = put_rex1(p, size,
                       0xe0, opr_regno(&inst->dst.reg),
                       0x81);
        }

        switch (size) {
        case REG8:  PUT_CODE(p, IM8(value)); p += 1; break;
        case REG16: PUT_CODE(p, IM16(value)); p += 2; break;
        case REG32: case REG64:
          PUT_CODE(p, IM32(value));
          p += 4;
          break;
        default: assert(false); break;
        }
      } else {
        return assemble_error(info, "Too large constant");
      }
    }
    break;
  case OR:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.reg.size != inst->src.reg.size)
        return assemble_error(info, "Different source and destination register size");

      enum RegSize size = inst->dst.reg.size;
      p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x08 : 0x09);
    } else if (inst->src.type == IMMEDIATE && inst->dst.type == REG) {
      long value = inst->src.immediate;
      enum RegSize size = inst->dst.reg.size;
      if (is_im8(value) && (size != REG8 || opr_regno(&inst->dst.reg) != AL - AL)) {
        p = put_rex1(p, size,
                     0xc8, opr_regno(&inst->dst.reg),
                     0x83);
        *p++ = IM8(value);
      } else if (size <= REG32 || is_im32(value)) {
        if (opr_regno(&inst->dst.reg) == RAX - RAX) {
          p = put_rex0(p, size,
                       0, opr_regno(&inst->dst.reg),
                       size == REG8 ? 0x0c : 0x0d);
        } else {
          p = put_rex1(p, size,
                       0xc8, opr_regno(&inst->dst.reg),
                       0x81);
        }
        switch (size) {
        case REG8:  PUT_CODE(p, IM8(value)); p += 1; break;
        case REG16: PUT_CODE(p, IM16(value)); p += 2; break;
        case REG32: case REG64:
          PUT_CODE(p, IM32(value));
          p += 4;
          break;
        default: assert(false); break;
        }
      } else {
        return assemble_error(info, "Too large constant");
      }
    }
    break;
  case XOR:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.reg.size != inst->src.reg.size)
        return assemble_error(info, "Different source and destination register size");

      enum RegSize size = inst->dst.reg.size;
      p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x30 : 0x31);
    } else if (inst->src.type == IMMEDIATE && inst->dst.type == REG) {
      long value = inst->src.immediate;
      enum RegSize size = inst->dst.reg.size;
      if (is_im8(value) && (size != REG8 || opr_regno(&inst->dst.reg) != AL - AL)) {
        p = put_rex1(p, size,
                     0xf0, opr_regno(&inst->dst.reg),
                     0x83);
        *p++ = IM8(value);
      } else if (size <= REG32 || is_im32(value)) {
        if (opr_regno(&inst->dst.reg) == RAX - RAX) {
          p = put_rex0(p, size,
                       0, opr_regno(&inst->dst.reg),
                       size == REG8 ? 0x34 : 0x35);
        } else {
          p = put_rex1(p, size,
                       0xf0, opr_regno(&inst->dst.reg),
                       0x81);
        }
        switch (size) {
        case REG8:  PUT_CODE(p, IM8(value)); p += 1; break;
        case REG16: PUT_CODE(p, IM16(value)); p += 2; break;
        case REG32: case REG64:
          PUT_CODE(p, IM32(value));
          p += 4;
          break;
        default: assert(false); break;
        }
      } else {
        return assemble_error(info, "Too large constant");
      }
    }
    break;
  case SHL:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (opr_regno(&inst->src.reg) != CL - AL)
        return assemble_error(info, "`%cl` expected");

      enum RegSize size = inst->dst.reg.size;
      p = put_rex1(p, size, 0xe0, opr_regno(&inst->dst.reg),
                   size == REG8 ? 0xd2 : 0xd3);
    } else if (inst->src.type == IMMEDIATE && inst->dst.type == REG) {
      enum RegSize size = inst->dst.reg.size;
      if (inst->src.immediate == 1) {
        p = put_rex1(p, size, 0xe0, opr_regno(&inst->dst.reg),
                     size == REG8 ? 0xd0 : 0xd1);
      } else {
        p = put_rex1(p, size, 0xe0, opr_regno(&inst->dst.reg),
                     size == REG8 ? 0xc0 : 0xc1);
        *p++ = IM8(inst->src.immediate);
      }
    }
    break;
  case SHR:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (opr_regno(&inst->src.reg) != CL - AL)
        return assemble_error(info, "`%cl` expected");

      enum RegSize size = inst->dst.reg.size;
      p = put_rex1(p, size, 0xe8, opr_regno(&inst->dst.reg),
                   size == REG8 ? 0xd2 : 0xd3);
    } else if (inst->src.type == IMMEDIATE && inst->dst.type == REG) {
      enum RegSize size = inst->dst.reg.size;
      if (inst->src.immediate == 1) {
        p = put_rex1(p, size, 0xe8, opr_regno(&inst->dst.reg),
                     size == REG8 ? 0xd0 : 0xd1);
      } else {
        p = put_rex1(p, size, 0xe8, opr_regno(&inst->dst.reg),
                     size == REG8 ? 0xc0 : 0xc1);
        *p++ = IM8(inst->src.immediate);
      }
    }
    break;
  case SAR:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (opr_regno(&inst->src.reg) != CL - AL)
        return assemble_error(info, "`%cl` expected");

      enum RegSize size = inst->dst.reg.size;
      p = put_rex1(p, size, 0xf8, opr_regno(&inst->dst.reg),
                   size == REG8 ? 0xd2 : 0xd3);
    } else if (inst->src.type == IMMEDIATE && inst->dst.type == REG) {
      enum RegSize size = inst->dst.reg.size;
      if (inst->src.immediate == 1) {
        p = put_rex1(p, size, 0xf8, opr_regno(&inst->dst.reg),
                     size == REG8 ? 0xd0 : 0xd1);
      } else {
        p = put_rex1(p, size, 0xf8, opr_regno(&inst->dst.reg),
                     size == REG8 ? 0xc0 : 0xc1);
        *p++ = IM8(inst->src.immediate);
      }
    }
    break;
  case CMP:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.reg.size != inst->src.reg.size)
        return assemble_error(info, "Different source and destination register size");

      enum RegSize size = inst->src.reg.size;
      p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x38 : 0x39);
    } else if (inst->src.type == IMMEDIATE && inst->dst.type == REG) {
      long value = inst->src.immediate;
      if (is_im32(value)) {
        enum RegSize size = inst->dst.reg.size;
        bool im8 = is_im8(value);
        int d = opr_regno(&inst->dst.reg);
        if (d == RAX - RAX && (size == REG8 || !im8))
          p = put_rex0(p, size, 0, d, size == REG8 ? 0x3c : 0x3d);
        else
          p = put_rex1(p, size, 0xf8, d, 0x80 | (size == REG8 ? 0 : im8 ? 3 : 1));

        if (im8) {
          *p++ = IM8(value);
        } else {
          switch (size) {
          case REG8:   *p++ = IM8(value); break;
          case REG16:  PUT_CODE(p, IM16(value)); p += 2; break;
          case REG32:
          case REG64:
            PUT_CODE(p, IM32(value));
            p += 4;
            break;
          }
        }
      }
    }
    break;
  case TEST:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.reg.size != inst->src.reg.size)
        return assemble_error(info, "Different source and destination register size");

      enum RegSize size = inst->src.reg.size;
      p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x84 : 0x85);
    }
    break;
  case CWTL:
    if (inst->src.type != NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    MAKE_CODE(inst, code, 0x98);
    return true;
  case CLTD:
    if (inst->src.type != NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    MAKE_CODE(inst, code, 0x99);
    return true;
  case CQTO:
    if (inst->src.type != NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    MAKE_CODE(inst, code, 0x48, 0x99);
    return true;

  case SETO: case SETNO: case SETB:  case SETAE:
  case SETE: case SETNE: case SETBE: case SETA:
  case SETS: case SETNS: case SETP:  case SETNP:
  case SETL: case SETGE: case SETLE: case SETG:
    if (inst->dst.type != NOOPERAND || inst->src.type != REG ||
        inst->src.reg.size != REG8)
      return assemble_error(info, "Illegal opeand");

    p = put_rex0(p, REG8, 0, opr_regno(&inst->src.reg), 0x0f);
    *p++ = 0x90 | (inst->op - SETO);
    *p++ = 0xc0 | inst->src.reg.no;
    break;
  case PUSH:
    if (inst->dst.type == NOOPERAND) {
      if (inst->src.type == REG && inst->src.reg.size == REG64) {
        p = put_rex0(p, REG32, 0, opr_regno(&inst->src.reg),
                     0x50 | inst->src.reg.no);
        break;
      } else if (inst->src.type == IMMEDIATE) {
        long value = inst->src.immediate;
        if (is_im8(value)) {
          *p++ = 0x6a;
          *p++ = IM8(value);
          break;
        } else if (is_im32(value)) {
          *p++ = 0x68;
          PUT_CODE(p, IM32(value));
          p += 4;
          break;
        }
      }
    }
    return assemble_error(info, "Illegal operand");
  case POP:
    if (inst->dst.type != NOOPERAND || inst->src.type != REG ||
        inst->src.reg.size != REG64)
      return assemble_error(info, "Illegal operand");

    p = put_rex0(p, REG32, 0, opr_regno(&inst->src.reg),
                 0x58 | inst->src.reg.no);
    break;
  case JMP:
    if (inst->src.type != DIRECT || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");
    //MAKE_CODE(inst, code, 0xe9, IM32(0));
    MAKE_CODE(inst, code, 0xeb, IM8(0));  // Short jmp in default.
    return true;
  case JO: case JNO: case JB:  case JAE:
  case JE: case JNE: case JBE: case JA:
  case JS: case JNS: case JP:  case JNP:
  case JL: case JGE: case JLE: case JG:
    if (inst->src.type != DIRECT || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    //MAKE_CODE(inst, code, 0x0f, 0x80 + (inst->op - JO), IM32(0));
    MAKE_CODE(inst, code, 0x70 + (inst->op - JO), IM8(0));  // Short jmp in default.
    return true;
  case CALL:
    if (inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    if (inst->src.type == DIRECT) {
      MAKE_CODE(inst, code, 0xe8, IM32(0));
      return true;
    } else if (inst->src.type == DEREF_REG) {
      int s = inst->src.deref_reg.no;
      if (!inst->src.deref_reg.x) {
        MAKE_CODE(inst, code, 0xff, 0xd0 + s);
      } else {
        MAKE_CODE(inst, code, 0x41, 0xff, 0xd0 + s);
      }
      return true;
    }
    break;
  case RET:
    if (inst->src.type != NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    MAKE_CODE(inst, code, 0xc3);
    return true;
  case INT:
    if (inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    if (inst->src.type == IMMEDIATE) {
      long value = inst->src.immediate;
      MAKE_CODE(inst, code, 0xcd, IM8(value));
      return true;
    }
    return true;
  case SYSCALL:
    if (inst->src.type != NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(info, "Illegal operand");

    MAKE_CODE(inst, code, 0x0f, 0x05);
    return true;
#ifndef __NO_FLONUM
  case MOVSD:
    p = assemble_movsd(inst, code, false);
    break;
  case ADDSD:
    p = assemble_bop_sd(inst, code, false, 0x58);
    break;
  case SUBSD:
    p = assemble_bop_sd(inst, code, false, 0x5c);
    break;
  case MULSD:
    p = assemble_bop_sd(inst, code, false, 0x59);
    break;
  case DIVSD:
    p = assemble_bop_sd(inst, code, false, 0x5e);
    break;
  case UCOMISD:
    p = assemble_ucomisd(inst, code, false);
    break;
  case CVTSI2SD:
    p = assemble_cvtsi2sd(inst, code, false);
    break;
  case CVTTSD2SI:
    p = assemble_cvttsd2si(inst, code, false);
    break;
  case SQRTSD:
    if (inst->src.type == REG_XMM && inst->dst.type == REG_XMM) {
      unsigned char sno = inst->src.regxmm - XMM0;
      unsigned char dno = inst->dst.regxmm - XMM0;
      short buf[] = {
        0xf2,
        sno >= 8 || dno >= 8 ? (unsigned char)0x40 | ((sno & 8) >> 3) | ((dno & 8) >> 1) : -1,
        0x0f,
        0x51,
        (unsigned char)0xc0 | ((dno & 7) << 3) | (sno & 7),
      };
      p = put_code_filtered(p, buf, ARRAY_SIZE(buf));
    }
    break;

  case MOVSS:
    p = assemble_movsd(inst, code, true);
    break;
  case ADDSS:
    p = assemble_bop_sd(inst, code, true, 0x58);
    break;
  case SUBSS:
    p = assemble_bop_sd(inst, code, true, 0x5c);
    break;
  case MULSS:
    p = assemble_bop_sd(inst, code, true, 0x59);
    break;
  case DIVSS:
    p = assemble_bop_sd(inst, code, true, 0x5e);
    break;
  case UCOMISS:
    p = assemble_ucomisd(inst, code, true);
    break;
  case CVTSI2SS:
    p = assemble_cvtsi2sd(inst, code, true);
    break;
  case CVTTSS2SI:
    p = assemble_cvttsd2si(inst, code, true);
    break;

  case CVTSD2SS:
    p = assemble_cvtsd2ss(inst, code, false);
    break;
  case CVTSS2SD:
    p = assemble_cvtsd2ss(inst, code, true);
    break;
#endif
  default:
    break;
  }

  if (p > code->buf) {
    code->inst = inst;
    code->len = p - code->buf;
    assert((size_t)code->len <= sizeof(code->buf));
    return true;
  }

  char buf[64];
  snprintf(buf, sizeof(buf), "op=%2d: not handled", inst->op);
  assemble_error(info, buf);
  return false;
}
#include "gen.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "util.h"

typedef struct {
  uintptr_t start_address;
  Buffer buf;
} Section;

static Section sections[SECTION_COUNT];
static size_t bss_size;

size_t section_aligns[SECTION_COUNT];
uintptr_t section_start_addresses[SECTION_COUNT];

void add_bss(size_t size) {
  bss_size += size;
}

void align_section_size(enum SectionType secno, size_t align) {
  if (secno != SEC_BSS) {
    Section *sec = &sections[secno];
    buf_align(&sec->buf, align);
  } else {
    bss_size = ALIGN(bss_size, align);
  }
}

void add_section_data(enum SectionType secno, const void *data, size_t bytes) {
  assert(secno != SEC_BSS);
  Section *sec = &sections[secno];
  buf_put(&sec->buf, data, bytes);
}

void add_code(const void *buf, size_t bytes) {
  add_section_data(SEC_CODE, buf, bytes);
}

void fix_section_size(uintptr_t start_address) {
  sections[SEC_CODE].start_address = start_address;
  int rodata_align = MAX(section_aligns[SEC_RODATA], 1);
  uintptr_t rodata_addr = ALIGN(start_address + sections[SEC_CODE].buf.size, rodata_align);
  sections[SEC_RODATA].start_address = rodata_addr;

  int data_align = MAX(section_aligns[SEC_DATA], 1);
  sections[SEC_DATA].start_address =
      ALIGN(sections[SEC_RODATA].start_address + sections[SEC_RODATA].buf.size, data_align);
  int bss_align = MAX(section_aligns[SEC_BSS], 1);
  sections[SEC_BSS].start_address =
      sections[SEC_DATA].start_address + ALIGN(sections[SEC_DATA].buf.size, bss_align);
}

void get_section_size(int section, size_t *psize, uintptr_t *ploadadr) {
  switch (section) {
  case SEC_CODE:
  case SEC_RODATA:
  case SEC_DATA:
    {
      const Section *sec = &sections[section];
      if (ploadadr != NULL)
        *ploadadr = sec->start_address;
      *psize = sec->buf.size;
    }
    break;
  case SEC_BSS:
    {
      assert(ploadadr == NULL);
      *psize = bss_size;
    }
    break;
  default:
    assert(!"Illegal");
    break;
  }
}

void output_section(FILE *fp, int section) {
  Section *sec = &sections[section];
  const void *data = sec->buf.data;
  fwrite(data, sec->buf.size, 1, fp);
}
#include "ir_asm.h"

#include <assert.h>
#include <stdlib.h>  // malloc

#include "gen.h"
#include "inst.h"
#include "table.h"
#include "util.h"

#define BYTE_SIZE  (1)
#define WORD_SIZE  (2)
#define LONG_SIZE  (4)
#define QUAD_SIZE  (8)

LabelInfo *new_label(int section, uintptr_t address) {
  LabelInfo *info = malloc(sizeof(*info));
  info->section = section;
  info->flag = 0;
  info->address = address;
  return info;
}

bool add_label_table(Table *label_table, const Name *label, int section, bool define, bool global) {
  LabelInfo *info = table_get(label_table, label);
  if (info != NULL) {
    if (define) {
      if ((info->flag & LF_DEFINED) != 0) {
        fprintf(stderr, "`%.*s' already defined\n", label->bytes, label->chars);
        return false;
      }
      info->address = 1;
      info->section = section;
    }
  } else {
    info = new_label(section, 0);
    table_put(label_table, label, info);
  }
  if (define)
    info->flag |= LF_DEFINED;
  if (global)
    info->flag |= LF_GLOBAL;
  return true;
}

IR *new_ir_label(const Name *label) {
  IR *ir = malloc(sizeof(*ir));
  ir->kind = IR_LABEL;
  ir->label = label;
  return ir;
}

IR *new_ir_code(const Code *code) {
  IR *ir = malloc(sizeof(*ir));
  ir->kind = IR_CODE;
  ir->code = *code;
  return ir;
}

IR *new_ir_data(const void *data, size_t size) {
  IR *ir = malloc(sizeof(*ir));
  ir->kind = IR_DATA;
  ir->data.len = size;
  ir->data.buf = (unsigned char*)data;
  return ir;
}

IR *new_ir_bss(size_t size) {
  IR *ir = malloc(sizeof(*ir));
  ir->kind = IR_BSS;
  ir->bss = size;
  return ir;
}

IR *new_ir_align(int align) {
  IR *ir = malloc(sizeof(*ir));
  ir->kind = IR_ALIGN;
  ir->align = align;
  return ir;
}

IR *new_ir_expr(enum IrKind kind, const Expr *expr) {
  IR *ir = malloc(sizeof(*ir));
  ir->kind = kind;
  ir->expr = expr;
  return ir;
}

static uintptr_t align_next_section(enum SectionType sec, uintptr_t address) {
  size_t align = section_aligns[sec];
  if (align > 1)
    address = ALIGN(address, align);
  return address;
}

bool calc_label_address(uintptr_t start_address, Vector **section_irs, Table *label_table) {
  bool settle = true;
  uintptr_t address = start_address;
  for (int sec = 0; sec < SECTION_COUNT; ++sec) {
    address = align_next_section(sec, address);
    section_start_addresses[sec] = address;

    Vector *irs = section_irs[sec];
    for (int i = 0, len = irs->len; i < len; ++i) {
      IR *ir = irs->data[i];
      ir->address = address;
      switch (ir->kind) {
      case IR_LABEL:
        {
          LabelInfo *info;
          if (!table_try_get(label_table, ir->label, (void**)&info)) {
            fprintf(stderr, "[%.*s] not found\n", ir->label->bytes, ir->label->chars);
            assert(!"Unexpected");
          } else {
            info->address = address;
          }
        }
        break;
      case IR_CODE:
        address += ir->code.len;
        break;
      case IR_DATA:
        address += ir->data.len;
        break;
      case IR_BSS:
        address += ir->bss;
        break;
      case IR_ALIGN:
        ir->address = address = ALIGN(address, ir->align);
        if ((size_t)ir->align > section_aligns[sec]) {
          section_aligns[sec] = ir->align;
          settle = false;
        }
        break;
      case IR_EXPR_BYTE:
        address += BYTE_SIZE;
        break;
      case IR_EXPR_WORD:
        address += WORD_SIZE;
        break;
      case IR_EXPR_LONG:
        address += LONG_SIZE;
        break;
      case IR_EXPR_QUAD:
        address += QUAD_SIZE;
        break;
      default:  assert(false); break;
      }
    }
  }
  return settle;
}

static void put_value(unsigned char *p, intptr_t value, int size) {
  for (int i = 0; i < size; ++i) {
    *p++ = value;
    value >>= 8;
  }
}

static void put_unresolved(Table *table, const Name *label) {
  table_put(table, label, (void*)label);
}

static bool calc_expr(Table *label_table, const Expr *expr, intptr_t *result,
                      Table *unresolved_labels) {
  assert(expr != NULL);
  switch (expr->kind) {
  case EX_LABEL:
    {
      LabelInfo *dst = table_get(label_table, expr->label);
      if (dst != NULL && (dst->flag & LF_DEFINED) != 0) {
        *result = dst->address;
        return true;
      } else {
        if (unresolved_labels != NULL)
          put_unresolved(unresolved_labels, expr->label);
        return false;
      }
    }
    break;
  case EX_FIXNUM:
    *result = expr->fixnum;
    return true;
  case EX_ADD:
  case EX_SUB:
  case EX_MUL:
  case EX_DIV:
    {
      intptr_t lhs, rhs;
      if (!calc_expr(label_table, expr->bop.lhs, &lhs, unresolved_labels) ||
          !calc_expr(label_table, expr->bop.rhs, &rhs, unresolved_labels))
        return false;
      switch (expr->kind) {
      case EX_ADD:  *result = lhs + rhs; break;
      case EX_SUB:  *result = lhs - rhs; break;
      case EX_MUL:  *result = lhs * rhs; break;
      case EX_DIV:  *result = lhs / rhs; break;
      default: assert(false); return false;
      }
    }
    return true;

  case EX_POS:
  case EX_NEG:
    {
      intptr_t sub;
      if (!calc_expr(label_table, expr->unary.sub, &sub, unresolved_labels))
        return false;
      switch (expr->kind) {
      case EX_POS:  *result = sub; break;
      case EX_NEG:  *result = -sub; break;
      default: assert(false); return false;
      }
    }
    return true;

  default: assert(false); return false;
  }
}

bool resolve_relative_address(Vector **section_irs, Table *label_table, Vector *unresolved) {
  Table unresolved_labels;
  table_init(&unresolved_labels);
  if (unresolved != NULL)
    vec_clear(unresolved);
  bool size_upgraded = false;
  for (int sec = 0; sec < SECTION_COUNT; ++sec) {
    Vector *irs = section_irs[sec];
    uintptr_t start_address = irs->len > 0 ? ((IR*)irs->data[0])->address : 0;
    for (int i = 0, len = irs->len; i < len; ++i) {
      IR *ir = irs->data[i];
      uintptr_t address = ir->address;
      switch (ir->kind) {
      case IR_CODE:
        {
          Inst *inst = ir->code.inst;
          switch (inst->op) {
          case LEA:
            if (inst->src.type == INDIRECT &&
                inst->src.indirect.reg.no == RIP &&
                inst->src.indirect.offset->kind != EX_FIXNUM) {
              Expr *expr = inst->src.indirect.offset;

              bool unres = false;
              if (expr->kind == EX_LABEL && unresolved != NULL) {
                LabelInfo *label = table_get(label_table, expr->label);
                if (label == NULL) {
                  UnresolvedInfo *info = malloc(sizeof(*info));
                  info->kind = UNRES_EXTERN_PC32;
                  info->label = expr->label;
                  info->src_section = sec;
                  info->offset = address + 3 - start_address;
                  info->add = -4;
                  vec_push(unresolved, info);
                  unres = true;
                } else if (label->section != sec) {
                  Vector *irs2 = section_irs[label->section];
                  uintptr_t dst_start_address = irs2->len > 0 ? ((IR*)irs2->data[0])->address : 0;

                  UnresolvedInfo *info = malloc(sizeof(*info));
                  info->kind = UNRES_OTHER_SECTION;
                  info->label = expr->label;
                  info->src_section = sec;
                  info->offset = address + 3 - start_address;
                  info->add = label->address - dst_start_address - 4;
                  vec_push(unresolved, info);
                  unres = true;
                }
              }
              if (!unres) {
                intptr_t dst;
                if (calc_expr(label_table, expr, &dst, &unresolved_labels)) {
                  intptr_t offset = dst - ((intptr_t)address + ir->code.len);
                  put_value(ir->code.buf + 3, offset, sizeof(int32_t));
                }
              }
            }
            break;
          case JMP:
          case JO: case JNO: case JB:  case JAE:
          case JE: case JNE: case JBE: case JA:
          case JS: case JNS: case JP:  case JNP:
          case JL: case JGE: case JLE: case JG:
            if (inst->src.type == DIRECT) {
              intptr_t dst;
              if (calc_expr(label_table, inst->src.direct.expr, &dst, &unresolved_labels)) {
                intptr_t offset = dst - ((intptr_t)address + ir->code.len);
                bool long_offset = ir->code.flag & INST_LONG_OFFSET;
                if (!long_offset) {
                  if (!is_im8(offset)) {
                    // Change to long offset, and recalculate.
                    ir->code.flag |= INST_LONG_OFFSET;
                    if (inst->op == JMP)
                      MAKE_CODE(inst, &ir->code, 0xe9, IM32(-1));
                    else
                      MAKE_CODE(inst, &ir->code, 0x0f, 0x80 + (inst->op - JO), IM32(-1));
                    size_upgraded = true;
                  } else {
                    put_value(ir->code.buf + 1, offset, sizeof(int8_t));
                  }
                } else {
                  if (!is_im32(offset))
                    error("Jump offset too far (over 32bit)");

                  int d = inst->op == JMP ? 1 : 2;
                  put_value(ir->code.buf + d, offset, sizeof(int32_t));
                }
              } else {
                assert(unresolved == NULL);  // Not implemented.
              }
            }
            break;
          case CALL:
            if (inst->src.type == DIRECT) {
              intptr_t dst;
              Expr *expr = inst->src.direct.expr;
              if (calc_expr(label_table, expr, &dst, &unresolved_labels)) {
                intptr_t offset = (intptr_t)dst - ((intptr_t)address + ir->code.len);
                put_value(ir->code.buf + 1, offset, sizeof(int32_t));
              } else {
                if (unresolved != NULL) {
                  assert(expr->kind == EX_LABEL);
                  UnresolvedInfo *info = malloc(sizeof(*info));
                  info->kind = UNRES_EXTERN;
                  info->label = expr->label;
                  info->src_section = sec;
                  info->offset = address + 1 - start_address;
                  info->add = -4;
                  vec_push(unresolved, info);
                }
              }
            }
            break;
          default:
            break;
          }
        }
        break;
      case IR_EXPR_BYTE:
      case IR_EXPR_WORD:
      case IR_EXPR_LONG:
        {
          intptr_t value;
          if (calc_expr(label_table, ir->expr, &value, &unresolved_labels)) {
            put_value(ir->code.buf + 3, value, sizeof(int32_t));
          } else {
            assert(!"Unhandled");
          }
        }
        break;
      case IR_EXPR_QUAD:
        {
          const Expr *expr = ir->expr;

          bool unres = false;
          if (expr->kind == EX_LABEL && unresolved != NULL) {
            LabelInfo *label = table_get(label_table, expr->label);
            if (label == NULL) {
              UnresolvedInfo *info = malloc(sizeof(*info));
              info->kind = UNRES_ABS64;  // TODO:
              info->label = expr->label;
              info->src_section = sec;
              info->offset = address - start_address;
              info->add = 0;
              vec_push(unresolved, info);
              unres = true;
            } else if (sec != SEC_CODE || label->section != sec) {
              UnresolvedInfo *info = malloc(sizeof(*info));
              info->kind = UNRES_ABS64;  // TODO
              info->label = expr->label;
              info->src_section = sec;
              info->offset = address - start_address;
              info->add = 0;
              vec_push(unresolved, info);
              unres = true;
            }
          }
          if (!unres) {
            intptr_t value;
            if (calc_expr(label_table, expr, &value, &unresolved_labels)) {
              put_value(ir->code.buf + 3, value, sizeof(int32_t));
            }
          }
        }
        break;
      case IR_LABEL:
      case IR_DATA:
      case IR_BSS:
      case IR_ALIGN:
        break;
      default:  assert(false); break;
      }
    }
  }

  if (unresolved_labels.count > 0 && unresolved == NULL) {
    for (int i = 0; ;) {
      const Name *name;
      void *dummy;
      i = table_iterate(&unresolved_labels, i, &name, &dummy);
      if (i < 0)
        break;
      fprintf(stderr, "Undefined reference: `%.*s'\n", name->bytes, name->chars);
    }
    exit(1);
  }

  return !size_upgraded;
}

void emit_irs(Vector **section_irs, Table *label_table) {
  for (int sec = 0; sec < SECTION_COUNT; ++sec) {
    Vector *irs = section_irs[sec];
    for (int i = 0, len = irs->len; i < len; ++i) {
      IR *ir = irs->data[i];
      switch (ir->kind) {
      case IR_LABEL:
        break;
      case IR_CODE:
        add_code(ir->code.buf, ir->code.len);
        break;
      case IR_DATA:
        add_section_data(sec, ir->data.buf, ir->data.len);
        break;
      case IR_BSS:
        add_bss(ir->bss);
        break;
      case IR_ALIGN:
        align_section_size(sec, ir->align);
        break;
      case IR_EXPR_BYTE:
      case IR_EXPR_WORD:
      case IR_EXPR_LONG:
      case IR_EXPR_QUAD:
        {
          intptr_t value;
          if (calc_expr(label_table, ir->expr, &value, NULL)) {
            int size = 1 << (ir->kind - IR_EXPR_BYTE);
            add_section_data(sec, &value, size);  // TODO: Target endian
          }
        }
        break;
      default:  assert(false); break;
      }
    }
  }
}
#include "parse_asm.h"

#include <assert.h>
#include <ctype.h>
#include <stddef.h>
#include <stdlib.h>  // strtoul
#include <string.h>
#include <strings.h>

#include "gen.h"
#include "ir_asm.h"
#include "table.h"
#include "util.h"

static Expr *parse_expr(ParseInfo *info);

// Align with Opcode.
static const char *kOpTable[] = {
  "mov",
  "movsx",
  "movzx",
  "lea",

  "add",
  "addq",
  "sub",
  "subq",
  "mul",
  "div",
  "idiv",
  "neg",
  "not",
  "inc",
  "incb",
  "incw",
  "incl",
  "incq",
  "dec",
  "decb",
  "decw",
  "decl",
  "decq",
  "and",
  "or",
  "xor",
  "shl",
  "shr",
  "sar",
  "cmp",
  "test",
  "cwtl",
  "cltd",
  "cqto",

  "seto",
  "setno",
  "setb",
  "setae",
  "sete",
  "setne",
  "setbe",
  "seta",
  "sets",
  "setns",
  "setp",
  "setnp",
  "setl",
  "setge",
  "setle",
  "setg",

  "jmp",
  "jo",
  "jno",
  "jb",
  "jae",
  "je",
  "jne",
  "jbe",
  "ja",
  "js",
  "jns",
  "jp",
  "jnp",
  "jl",
  "jge",
  "jle",
  "jg",
  "call",
  "ret",
  "push",
  "pop",

  "int",
  "syscall",

#ifndef __NO_FLONUM
  "movsd",
  "addsd",
  "subsd",
  "mulsd",
  "divsd",
  "ucomisd",
  "cvtsi2sd",
  "cvttsd2si",
  "sqrtsd",

  "movss",
  "addss",
  "subss",
  "mulss",
  "divss",
  "ucomiss",
  "cvtsi2ss",
  "cvttss2si",

  "cvtsd2ss",
  "cvtss2sd",
#endif
};

static const struct {
  const char *name;
  enum RegType reg;
} kRegisters[] = {
  {"al", AL},
  {"cl", CL},
  {"dl", DL},
  {"bl", BL},
  {"spl", SPL},
  {"bpl", BPL},
  {"sil", SIL},
  {"dil", DIL},

  {"r8b", R8B},
  {"r9b", R9B},
  {"r10b", R10B},
  {"r11b", R11B},
  {"r12b", R12B},
  {"r13b", R13B},
  {"r14b", R14B},
  {"r15b", R15B},

  {"ax", AX},
  {"cx", CX},
  {"dx", DX},
  {"bx", BX},
  {"sp", SP},
  {"bp", BP},
  {"si", SI},
  {"di", DI},

  {"r8w", R8W},
  {"r9w", R9W},
  {"r10w", R10W},
  {"r11w", R11W},
  {"r12w", R12W},
  {"r13w", R13W},
  {"r14w", R14W},
  {"r15w", R15W},

  {"eax", EAX},
  {"ecx", ECX},
  {"edx", EDX},
  {"ebx", EBX},
  {"esp", ESP},
  {"ebp", EBP},
  {"esi", ESI},
  {"edi", EDI},

  {"r8d", R8D},
  {"r9d", R9D},
  {"r10d", R10D},
  {"r11d", R11D},
  {"r12d", R12D},
  {"r13d", R13D},
  {"r14d", R14D},
  {"r15d", R15D},

  {"rax", RAX},
  {"rcx", RCX},
  {"rdx", RDX},
  {"rbx", RBX},
  {"rsp", RSP},
  {"rbp", RBP},
  {"rsi", RSI},
  {"rdi", RDI},

  {"r8", R8},
  {"r9", R9},
  {"r10", R10},
  {"r11", R11},
  {"r12", R12},
  {"r13", R13},
  {"r14", R14},
  {"r15", R15},

  {"rip", RIP},
};

#ifndef __NO_FLONUM
static const char kXmmRegisters[][6] = {
  "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7",
  "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15",
};
#endif

static const char *kDirectiveTable[] = {
  "ascii",
  "section",
  "text",
  "data",
  "align",
  "byte",
  "word",
  "long",
  "quad",
  "comm",
  "globl",
  "extern",
#ifndef __NO_FLONUM
  "float",
  "double",
#endif
};

bool err;

void parse_error(const ParseInfo *info, const char *message) {
  fprintf(stderr, "%s(%d): %s\n", info->filename, info->lineno, message);
  fprintf(stderr, "%s\n", info->rawline);
  err = true;
}

static bool is_reg8(enum RegType reg) {
  return reg >= AL && reg <= R15B;
}

static bool is_reg16(enum RegType reg) {
  return reg >= AX && reg <= R15W;
}

static bool is_reg32(enum RegType reg) {
  return reg >= EAX && reg <= R15D;
}

static bool is_reg64(enum RegType reg) {
  return reg >= RAX && reg <= R15;
}

static int find_match_index(const char **pp, const char **table, size_t count) {
  const char *p = *pp;
  const char *start = p;

  while (isalnum(*p))
    ++p;
  if (*p == '\0' || isspace(*p)) {
    size_t n = p - start;
    for (size_t i = 0; i < count; ++i) {
      const char *name = table[i];
      size_t len = strlen(name);
      if (n == len && strncasecmp(start, name, n) == 0) {
        *pp = skip_whitespaces(p);
        return i;
      }
    }
  }
  return -1;
}

static enum Opcode find_opcode(ParseInfo *info) {
  return find_match_index(&info->p, kOpTable, sizeof(kOpTable) / sizeof(*kOpTable)) + 1;
}

static enum DirectiveType find_directive(ParseInfo *info) {
  return find_match_index(&info->p, kDirectiveTable,
                          sizeof(kDirectiveTable) / sizeof(*kDirectiveTable)) + 1;
}

static enum RegType find_register(const char **pp) {
  const char *p = *pp;
  for (int i = 0, len = sizeof(kRegisters) / sizeof(*kRegisters); i < len; ++i) {
    const char *name = kRegisters[i].name;
    size_t n = strlen(name);
    if (strncmp(p, name, n) == 0) {
      *pp = p + n;
      return kRegisters[i].reg;
    }
  }
  return NOREG;
}

#ifndef __NO_FLONUM
static enum RegXmmType find_xmm_register(const char **pp) {
  const char *p = *pp;
  const char *q;
  for (q = p; isalnum(*q); ++q)
    ;
  size_t l = q - p;

  for (int i = 0, len = sizeof(kXmmRegisters) / sizeof(*kXmmRegisters); i < len; ++i) {
    const char *name = kXmmRegisters[i];
    size_t n = strlen(name);
    if (l == n && strncmp(p, name, n) == 0) {
      *pp = p + n;
      return i + XMM0;
    }
  }
  return NOREGXMM;
}
#endif

static bool immediate(const char **pp, long *value) {
  const char *p = *pp;
  bool negative = false;
  if (*p == '-') {
    negative = true;
    ++p;
  }

  int base = 10;
  if (*p == '0') {
    char c = tolower(p[1]);
    if (c == 'x') {
      base = 16;
      p += 2;
      c = tolower(*p);
      if (!isxdigit(c))
        return false;
    } else if (isdigit(c)) {
      if (c >= '8')
        return false;
      base = 8;
    }
  }
  const char *q = p;
  unsigned long val = strtoul(p, (char**)&p, base);
  if (p == q)
    return false;

  *value = negative ? -val : val;
  *pp = p;
  return true;
}

static bool is_label_first_chr(char c) {
  return isalpha(c) || c == '_' || c == '.';
}

static bool is_label_chr(char c) {
  return is_label_first_chr(c) || isdigit(c);
}

static const Name *parse_label(ParseInfo *info) {
  const char *p = info->p;
  const char *start = p;
  if (!is_label_first_chr(*p))
    return NULL;

  do {
    ++p;
  } while (is_label_chr(*p));
  info->p = p;
  return alloc_name(start, p, false);
}

static const Name *parse_section_name(ParseInfo *info) {
  const char *p = info->p;
  const char *start = p;

  if (!is_label_first_chr(*p))
    return NULL;

  do {
    ++p;
  } while (isalnum(*p) || *p == '_' || *p == '.');
  info->p = p;
  return alloc_name(start, p, false);
}

static enum RegType parse_direct_register(ParseInfo *info, Operand *operand) {
#ifndef __NO_FLONUM
  {
    enum RegXmmType regxmm = find_xmm_register(&info->p);
    if (regxmm != NOREGXMM) {
      operand->type = REG_XMM;
      operand->regxmm = regxmm;
      return true;
    }
  }
#endif

  enum RegType reg = find_register(&info->p);
  enum RegSize size;
  int no;
  if (is_reg8(reg)) {
    size = REG8;
    no = reg - AL;
  } else if (is_reg16(reg)) {
    size = REG16;
    no = reg - AX;
  } else if (is_reg32(reg)) {
    size = REG32;
    no = reg - EAX;
  } else if (is_reg64(reg)) {
    size = REG64;
    no = reg - RAX;
  } else {
    parse_error(info, "Illegal register");
    return false;
  }

  operand->type = REG;
  operand->reg.size = size;
  operand->reg.no = no & 7;
  operand->reg.x = (no & 8) >> 3;
  return true;
}

static bool parse_indirect_register(ParseInfo *info, Expr *offset, Operand *operand) {
  enum RegType index_reg = NOREG;
  Expr *scale = NULL;
  // Already read "(%".
  enum RegType base_reg = find_register(&info->p);

  info->p = skip_whitespaces(info->p);
  if (*info->p == ',') {
    info->p = skip_whitespaces(info->p + 1);
    if (*info->p != '%' ||
        (++info->p, index_reg = find_register(&info->p), !is_reg64(index_reg)))
      parse_error(info, "Register expected");
    info->p = skip_whitespaces(info->p);
    if (*info->p == ',') {
      info->p = skip_whitespaces(info->p + 1);
      scale = parse_expr(info);
      if (scale->kind != EX_FIXNUM)
        parse_error(info, "constant value expected");
      info->p = skip_whitespaces(info->p);
    }
  }
  if (*info->p != ')')
    parse_error(info, "`)' expected");
  else
    ++info->p;

  if (!(is_reg64(base_reg) || (base_reg == RIP && index_reg == NOREG)))
    parse_error(info, "Register expected1");

  if (index_reg == NOREG) {
    char no = base_reg - RAX;
    operand->type = INDIRECT;
    operand->indirect.reg.size = REG64;
    operand->indirect.reg.no = base_reg != RIP ? no & 7 : RIP;
    operand->indirect.reg.x = (no & 8) >> 3;
    operand->indirect.offset = offset;
  } else {
    if (!is_reg64(index_reg))
      parse_error(info, "Register expected2");

    operand->type = INDIRECT_WITH_INDEX;
    operand->indirect_with_index.offset = offset;
    operand->indirect_with_index.scale = scale;
    char base_no = base_reg - RAX;
    operand->indirect_with_index.base_reg.size = REG64;
    operand->indirect_with_index.base_reg.no = base_no & 7;
    operand->indirect_with_index.base_reg.x = (base_no & 8) >> 3;
    char index_no = index_reg - RAX;
    operand->indirect_with_index.index_reg.size = REG64;
    operand->indirect_with_index.index_reg.no = index_no & 7;
    operand->indirect_with_index.index_reg.x = (index_no & 8) >> 3;
  }

  return true;
}

static enum RegType parse_deref_register(ParseInfo *info, Operand *operand) {
  enum RegType reg = find_register(&info->p);
  if (!is_reg64(reg))
    parse_error(info, "Illegal register");

  char no = reg - RAX;
  operand->type = DEREF_REG;
  operand->deref_reg.size = REG64;
  operand->deref_reg.no = no & 7;
  operand->deref_reg.x = (no & 8) >> 3;
  return true;
}

enum TokenKind {
  TK_UNKNOWN,
  TK_LABEL,
  TK_FIXNUM,
  TK_ADD = '+',
  TK_SUB = '-',
  TK_MUL = '*',
  TK_DIV = '/',
#ifndef __NO_FLONUM
  TK_FLONUM,
#endif
};

typedef struct Token {
  enum TokenKind kind;
  union {
    const Name *label;
    long fixnum;
#ifndef __NO_FLONUM
    double flonum;
#endif
  };
} Token;

static Token *new_token(enum TokenKind kind) {
  Token *token = malloc(sizeof(*token));
  token->kind = kind;
  return token;
}

static bool ishexdigit(int c) {
  if (isdigit(c))
    return true;
  c = tolower(c);
  return 'a' <= c && c <= 'f';
}

#ifndef __NO_FLONUM
static const Token *read_flonum(ParseInfo *info) {
  const char *p = info->p;
  char *q;
  double f = strtod(p, &q);
  Token *token = new_token(TK_FLONUM);
  token->flonum = f;
  info->next = q;
  return token;
}
#endif

static const Token *fetch_token(ParseInfo *info) {
  if (info->token != NULL)
    return info->token;

  const char *start = skip_whitespaces(info->p);
  const char *p = start;
  char c = *p;
  if (isdigit(c)) {
    int base = 10;
    if (tolower(p[1]) == 'x' && ishexdigit(p[2])) {
      p += 2;
      base = 16;
    }
    char *q;
    unsigned long v = strtoul(p, &q, base);
#ifndef __NO_FLONUM
    if (*q == '.' || tolower(*q)== 'e') {
      info->p = p;
      return read_flonum(info);
    }
#endif
    Token *token = new_token(TK_FIXNUM);
    token->fixnum = v;
    info->next = q;
    return token;
#ifndef __NO_FLONUM
  } else if (c == '.' && isdigit(p[1])) {
    info->p = p;
    return read_flonum(info);
#endif
  } else if (is_label_first_chr(c)) {
    while (c = *++p, is_label_chr(c))
      ;
    Token *token = new_token(TK_LABEL);
    token->label = alloc_name(start, p, false);
    info->next = p;
    return token;
  } else if (strchr("+-*/", c) != NULL) {
    info->next = p + 1;
    return new_token(c);
  }

  static const Token kTokUnknown = {TK_UNKNOWN};
  return &kTokUnknown;
}

static const Token *match(ParseInfo *info, enum TokenKind kind) {
  const Token *token = fetch_token(info);
  if (token->kind != kind)
    return NULL;
  info->token = NULL;
  info->p = info->next;
  return token;
}

static Expr *new_expr(enum ExprKind kind) {
  Expr *expr = malloc(sizeof(*expr));
  expr->kind = kind;
  return expr;
}

static Expr *prim(ParseInfo *info) {
  Expr *expr = NULL;
  const Token *tok;
  if ((tok = match(info, TK_LABEL)) != NULL) {
    expr = new_expr(EX_LABEL);
    expr->label = tok->label;
  } else if ((tok = match(info, TK_FIXNUM)) != NULL) {
    expr = new_expr(EX_FIXNUM);
    expr->fixnum = tok->fixnum;
#ifndef __NO_FLONUM
  } else if ((tok = match(info, TK_FLONUM)) != NULL) {
    expr = new_expr(EX_FLONUM);
    expr->flonum = tok->flonum;
#endif
  }
  return expr;
}

static Expr *unary(ParseInfo *info) {
  const Token *tok;
  if ((tok = match(info, TK_ADD)) != NULL) {
    Expr *expr = unary(info);
    if (expr == NULL)
      return NULL;
    switch (expr->kind) {
    case EX_FIXNUM:
#ifndef __NO_FLONUM
    case EX_FLONUM:
#endif
      return expr;
    default:
      {
        Expr *op = new_expr(EX_POS);
        op->unary.sub = expr;
        return op;
      }
    }
  }

  if ((tok = match(info, TK_SUB)) != NULL) {
    Expr *expr = unary(info);
    if (expr == NULL)
      return NULL;
    switch (expr->kind) {
    case EX_FIXNUM:
      expr->fixnum = -expr->fixnum;
      return expr;
#ifndef __NO_FLONUM
    case EX_FLONUM:
      expr->flonum = -expr->flonum;
      return expr;
#endif
    default:
      {
        Expr *op = new_expr(EX_NEG);
        op->unary.sub = expr;
        return op;
      }
    }
  }

  return prim(info);
}

static Expr *parse_mul(ParseInfo *info) {
  Expr *expr = unary(info);
  if (expr == NULL)
    return expr;

  const Token *tok;
  while ((tok = match(info, TK_MUL)) != NULL ||
         (tok = match(info, TK_DIV)) != NULL) {
    Expr *rhs = unary(info);
    if (rhs == NULL) {
      parse_error(info, "expression error");
      break;
    }

    Expr *lhs = expr;
    if (lhs->kind == EX_FIXNUM && rhs->kind == EX_FIXNUM) {
      switch (tok->kind) {
      case TK_MUL:  lhs->fixnum *= rhs->fixnum; break;
      case TK_DIV:  lhs->fixnum += rhs->fixnum; break;
      default:  assert(false); break;
      }
    } else {
      expr = new_expr((enum ExprKind)tok->kind);  // Assume TokenKind is same as ExprKind.
      expr->bop.lhs = lhs;
      expr->bop.rhs = rhs;
    }
  }
  return expr;
}

static Expr *parse_add(ParseInfo *info) {
  Expr *expr = parse_mul(info);
  if (expr == NULL)
    return expr;

  const Token *tok;
  while ((tok = match(info, TK_ADD)) != NULL ||
         (tok = match(info, TK_SUB)) != NULL) {
    Expr *rhs = parse_mul(info);
    if (rhs == NULL) {
      parse_error(info, "expression error");
      break;
    }

    Expr *lhs = expr;
    if (lhs->kind == EX_FIXNUM && rhs->kind == EX_FIXNUM) {
      switch (tok->kind) {
      case TK_ADD:  lhs->fixnum += rhs->fixnum; break;
      case TK_SUB:  lhs->fixnum += rhs->fixnum; break;
      default:  assert(false); break;
      }
    } else {
      expr = new_expr((enum ExprKind)tok->kind);  // Assume TokenKind is same as ExprKind.
      expr->bop.lhs = lhs;
      expr->bop.rhs = rhs;
    }
  }
  return expr;
}

static Expr *parse_expr(ParseInfo *info) {
  info->token = NULL;
  info->next = NULL;
  return parse_add(info);
}

static bool parse_operand(ParseInfo *info, Operand *operand) {
  const char *p = info->p;
  if (*p == '%') {
    info->p = p + 1;
    return parse_direct_register(info, operand);
  }

  if (*p == '*' && p[1] == '%') {
    info->p = p + 2;
    return parse_deref_register(info, operand);
  }

  if (*p == '$') {
    info->p = p + 1;
    if (!immediate(&info->p, &operand->immediate))
      parse_error(info, "Syntax error");
    operand->type = IMMEDIATE;
    return true;
  }

  Expr *expr = parse_expr(info);
  info->p = skip_whitespaces(info->p);
  if (*info->p != '(') {
    if (expr != NULL) {
      if (expr->kind == EX_LABEL) {
        operand->type = DIRECT;
        operand->direct.expr = expr;
        return true;
      }
      parse_error(info, "direct number not implemented");
    }
  } else {
    if (info->p[1] == '%') {
      info->p += 2;
      if (expr == NULL) {
        expr = malloc(sizeof(*expr));
        expr->kind = EX_FIXNUM;
        expr->fixnum = 0;
      }
      return parse_indirect_register(info, expr, operand);
    }
    parse_error(info, "Illegal `('");
  }

  return false;
}

static void parse_inst(ParseInfo *info, Inst *inst) {
  enum Opcode op = find_opcode(info);
  inst->op = op;
  if (op != NOOP) {
    if (parse_operand(info, &inst->src)) {
      info->p = skip_whitespaces(info->p);
      if (*info->p == ',') {
        info->p = skip_whitespaces(info->p + 1);
        parse_operand(info, &inst->dst);
        info->p = skip_whitespaces(info->p);
      }
    }
  }
}

int current_section = SEC_CODE;

Line *parse_line(ParseInfo *info) {
  Line *line = malloc(sizeof(*line));
  line->label = NULL;
  line->inst.op = NOOP;
  line->inst.src.type = line->inst.dst.type = NOOPERAND;
  line->dir = NODIRECTIVE;

  info->p = info->rawline;
  line->label = parse_label(info);
  if (line->label != NULL) {
    if (*info->p != ':') {
      parse_error(info, "`:' required after label");
      return NULL;
    }
    ++info->p;
  }

  info->p = skip_whitespaces(info->p);
  if (*info->p == '.') {
    ++info->p;
    enum DirectiveType dir = find_directive(info);
    if (dir == NODIRECTIVE) {
      parse_error(info, "Unknown directive");
      return NULL;
    }
    line->dir = dir;
  } else if (*info->p != '\0') {
    parse_inst(info, &line->inst);
    if (*info->p != '\0' && !(*info->p == '/' && info->p[1] == '/')) {
      parse_error(info, "Syntax error");
      err = true;
    }
  }
  return line;
}

static char unescape_char(char c) {
  switch (c) {
  case '0':  return '\0';
  case 'n':  return '\n';
  case 't':  return '\t';
  case 'r':  return '\r';
  case '"':  return '"';
  case '\'':  return '\'';
  default:
    return c;
  }
}

static size_t unescape_string(ParseInfo *info, const char *p, char *dst) {
  size_t len = 0;
  for (; *p != '"'; ++p, ++len) {
    char c = *p;
    if (c == '\0')
      parse_error(info, "string not closed");
    if (c == '\\') {
      // TODO: Handle \x...
      c = unescape_char(*(++p));
    }
    if (dst != NULL)
      *dst++ = c;
  }
  return len;
}

void handle_directive(ParseInfo *info, enum DirectiveType dir, Vector **section_irs,
                      Table *label_table) {
  Vector *irs = section_irs[current_section];

  switch (dir) {
  case DT_ASCII:
    {
      if (*info->p != '"')
        parse_error(info, "`\"' expected");
      ++info->p;
      size_t len = unescape_string(info, info->p, NULL);
      char *str = malloc(len);
      unescape_string(info, info->p, str);

      vec_push(irs, new_ir_data(str, len));
    }
    break;

  case DT_COMM:
    {
      const Name *label = parse_label(info);
      if (label == NULL)
        parse_error(info, ".comm: label expected");
      info->p = skip_whitespaces(info->p);
      if (*info->p != ',')
        parse_error(info, ".comm: `,' expected");
      info->p = skip_whitespaces(info->p + 1);
      long count;
      if (!immediate(&info->p, &count)) {
        parse_error(info, ".comm: count expected");
        return;
      }

      long align = 0;
      if (*info->p == ',') {
        info->p = skip_whitespaces(info->p + 1);
        if (!immediate(&info->p, &align) || align < 1) {
          parse_error(info, ".comm: optional alignment expected");
          return;
        }
      }

      enum SectionType sec = SEC_BSS;
      irs = section_irs[sec];
      if (align > 1)
        vec_push(irs, new_ir_align(align));
      vec_push(irs, new_ir_label(label));
      vec_push(irs, new_ir_bss(count));

      if (!add_label_table(label_table, label, sec, true, false))
        return;
    }
    break;

  case DT_TEXT:
    current_section = SEC_CODE;
    break;

  case DT_DATA:
    current_section = SEC_DATA;
    break;

  case DT_ALIGN:
    {
      long align;
      if (!immediate(&info->p, &align))
        parse_error(info, ".align: number expected");
      vec_push(irs, new_ir_align(align));
    }
    break;

  case DT_BYTE:
  case DT_WORD:
  case DT_LONG:
  case DT_QUAD:
    {
      Expr *expr = parse_expr(info);
      if (expr == NULL) {
        parse_error(info, "expression expected");
        break;
      }

#ifndef __NO_FLONUM
      assert(expr->kind != EX_FLONUM);
#endif
      if (expr->kind == EX_FIXNUM) {
        // TODO: Target endian.
        long value = expr->fixnum;
        int size = 1 << (dir - DT_BYTE);
        unsigned char *buf = malloc(size);
        for (int i = 0; i < size; ++i)
          buf[i] = value >> (8 * i);
        vec_push(irs, new_ir_data(buf, size));
      } else {
        vec_push(irs, new_ir_expr((enum IrKind)(IR_EXPR_BYTE + (dir - DT_BYTE)), expr));
      }
    }
    break;

#ifndef __NO_FLONUM
  case DT_FLOAT:
  case DT_DOUBLE:
    {
      Expr *expr = parse_expr(info);
      if (expr == NULL) {
        parse_error(info, "expression expected");
        break;
      }

      double value;
      switch (expr->kind) {
      case EX_FIXNUM:  value = expr->fixnum; break;
      case EX_FLONUM:  value = expr->flonum; break;
      default:
        assert(false);
        value = -1;
        break;
      }
      int size = dir == DT_FLOAT ? sizeof(float) : sizeof(double);
      unsigned char *buf = malloc(size);
      if (dir == DT_FLOAT) {
        float fval = value;
        memcpy(buf, (void*)&fval, sizeof(fval));  // TODO: Endian
      } else {
        memcpy(buf, (void*)&value, sizeof(value));  // TODO: Endian
      }
      vec_push(irs, new_ir_data(buf, size));
    }
    break;
#endif

  case DT_GLOBL:
    {
      const Name *label = parse_label(info);
      if (label == NULL) {
        parse_error(info, ".globl: label expected");
        return;
      }

      if (!add_label_table(label_table, label, current_section, false, true))
        err = true;
    }
    break;

  case DT_SECTION:
    {
      const Name *name = parse_section_name(info);
      if (name == NULL) {
        parse_error(info, ".section: section name expected");
        return;
      }
      if (equal_name(name, alloc_name(".rodata", NULL, false))) {
        current_section = SEC_RODATA;
      } else {
        parse_error(info, "Unknown section name");
        return;
      }
    }
    break;

  case DT_EXTERN:
    break;

  default:
    parse_error(info, "Unhandled directive");
    break;
  }
}
#include "util.h"

#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc
#include <string.h>  // strcmp

#define VERSION  "0.1.0"
#include "table.h"

char *strdup_(const char *str) {
  return strndup_(str, strlen(str));
}

char *strndup_(const char *str, size_t size) {
  char *dup = malloc(size + 1);
  strncpy(dup, str, size);
  dup[size] = '\0';
  return dup;
}

bool starts_with(const char *str, const char *prefix) {
  size_t len = strlen(prefix);
  return strncmp(str, prefix, len) == 0;
}

static char label_prefix[8] = "L";

const Name *alloc_label(void) {
  static int label_no;
  ++label_no;
  char buf[1 + (sizeof(label_prefix) - 1) + sizeof(int) * 3 + 1];
  snprintf(buf, sizeof(buf), ".%s%d", label_prefix, label_no);
  return alloc_name(buf, NULL, true);
}

void set_local_label_prefix(const char *prefix) {
  if (strlen(prefix) >= sizeof(label_prefix) - 1)
    error("Label prefix too long");
  strncpy(label_prefix, prefix, sizeof(label_prefix));
}

ssize_t getline_cat(char **lineptr, size_t *n, FILE *stream, size_t curlen) {
  char *nextline = NULL;
  size_t capa = 0;
  ssize_t len = getline(&nextline, &capa, stream);
  if (len == -1)
    return -1;
  if (len > 0) {
    char *oldline = *lineptr;
    char *reallocated = realloc(oldline, curlen + len + 1);
    if (reallocated == NULL)
      return -1;

    memcpy(reallocated + curlen, nextline, len + 1);
    *lineptr = reallocated;
    *n = curlen + len;  // '\0' is not included.
    free(nextline);
  }
  return curlen + len;
}

bool is_fullpath(const char *filename) {
  if (*filename != '/')
    return false;
  for (const char *p = filename;;) {
    p = strstr(p, "/..");
    if (p == NULL)
      return true;
    if (p[3] == '/' || p[3] == '\0')
      return false;
    p += 3;
  }
}

char *cat_path(const char *root, const char *path) {
  if (is_fullpath(path))
    return strdup_(path);
  if (*path == '/')
    root = "/";

  // Assume that root doesn't include ".."

  bool is_root = *root == '/';

  Vector *dirs = new_vector();  // [start, end]
  for (const char *p = root; *p != '\0'; ) {
    if (*p == '/')
      if (*(++p) == '\0')
        break;
    vec_push(dirs, p);
    const char *q = strchr(p, '/');
    if (q == NULL) {
      vec_push(dirs, p + strlen(p));
      break;
    }
    vec_push(dirs, q);
    p = q;
  }

  for (const char *p = path; *p != '\0'; ) {
    if (*p == '/') {
      while (*p == '/')
        ++p;
      if (*p == '\0') {
        // End with '/'.
        vec_push(dirs, p);
        vec_push(dirs, p);
        break;
      }
    }
    const char *q = strchr(p, '/');
    if (q == NULL)
      q = p + strlen(p);
    size_t size = q - p;
    if (size == 1 && strncmp(p, ".", size) == 0) {
      // Skip
    } else if (size == 2 && strncmp(p, "..", size) == 0) {
      if (dirs->len < 2)
        return NULL;  // Illegal
      dirs->len -= 2;
    } else {
      vec_push(dirs, p);
      vec_push(dirs, q);
    }
    p = q;
  }

  if (dirs->len == 0)
    return strdup_("/");

  size_t total_len = 1;  // 1 for NUL-terminate.
  for (int i = 0; i < dirs->len; i += 2) {
    if (i != 0 || is_root)
      total_len += 1;
    total_len += ((char*)dirs->data[i + 1] - (char*)dirs->data[i]);
  }

  char *buf = malloc(total_len);
  char *p = buf;
  for (int i = 0; i < dirs->len; i += 2) {
    if (i != 0 || is_root)
      *p++ = '/';
    size_t size = (char*)dirs->data[i + 1] - (char*)dirs->data[i];
    memcpy(p, dirs->data[i], size);
    p += size;
  }
  *p = '\0';
  return buf;
}

char *change_ext(const char *path, const char *ext) {
  const char *p = strrchr(path, '/');
  if (p == NULL)
    p = path;

  const char *q = strrchr(p, '.');
  size_t len = q != NULL ? (size_t)(q - path) : strlen(path);
  size_t ext_len = strlen(ext);
  char *s = malloc(len + 1 + ext_len);
  if (s != NULL) {
    memcpy(s, path, len);
    s[len] = '.';
    strcpy(s + (len + 1), ext);
  }
  return s;
}

#ifndef SELF_HOSTING
void myqsort(void *base, size_t nmemb, size_t size, int (*compare)(const void *, const void *)) {
  if (nmemb <= 1)
    return;

  char *a = base;
  const char *px;

  px = &a[(nmemb >> 1) * size];
  int i = 0;
  int j = nmemb - 1;
  for (;;) {
    while (compare(&a[i * size], px) < 0)
      ++i;
    while (compare(px, &a[j * size]) < 0)
      --j;
    if (i >= j)
      break;

    char *pi = &a[i * size];
    char *pj = &a[j * size];
    for (size_t k = 0; k < size; ++k) {
      char t = pi[k];
      pi[k] = pj[k];
      pj[k] = t;
    }
    if (px == pi)
      px = pj;
    else if (px == pj)
      px = pi;
    ++i;
    --j;
  }
  if (i > 1)
    myqsort(a, i, size, compare);
  if ((size_t)(j + 2) < nmemb)
    myqsort(&a[(j + 1) * size], nmemb - j - 1, size, compare);
}
#endif

void show_version(const char *exe) {
  printf("%s %s\n", exe, VERSION);
}

void error(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fprintf(stderr, "\n");
  exit(1);
}

bool is_im8(intptr_t x) {
  return x <= ((1L << 7) - 1) && x >= -(1L << 7);
}

bool is_im32(intptr_t x) {
  return x <= ((1L << 31) - 1) && x >= -(1L << 31);
}

const char *skip_whitespaces(const char *s) {
  while (isspace(*s))
    ++s;
  return s;
}

// Container

#define BUF_MIN    (16 / 2)
#define BUF_ALIGN  (16)

void buf_put(Buffer *buf, const void *data, size_t bytes) {
  size_t size = buf->size;
  size_t newsize = size + bytes;

  if (newsize > buf->capa) {
    size_t newcapa = ALIGN(MAX(newsize, BUF_MIN) * 2, BUF_ALIGN);
    unsigned char *p = realloc(buf->data, newcapa);
    if (p == NULL)
      error("not enough memory");
    buf->data = p;
    buf->capa = newcapa;
  }

  memcpy(buf->data + size, data, bytes);
  buf->size = newsize;
}

void buf_align(Buffer *buf, int align) {
  size_t size = buf->size;
  size_t aligned_size = ALIGN(size, align);
  size_t add = aligned_size - size;
  if (add <= 0)
    return;

  void *zero = calloc(add, 1);
  buf_put(buf, zero, add);
  free(zero);

  assert(buf->size == aligned_size);
}

Vector *new_vector(void) {
  Vector *vec = malloc(sizeof(Vector));
  vec->data = NULL;
  vec->capacity = 0;
  vec->len = 0;
  return vec;
}

void vec_clear(Vector *vec) {
  vec->len = 0;
}

void vec_push(Vector *vec, const void *elem) {
  if (vec->capacity <= vec->len) {
    if (vec->capacity <= 0)
      vec->capacity = 16;
    else
      vec->capacity <<= 1;
    vec->data = realloc(vec->data, sizeof(*vec->data) * vec->capacity);
  }
  vec->data[vec->len++] = (void*)elem;
}

void *vec_pop(Vector *vec) {
  return vec->len > 0 ? vec->data[--vec->len] : NULL;
}

void vec_insert(Vector *vec, int pos, const void *elem) {
  int len = vec->len;
  if (pos < 0 || pos > len)
    return;

  if (pos < len) {
    vec_push(vec, NULL);
    memmove(&vec->data[pos + 1], &vec->data[pos], sizeof(void*) * (len - pos));
    vec->data[pos] = (void*)elem;
  } else {
    vec_push(vec, elem);
  }
}

void vec_remove_at(Vector *vec, int index) {
  if (index < 0 || index >= vec->len)
    return;
  int d = vec->len - index - 1;
  if (d > 0)
    memmove(&vec->data[index], &vec->data[index + 1], d * sizeof(*vec->data));
  --vec->len;
}

bool vec_contains(Vector *vec, void *elem) {
  for (int i = 0, len = vec->len; i < len; ++i) {
    if (vec->data[i] == elem)
      return true;
  }
  return false;
}

// StringBuffer

typedef struct {
  const char *start;
  size_t len;
} StringElement;

void sb_init(StringBuffer *sb) {
  sb->elems = new_vector();
}

void sb_clear(StringBuffer *sb) {
  vec_clear(sb->elems);
}

bool sb_empty(StringBuffer *sb) {
  return sb->elems->len == 0;
}

void sb_append(StringBuffer *sb, const char *start, const char *end) {
  StringElement *elem = malloc(sizeof(*elem));
  elem->start = start;
  elem->len = end != NULL ? (size_t)(end - start) : strlen(start);
  vec_push(sb->elems, elem);
}

char *sb_to_string(StringBuffer *sb) {
  size_t total_len = 0;
  int count = sb->elems->len;
  for (int i = 0; i < count; ++i) {
    StringElement *elem = sb->elems->data[i];
    total_len += elem->len;
  }

  char *str = malloc(total_len + 1);
  char *p = str;
  for (int i = 0; i < count; ++i) {
    StringElement *elem = sb->elems->data[i];
    memcpy(p, elem->start, elem->len);
    p += elem->len;
  }
  *p = '\0';
  return str;
}

static const char *escape(int c) {
  switch (c) {
  case '\0': return "\\0";
  case '\n': return "\\n";
  case '\r': return "\\r";
  case '\t': return "\\t";
  case '"': return "\\\"";
  case '\\': return "\\\\";
  default:
    if (c < 0x20 || c >= 0x7f) {
      char *s = malloc(5);
      snprintf(s, 5, "\\x%02x", c & 0xff);
      return s;
    }
    return NULL;
  }
}

void escape_string(const char *str, size_t size, StringBuffer *sb) {
  const char *s, *p;
  const char *end = str + size;
  for (s = p = str; p < end; ++p) {
    const char *e = escape(*p);
    if (e == NULL)
      continue;

    if (p > s)
      sb_append(sb, s, p);
    sb_append(sb, e, NULL);
    s = p + 1;
  }
  if (p > s)
    sb_append(sb, s, p);
}
#include "table.h"

#include <stdlib.h>  // malloc
#include <string.h>

// Hash

static uint32_t hash_string(const char *key, int length) {
  const unsigned char *u = (const unsigned char*)key;
  // FNV1a
  uint32_t hash = 2166136261u;
  for (int i = 0; i < length; ++i)
    hash = (hash ^ u[i]) * 16777619u;
  return hash;
}

// Name

static Table name_table;

static const Name *find_name_table(const char *chars, int bytes, uint32_t hash) {
  const Table *table = &name_table;
  if (table->count == 0)
    return NULL;

  for (uint32_t index = hash % table->capacity; ; index = (index + 1) % table->capacity) {
    TableEntry *entry = &table->entries[index];
    const Name *key = entry->key;
    if (key == NULL) {
      if (entry->value == NULL)
        return NULL;
    } else if (key->bytes == bytes &&
               key->hash == hash &&
               memcmp(key->chars, chars, bytes) == 0) {
      return key;
    }
  }
}

const Name *alloc_name(const char *begin, const char *end, bool make_copy) {
  int bytes = end != NULL ? (int)(end - begin) : (int)strlen(begin);
  uint32_t hash = hash_string(begin, bytes);
  const Name *name = find_name_table(begin, bytes, hash);
  if (name == NULL) {
    if (make_copy) {
      char *new_str = malloc(bytes);
      memcpy(new_str, begin, bytes);
      begin = new_str;
    }
    Name *new_name = malloc(sizeof(*new_name));
    new_name->chars = begin;
    new_name->bytes = bytes;
    new_name->hash = hash;
    table_put(&name_table, new_name, new_name);
    name = new_name;
  }
  return name;
}

bool equal_name(const Name *name1, const Name *name2) {
  return name1 == name2;  // All names are interned, so they can compare by pointers.
}

// Table

static TableEntry *find_entry(TableEntry *entries, int capacity, const Name *key) {
  TableEntry *tombstone = NULL;
  for (uint32_t index = key->hash % capacity; ; index = (index + 1) % capacity) {
    TableEntry *entry = &entries[index];
    if (entry->key == NULL) {
      if (entry->value == NULL) {
        return tombstone != NULL ? tombstone : entry;
      } else {  // Tombstone.
        if (tombstone == NULL)
          tombstone = entry;
      }
    } else if (entry->key == key) {
      return entry;
    }
  }
}

static void adjust_capacity(Table *table, int new_capacity) {
  TableEntry *new_entries = malloc(sizeof(TableEntry) * new_capacity);
  for (int i = 0; i < new_capacity; ++i) {
    TableEntry *entry = &new_entries[i];
    entry->key = NULL;
    entry->value = NULL;
  }

  TableEntry *old_entries = table->entries;
  int old_capacity = table->capacity;
  int new_count = 0;
  for (int i = 0; i < old_capacity; ++i) {
    TableEntry *entry = &old_entries[i];
    if (entry->key == NULL)
      continue;

    TableEntry *dest = find_entry(new_entries, new_capacity, entry->key);
    dest->key = entry->key;
    dest->value = entry->value;
    ++new_count;
  }

  free(old_entries);
  table->entries = new_entries;
  table->capacity = new_capacity;
  table->count = new_count;
}

void table_init(Table *table) {
  table->entries = NULL;
  table->count = table->capacity = 0;
}

void *table_get(Table *table, const Name *key) {
  if (table->count == 0)
    return NULL;

  TableEntry *entry = find_entry(table->entries, table->capacity, key);
  if (entry->key == NULL)
    return NULL;

  return entry->value;
}

bool table_try_get(Table *table, const Name *key, void **output) {
  if (table->count == 0)
    return false;

  TableEntry *entry = find_entry(table->entries, table->capacity, key);
  if (entry->key == NULL)
    return false;

  *output = entry->value;
  return true;
}

bool table_put(Table *table, const Name *key, void *value) {
  const int MIN_CAPACITY = 15;
  if (table->count >= table->capacity / 2) {
    int capacity = table->capacity * 2 - 1;  // Keep odd.
    if (capacity < MIN_CAPACITY)
      capacity = MIN_CAPACITY;
    adjust_capacity(table, capacity);
  }

  TableEntry *entry = find_entry(table->entries, table->capacity, key);
  bool is_new_key = entry->key == NULL;
  if (is_new_key && entry->value == NULL)
    ++table->count;

  entry->key = key;
  entry->value = value;
  return is_new_key;
}

bool table_delete(Table *table, const Name *key) {
  if (table->count == 0)
    return false;

  TableEntry *entry = find_entry(table->entries, table->capacity, key);
  if (entry->key == NULL)
    return false;

  // Put tombstone.
  entry->key = NULL;
  entry->value = entry;

  return true;
}

int table_iterate(Table *table, int iterator, const Name **pkey, void **pvalue) {
  int capacity = table->capacity;
  for (; iterator < capacity; ++iterator) {
    const TableEntry *entry = &table->entries[iterator];
    const Name *key = entry->key;
    if (key != NULL) {
      if (pkey != NULL)
        *pkey = key;
      if (pvalue != NULL)
        *pvalue = entry->value;
      return iterator + 1;
    }
  }
  return -1;
}
#include "elfutil.h"

#ifndef ELF_NOT_SUPPORTED

#include <stdio.h>
#include <stdlib.h>  // calloc
#include <string.h>  // memcpy

#if defined(__XV6)
// XV6
#include "../kernel/types.h"
#include "../kernel/elf.h"

#elif defined(__linux__)
// Linux
#include <elf.h>

#endif

#if !defined(__NO_ELF_OBJ)
void strtab_init(Strtab *strtab) {
  table_init(&strtab->offsets);
}

size_t strtab_add(Strtab *strtab, const Name *name) {
  void *result;
  if (!table_try_get(&strtab->offsets, name, &result)) {
    size_t offset = strtab->size;
    table_put(&strtab->offsets, name, (void*)offset);
    strtab->size += name->bytes + 1;
    return offset;
  } else {
    return (size_t)result;
  }
}

void *strtab_dump(Strtab *strtab) {
  void *buf = malloc(strtab->size);
  if (buf != NULL) {
    unsigned char *p = buf;
    const Name *name;
    void *value;
    for (int it = 0; (it = table_iterate(&strtab->offsets, it, &name, &value)) != -1; ) {
      uintptr_t offset = (uintptr_t)value;
      memcpy(p + offset, name->chars, name->bytes);
      p[offset + name->bytes] = '\0';
    }
  }
  return buf;
}

//

void symtab_init(Symtab *symtab) {
  strtab_init(&symtab->strtab);
  table_init(&symtab->indices);
  symtab->buf = NULL;
  symtab->count = 0;
}

Elf64_Sym *symtab_add(Symtab *symtab, const Name *name) {
  uint32_t offset = strtab_add(&symtab->strtab, name);
  if (name->bytes > 0) {
    for (int i = 0; i < symtab->count; ++i) {
      uintptr_t index;
      if (table_try_get(&symtab->indices, name, (void**)&index)) {
        return &symtab->buf[index];
      }
    }
  }

  int old_count = symtab->count;
  int new_count = old_count + 1;
  symtab->buf = realloc(symtab->buf, sizeof(Elf64_Sym) * new_count);
  symtab->count = new_count;
  Elf64_Sym *sym = &symtab->buf[old_count];
  memset(sym, 0x00, sizeof(*sym));
  sym->st_name = offset;
  table_put(&symtab->indices, name, (void*)(uintptr_t)old_count);
  return sym;
}
#endif  // !defined(__NO_ELF_OBJ)

//

void out_elf_header(FILE *fp, uintptr_t entry, int phnum, int shnum) {
  Elf64_Ehdr ehdr = {
    .e_ident     = { ELFMAG0, ELFMAG1, ELFMAG2 ,ELFMAG3,
                     ELFCLASS64, ELFDATA2LSB, EV_CURRENT, ELFOSABI_SYSV },
    .e_type      = phnum > 0 ? ET_EXEC : ET_REL,
    .e_machine   = EM_X86_64,
    .e_version   = EV_CURRENT,
    .e_entry     = entry,
    .e_phoff     = phnum > 0 ? sizeof(Elf64_Ehdr) : 0,
    .e_shoff     = 0, // dummy
    .e_flags     = 0x0,
    .e_ehsize    = sizeof(Elf64_Ehdr),
    .e_phentsize = phnum > 0 ? sizeof(Elf64_Phdr) : 0,
    .e_phnum     = phnum,
#if !defined(__NO_ELF_OBJ)
    .e_shentsize = shnum > 0 ? sizeof(Elf64_Shdr) : 0,
#else
    .e_shentsize = 0,
#endif  // !defined(__NO_ELF_OBJ)
    .e_shnum     = shnum,
    .e_shstrndx  = shnum > 0 ? shnum - 1 : 0,
  };

  fwrite(&ehdr, sizeof(Elf64_Ehdr), 1, fp);
}

void out_program_header(FILE *fp, int sec, uintptr_t offset, uintptr_t vaddr, size_t filesz,
                        size_t memsz) {
  static const int kFlags[] = {
    PF_R | PF_X,  // code
    PF_R | PF_W,  // rwdata
  };

  Elf64_Phdr phdr = {
    .p_type   = PT_LOAD,
    .p_offset = offset,
    .p_vaddr  = vaddr,
    .p_paddr  = 0, // dummy
    .p_filesz = filesz,
    .p_memsz  = memsz,
    .p_flags  = kFlags[sec],
    .p_align  = 0x10,
  };

  fwrite(&phdr, sizeof(Elf64_Phdr), 1, fp);
}

#endif  // !ELF_NOT_SUPPORTED
