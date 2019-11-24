#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdint.h>  // uintptr_t
#include <stdio.h>
#include <stdlib.h>  // malloc, calloc
#include <string.h>
#include <strings.h>  // strncasecmp

#include "asm_x86.h"
#include "elfutil.h"
#include "gen.h"
#include "ir_asm.h"
#include "parse_asm.h"
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

#include <unistd.h>
#define AS_USE_CC

#endif

#define LOAD_ADDRESS    START_ADDRESS

#if !defined(AS_USE_CC)
void parse_file(FILE *fp, Vector **section_irs, Map *label_map) {
  for (;;) {
    char *rawline = NULL;
    size_t capa = 0;
    ssize_t len = getline_(&rawline, &capa, fp, 0);
    if (len == EOF)
      break;

    Vector *irs = section_irs[current_section];
    Line *line = parse_line(rawline);

    if (line->label != NULL) {
      vec_push(irs, new_ir_label(line->label));

      void *address;
      if (map_try_get(label_map, line->label, &address)) {
        fprintf(stderr, "`%s' already defined\n", line->label);
        err = true;
      }
      map_put(label_map, line->label, NULL);
    }

    if (line->dir == NODIRECTIVE) {
      Code code;
      assemble_inst(&line->inst, line->rawline, &code);
      if (code.len > 0)
        vec_push(irs, new_ir_code(&code));
    } else {
      handle_directive(line->dir, line->directive_line, section_irs);
    }
  }
}

static void put_padding(FILE* fp, uintptr_t start) {
  long cur = ftell(fp);
   if (start > (size_t)cur) {
    size_t size = start - (uintptr_t)cur;
    char* buf = calloc(1, size);
    fwrite(buf, size, 1, fp);
    free(buf);
  }
}
#endif  // !AS_USE_CC

// ================================================

int main(int argc, char* argv[]) {
  const char *ofn = "a.out";
  int iarg;

  for (iarg = 1; iarg < argc; ++iarg) {
    if (*argv[iarg] != '-')
      break;
    if (strncmp(argv[iarg], "-o", 2) == 0)
      ofn = strdup_(argv[iarg] + 2);
  }

#if !defined(AS_USE_CC)
  // ================================================
  // Run own assembler

  FILE* fp = fopen(ofn, "wb");
  if (fp == NULL) {
    fprintf(stderr, "Failed to open output file: %s\n", ofn);
    return 1;
  }

  Vector *section_irs[SECTION_COUNT];
  Map *label_map = new_map();
  for (int i = 0; i < SECTION_COUNT; ++i)
    section_irs[i] = new_vector();

  if (iarg < argc) {
    for (int i = iarg; i < argc; ++i) {
      FILE *fp = fopen(argv[i], "r");
      if (fp == NULL)
        error("Cannot open %s\n", argv[i]);
      parse_file(fp, section_irs, label_map);
      fclose(fp);
      if (err)
        break;
    }
  } else {
    parse_file(stdin, section_irs, label_map);
  }

  if (!err) {
    do {
      calc_label_address(LOAD_ADDRESS, section_irs, label_map);
    } while (!resolve_relative_address(section_irs, label_map));
    emit_irs(section_irs, label_map);
  }

  if (err) {
    if (fp != NULL) {
      fclose(fp);
      remove(ofn);
    }
    return 1;
  }

  fix_section_size(LOAD_ADDRESS);

  size_t codefilesz, codememsz;
  size_t datafilesz, datamemsz;
  uintptr_t codeloadadr, dataloadadr;
  get_section_size(SEC_CODE, &codefilesz, &codememsz, &codeloadadr);
  get_section_size(SEC_DATA, &datafilesz, &datamemsz, &dataloadadr);

  void *entry;
  if (!map_try_get(label_map, "_start", &entry))
    error("Cannot find label: `%s'", "_start");

  int phnum = datamemsz > 0 ? 2 : 1;

  out_elf_header(fp, (uintptr_t)entry, phnum);
  out_program_header(fp, 0, PROG_START, codeloadadr, codefilesz, codememsz);
  if (phnum > 1)
    out_program_header(fp, 1, ALIGN(PROG_START + codefilesz, 0x1000), dataloadadr, datafilesz, datamemsz);

  put_padding(fp, PROG_START);
  output_section(fp, SEC_CODE);
  if (phnum > 1) {
    put_padding(fp, ALIGN(PROG_START + codefilesz, 0x1000));
    output_section(fp, SEC_DATA);
  }
  fclose(fp);

#if !defined(__XV6) && defined(__linux__)
  if (chmod(ofn, 0755) == -1) {
    perror("chmod failed\n");
    return 1;
  }
#endif

#else  // AS_NOT_SUPPORTED
  // ================================================
  // Run system's cc.

  Vector *cc_args = new_vector();
  vec_push(cc_args, "cc");
  vec_push(cc_args, "-o");
  vec_push(cc_args, ofn);

  char temp_file_name[FILENAME_MAX + 2];
  if (iarg >= argc) {
    // Read from stdin and write to temporary file.
    char *tmpdir = getenv("TMPDIR");
    if (tmpdir == NULL)
      tmpdir = "/tmp";

    snprintf(temp_file_name, sizeof(temp_file_name), "%s/as_XXXXXX", tmpdir);
    mkstemp(temp_file_name);
    strcat(temp_file_name, ".s");
    FILE *tmpfp = fopen(temp_file_name, "w");
    if (tmpfp == NULL)
      error("Failed to open temporary file");
    for (;;) {
      static char buf[4096];
      size_t size = fread(buf, 1, sizeof(buf), stdin);
      if (size > 0)
        fwrite(buf, 1, size, tmpfp);
      if (size < sizeof(buf))
        break;
    }
    fclose(tmpfp);

    vec_push(cc_args, temp_file_name);
  } else {
    for (int i = iarg; i < argc; ++i) {
      vec_push(cc_args, argv[i]);
    }
  }

  vec_push(cc_args, NULL);
  if (execvp(cc_args->data[0], (char *const*)cc_args->data) < 0)
    error("Failed to call cc");
#endif
  return 0;
}
#include "asm_x86.h"

#include <assert.h>
#include <stdlib.h>  // exit
#include <string.h>  // memcpy

#include "inst.h"
#include "util.h"

#ifndef PUT_CODE
#define PUT_CODE(p, ...)  do { unsigned char buf[] = {__VA_ARGS__}; memcpy(p, buf, sizeof(buf)); } while (0)
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

static bool assemble_error(const char *rawline, const char *message) {
  fprintf(stderr, "%s\n", message);
  fprintf(stderr, "%s\n", rawline);
  return false;
}

static unsigned char *put_rex0(unsigned char *p, enum RegSize size,
                               int sno, int dno, unsigned char opcode)
{
  if (size == REG16)
    *p++ = 0x66;
  if (sno >= 8 || dno >= 8 ||
      (size == REG8 && (sno >= 4 || dno >= 4)) ||
      size == REG64)
    *p++ = 0x40 | ((dno & 8) >> 3) | ((sno & 8) >> 1) | (size != REG64 ? 0 : 8);
  *p++ = opcode;
  return p;
}

static unsigned char *put_rex2(unsigned char *p, enum RegSize size,
                               int sno, int dno, unsigned char opcode)
{
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

static bool assemble_mov(Inst *inst, const char *rawline, Code *code) {
  unsigned char *p = code->buf;

  if (inst->src.type == REG && inst->dst.type == REG) {
    if (inst->dst.reg.size != inst->src.reg.size)
      return assemble_error(rawline, "Different source and destination register size");

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
    if (inst->src.indirect.label == NULL) {
      if (inst->src.indirect.reg.no != RIP) {
        long offset = inst->src.indirect.offset;
        enum RegSize size = inst->dst.reg.size;
        p = put_rex_indirect(
            p, size,
            opr_regno(&inst->dst.reg),
            opr_regno(&inst->src.indirect.reg),
            0x8a, 0x00, offset);
      }
    }
  } else if (inst->src.type == REG && inst->dst.type == INDIRECT) {
    if (inst->dst.indirect.label == NULL) {
      if (inst->dst.indirect.reg.no != RIP) {
        long offset = inst->dst.indirect.offset;
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

  return assemble_error(rawline, "Illegal operand");
}

bool assemble_inst(Inst *inst, const char *rawline, Code *code) {
  unsigned char *p = code->buf;

  code->flag = 0;
  code->len = 0;

  switch(inst->op) {
  case NOOP:
    return true;
  case MOV:
    return assemble_mov(inst, rawline, code);
  case MOVSX:
    if (inst->src.type == REG && inst->dst.type == REG) {
      int s = inst->src.reg.no;
      int d = inst->dst.reg.no;
      switch (inst->src.reg.size) {
      case REG8:
        switch (inst->dst.reg.size) {
        case REG32:
          if (opr_reg8(&inst->src.reg) && !inst->dst.reg.x) {
            MAKE_CODE(inst, code, 0x0f, 0xbe, 0xc0 + s + d * 8);
          } else {
            int pre = (!inst->src.reg.x ? 0x40 : 0x41) + (!inst->dst.reg.x ? 0 : 4);
            MAKE_CODE(inst, code, pre, 0x0f, 0xbe, 0xc0 + s + d * 8);
          }
          return true;
        case REG64:
          {
            int pre = (!inst->src.reg.x ? 0x48 : 0x49) + (!inst->dst.reg.x ? 0 : 4);
            MAKE_CODE(inst, code, pre, 0x0f, 0xbe, 0xc0 + s + d * 8);
            return true;
          }
        default:
          break;
        }
      case REG16:
        switch (inst->dst.reg.size) {
        case REG32:
          if (!inst->src.reg.x && !inst->dst.reg.x) {
            MAKE_CODE(inst, code, 0x0f, 0xbf, 0xc0 + s + d * 8);
          } else {
            int pre = (!inst->src.reg.x ? 0x40 : 0x41) + (!inst->dst.reg.x ? 0 : 4);
            MAKE_CODE(inst, code, pre, 0x0f, 0xbf, 0xc0 + s + d * 8);
          }
          return true;
        case REG64:
          {
            int pre = (!inst->src.reg.x ? 0x48 : 0x49) + (!inst->dst.reg.x ? 0 : 4);
            MAKE_CODE(inst, code, pre, 0x0f, 0xbf, 0xc0 + s + d * 8);
            return true;
          }
        default:
          break;
        }
      case REG32:
        if (inst->dst.reg.size == REG64) {
          int pre = (!inst->src.reg.x ? 0x48 : 0x49) + (!inst->dst.reg.x ? 0 : 4);
          MAKE_CODE(inst, code, pre, 0x63, 0xc0 + s + d * 8);
          return true;
        }
      default:
        break;
      }
      return assemble_error(rawline, "Illegal operand");
    }
    break;
  case LEA:
    if (inst->src.type == INDIRECT && inst->dst.type == REG) {
      if (inst->dst.reg.size != REG64)
        return assemble_error(rawline, "64 bit register expected for destination");

      int d = inst->dst.reg.no;
      if (inst->src.indirect.reg.no != RIP) {
        if (inst->src.indirect.label == NULL) {
          long offset = inst->src.indirect.offset;
          enum RegSize size = inst->dst.reg.size;
          p = put_rex_indirect(
              p, size,
              opr_regno(&inst->dst.reg),
              opr_regno(&inst->src.indirect.reg),
              0x8c, 0x00, offset);
        }
      } else {
        int pre = !inst->dst.reg.x ? 0x48 : 0x4c;
        if (inst->src.indirect.offset == 0) {
          MAKE_CODE(inst, code, pre, 0x8d, 0x05 | (d << 3), IM32(-1));
          return true;
        }
      }
    }
    break;
  case ADD:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.reg.size != inst->src.reg.size)
        return assemble_error(rawline, "Different source and destination register size");

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
          p = put_rex2(p, size, 0, d, im8 ? 0x83 : 0x81);

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
      if (inst->src.indirect.label == NULL &&
          inst->src.indirect.reg.no != RIP) {
        long offset = inst->src.indirect.offset;
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
      if (inst->dst.indirect.label == NULL) {
        long value = inst->src.immediate;
        if (is_im32(value)) {
          long offset = inst->dst.indirect.offset;
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
        return assemble_error(rawline, "Different source and destination register size");

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
          p = put_rex2(p, size, 5, d, im8 ? 0x83 : 0x81);  // 0xe8 = 0xc0 | (5 << 3)

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
      if (inst->dst.indirect.label == NULL) {
        long value = inst->src.immediate;
        if (is_im32(value)) {
          long offset = inst->dst.indirect.offset;
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
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == REG) {
      enum RegSize size = inst->src.reg.size;
      p = put_rex0(p, size, 0, opr_regno(&inst->src.reg),
                   0xf6 | (size == REG8 ? 0 : 1));
      *p++ = 0xe0 | inst->src.reg.no;
    }
    break;
  case IDIV:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == REG) {
      enum RegSize size = inst->src.reg.size;
      p = put_rex0(p, size, 0, opr_regno(&inst->src.reg),
                   0xf6 | (size == REG8 ? 0 : 1));
      *p++ = 0xf8 | inst->src.reg.no;
    }
    break;
  case NEG:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == REG) {
      p = put_rex2(p, inst->src.reg.size,
                   3, opr_regno(&inst->src.reg), 0xf7);  // 0xd8 = 0xc0 | (3 << 3)
    }
    break;
  case NOT:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == REG) {
      p = put_rex2(p, inst->src.reg.size,
                   2, opr_regno(&inst->src.reg), 0xf7);  // 0xd0 = 0xc0 | (2 << 3)
    }
    break;
  case INC:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == REG) {
      p = put_rex2(p, inst->src.reg.size,
                   0, opr_regno(&inst->src.reg), 0xff);
    }
    break;
  case INCL:
  case INCQ:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == INDIRECT &&
        inst->src.indirect.reg.no != RIP) {
      enum RegSize size = inst->op + (REG32 - INCL);
      long offset = inst->src.indirect.offset;
      p = put_rex_indirect(
          p, size,
          0, opr_regno(&inst->src.indirect.reg),
          0xfe, 0x00, offset);
    }
    break;
  case DEC:
    if (inst->src.type == NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == REG) {
      p = put_rex2(p, inst->src.reg.size,
                   1, opr_regno(&inst->src.reg), 0xff);  // 0xc8 = 0xc0 | (1 << 3)
    }
    break;
  case DECL:
  case DECQ:
    if (inst->src.type == NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == INDIRECT &&
        inst->src.indirect.reg.no != RIP) {
      enum RegSize size = inst->op + (REG32 - DECL);
      long offset = inst->src.indirect.offset;
      p = put_rex_indirect(
          p, size,
          0, opr_regno(&inst->src.indirect.reg),
          0xfe, 0x08, offset);
    }
    break;
  case AND:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.reg.size != inst->src.reg.size)
        return assemble_error(rawline, "Different source and destination register size");

      enum RegSize size = inst->src.reg.size;
      p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x20 : 0x21);
    }
    break;
  case OR:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.reg.size != inst->src.reg.size)
        return assemble_error(rawline, "Different source and destination register size");

      enum RegSize size = inst->src.reg.size;
      p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x08 : 0x09);
    }
    break;
  case XOR:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.reg.size != inst->src.reg.size)
        return assemble_error(rawline, "Different source and destination register size");

      enum RegSize size = inst->src.reg.size;
      p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x30 : 0x31);
    }
    break;
  case SHL:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (opr_regno(&inst->src.reg) != CL - AL)
        return assemble_error(rawline, "`%cl` expected");

      enum RegSize size = inst->dst.reg.size;
      p = put_rex2(p, size, 4, opr_regno(&inst->dst.reg),  // 0xe0 = 0xc0 | (4 << 3)
                   size == REG8 ? 0xd2 : 0xd3);
    }
    break;
  case SHR:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (opr_regno(&inst->src.reg) != CL - AL)
        return assemble_error(rawline, "`%cl` expected");

      enum RegSize size = inst->dst.reg.size;
      p = put_rex2(p, size, 5, opr_regno(&inst->dst.reg),  // 0xe8 = 0xc0 | (5 << 3)
                   size == REG8 ? 0xd2 : 0xd3);
    }
    break;
  case CMP:
    if (inst->src.type == REG && inst->dst.type == REG) {
      if (inst->dst.reg.size != inst->src.reg.size)
        return assemble_error(rawline, "Different source and destination register size");

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
          p = put_rex0(p, size, 0, d, im8 ? 0x3c : 0x3d);
        else
          p = put_rex2(p, size, 7, d, im8 ? 0x83 : 0x81);  // 0xf8 = 0xc0 | (7 << 3)

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
        return assemble_error(rawline, "Different source and destination register size");

      enum RegSize size = inst->src.reg.size;
      p = put_rex2(p, size, opr_regno(&inst->src.reg), opr_regno(&inst->dst.reg),
                   size == REG8 ? 0x84 : 0x85);
    }
    break;
  case CLTD:
    if (inst->src.type != NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    MAKE_CODE(inst, code, 0x99);
    return true;
  case CQTO:
    if (inst->src.type != NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    MAKE_CODE(inst, code, 0x48, 0x99);
    return true;

  case SETO: case SETNO: case SETB:  case SETAE:
  case SETE: case SETNE: case SETBE: case SETA:
  case SETS: case SETNS: case SETP:  case SETNP:
  case SETL: case SETGE: case SETLE: case SETG:
    if (inst->dst.type != NOOPERAND || inst->src.type != REG ||
        inst->src.reg.size != REG8)
      return assemble_error(rawline, "Illegal opeand");

    p = put_rex0(p, REG8, 0, opr_regno(&inst->src.reg), 0x0f);
    *p++ = 0x90 | (inst->op - SETO);
    *p++ = 0xc0 | inst->src.reg.no;
    break;
  case PUSH:
    if (inst->dst.type != NOOPERAND || inst->src.type != REG ||
        inst->src.reg.size != REG64)
      return assemble_error(rawline, "Illegal operand");

    p = put_rex0(p, REG32, 0, opr_regno(&inst->src.reg),
                 0x50 | inst->src.reg.no);
    break;
  case POP:
    if (inst->dst.type != NOOPERAND || inst->src.type != REG ||
        inst->src.reg.size != REG64)
      return assemble_error(rawline, "Illegal operand");

    p = put_rex0(p, REG32, 0, opr_regno(&inst->src.reg),
                 0x58 | inst->src.reg.no);
    break;
  case JMP:
    if (inst->src.type != LABEL || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");
    //MAKE_CODE(inst, code, 0xe9, IM32(-1));
    MAKE_CODE(inst, code, 0xeb, IM8(-1));  // Short jmp in default.
    return true;
  case JO: case JNO: case JB:  case JAE:
  case JE: case JNE: case JBE: case JA:
  case JS: case JNS: case JP:  case JNP:
  case JL: case JGE: case JLE: case JG:
    if (inst->src.type != LABEL || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    //MAKE_CODE(inst, code, 0x0f, 0x80 + (inst->op - JO), IM32(-1));
    MAKE_CODE(inst, code, 0x70 + (inst->op - JO), IM8(-1));  // Short jmp in default.
    return true;
  case CALL:
    if (inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == LABEL) {
      MAKE_CODE(inst, code, 0xe8, IM32(-1));
      return true;
    } if (inst->src.type == DEREF_REG) {
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
      return assemble_error(rawline, "Illegal operand");

    MAKE_CODE(inst, code, 0xc3);
    return true;
  case INT:
    if (inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    if (inst->src.type == IMMEDIATE) {
      long value = inst->src.immediate;
      MAKE_CODE(inst, code, 0xcd, IM8(value));
      return true;
    }
    return true;
  case SYSCALL:
    if (inst->src.type != NOOPERAND || inst->dst.type != NOOPERAND)
      return assemble_error(rawline, "Illegal operand");

    MAKE_CODE(inst, code, 0x0f, 0x05);
    return true;
  default:
    break;
  }

  if (p > code->buf) {
    code->inst = inst;
    code->len = p - code->buf;
    assert((size_t)code->len <= sizeof(code->buf));
    return true;
  }

  fprintf(stderr, "op=%2d: not handled\n", inst->op);
  return false;
}
#include "parse_asm.h"

#include <ctype.h>
#include <stddef.h>
#include <stdlib.h>  // strtol
#include <string.h>
#include <strings.h>

#include "gen.h"
#include "ir_asm.h"
#include "util.h"

static const char *kOpTable[] = {
  "mov",
  "movsx",
  "lea",

  "add",
  "addq",
  "sub",
  "subq",
  "mul",
  "idiv",
  "neg",
  "not",
  "inc",
  "incl",
  "incq",
  "dec",
  "decl",
  "decq",
  "and",
  "or",
  "xor",
  "shl",
  "shr",
  "cmp",
  "test",
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
};

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

const char *skip_whitespace(const char *p) {
  while (isspace(*p))
    ++p;
  return p;
}

static int find_match_index(const char **pp, const char **table, size_t count) {
  const char *p = *pp;
  const char *start = p;

  while (isalpha(*p))
    ++p;
  if (*p == '\0' || isspace(*p)) {
    size_t n = p - start;
    for (size_t i = 0; i < count; ++i) {
      const char *name = table[i];
      size_t len = strlen(name);
      if (n == len && strncasecmp(start, name, n) == 0) {
        *pp = skip_whitespace(p);
        return i;
      }
    }
  }
  return -1;
}

enum Opcode parse_opcode(const char **pp) {
  return find_match_index(pp, kOpTable, sizeof(kOpTable) / sizeof(*kOpTable)) + 1;
}

enum DirectiveType parse_directive(const char **pp) {
  return find_match_index(pp, kDirectiveTable, sizeof(kDirectiveTable) / sizeof(*kDirectiveTable)) + 1;
}

enum RegType parse_register(const char **pp) {
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

bool parse_immediate(const char **pp, long *value) {
  const char *p = *pp;
  bool negative = false;
  if (*p == '-') {
    negative = true;
    ++p;
  }
  if (!isdigit(*p))
    return false;
  long v = strtol(p, (char**)pp, 10);
  *value = negative ? -v : v;
  return true;
}

static bool is_label_first_chr(char c) {
  return isalpha(c) || c == '_' || c == '.';
}

static bool is_label_chr(char c) {
  return is_label_first_chr(c) || isdigit(c);
}

const char *parse_label(const char **pp) {
  const char *p = *pp;
  const char *start = p;
  if (!is_label_first_chr(*p))
    return NULL;

  do {
    ++p;
  } while (is_label_chr(*p));
  *pp = p;
  return strndup_(start, p - start);
}

bool parse_operand(const char **pp, Operand *operand) {
  const char *p = *pp;
  if (*p == '%') {
    *pp = p + 1;
    enum RegType reg = parse_register(pp);
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
      error("Illegal register");
      return false;
    }

    operand->type = REG;
    operand->reg.size = size;
    operand->reg.no = no & 7;
    operand->reg.x = (no & 8) >> 3;
    return true;
  }

  if (*p == '*' && p[1] == '%') {
    *pp = p + 2;
    enum RegType reg = parse_register(pp);
    if (!is_reg64(reg))
      error("Illegal register");

    char no = reg - RAX;
    operand->type = DEREF_REG;
    operand->deref_reg.size = REG64;
    operand->deref_reg.no = no & 7;
    operand->deref_reg.x = (no & 8) >> 3;
    return true;
  }

  if (*p == '$') {
    *pp = p + 1;
    if (!parse_immediate(pp, &operand->immediate))
      error("Syntax error");
    operand->type = IMMEDIATE;
    return true;
  }

  bool has_offset = false;
  long offset = 0;
  const char *label = parse_label(pp);
  if (label == NULL) {
    bool neg = false;
    if (*p == '-') {
      neg = true;
      ++p;
    }
    if (isdigit(*p)) {
      offset = strtol(p, (char**)pp, 10);
      if (*pp > p)
        has_offset = true;
      if (neg)
        offset = -offset;
    } else if (neg) {
      error("Illegal `-'");
    }
  }
  p = skip_whitespace(*pp);
  if (*p != '(') {
    if (label != NULL) {
      operand->type = LABEL;
      operand->label = label;
      *pp = p;
      return true;
    }
    if (has_offset)
      error("direct number not implemented");
  } else {
    if (p[1] == '%') {
      *pp = p + 2;
      enum RegType reg = parse_register(pp);
      if (!(is_reg64(reg) || reg == RIP))
        error("Register expected");
      p = skip_whitespace(*pp);
      if (*p != ')')
        error("`)' expected");
      *pp = ++p;

      char no = reg - RAX;
      operand->type = INDIRECT;
      operand->indirect.reg.size = REG64;
      operand->indirect.reg.no = reg != RIP ? no & 7 : RIP;
      operand->indirect.reg.x = (no & 8) >> 3;
      operand->indirect.label = label;
      operand->indirect.offset = offset;
      return true;
    }
    error("Illegal `('");
  }

  return false;
}

void parse_inst(const char **pp, Inst *inst) {
  const char *p = *pp;
  enum Opcode op = parse_opcode(&p);
  inst->op = op;
  if (op != NOOP) {
    if (parse_operand(&p, &inst->src)) {
      p = skip_whitespace(p);
      if (*p == ',') {
        p = skip_whitespace(p + 1);
        parse_operand(&p, &inst->dst);
        p = skip_whitespace(p);
      }
    }
  }

  *pp = p;
}

int current_section = SEC_CODE;
bool err;

Line *parse_line(const char *rawline) {
  Line *line = malloc(sizeof(*line));
  line->rawline = rawline;
  line->label = NULL;
  line->inst.op = NOOP;
  line->inst.src.type = line->inst.dst.type = NOOPERAND;
  line->dir = NODIRECTIVE;

  const char *p = rawline;
  line->label = parse_label(&p);
  if (line->label != NULL) {
    if (*p != ':')
      error("`:' expected");
    ++p;
  }

  p = skip_whitespace(p);
  if (*p == '.') {
    ++p;
    enum DirectiveType dir = parse_directive(&p);
    if (dir == NODIRECTIVE)
      error("Unknown directive");
    line->dir = dir;
    line->directive_line = p;
  } else if (*p != '\0') {
    parse_inst(&p, &line->inst);
    if (*p != '\0' && !(*p == '/' && p[1] == '/')) {
      fprintf(stderr, "Syntax error: %s\n", p);
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

static size_t unescape_string(const char *p, char *dst) {
  size_t len = 0;
  for (; *p != '"'; ++p, ++len) {
    char c = *p;
    if (c == '\0')
      error("string not closed");
    if (c == '\\') {
      // TODO: Handle \x...
      c = unescape_char(*(++p));
    }
    if (dst != NULL)
      *dst++ = c;
  }
  return len;
}

void handle_directive(enum DirectiveType dir, const char *p, Vector **section_irs) {
  Vector *irs = section_irs[current_section];

  switch (dir) {
  case DT_ASCII:
    {
      if (*p != '"')
        error("`\"' expected");
      ++p;
      size_t len = unescape_string(p, NULL);
      char *str = malloc(len);
      unescape_string(p, str);

      vec_push(irs, new_ir_data(str, len));
    }
    break;

  case DT_COMM:
    {
      const char *label = parse_label(&p);
      if (label == NULL)
        error(".comm: label expected");
      p = skip_whitespace(p);
      if (*p != ',')
        error(".comm: `,' expected");
      p = skip_whitespace(p + 1);
      long count;
      if (!parse_immediate(&p, &count))
        error(".comm: count expected");
      current_section = SEC_BSS;
      irs = section_irs[current_section];
      vec_push(irs, new_ir_label(label));
      vec_push(irs, new_ir_bss(count));
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
      if (!parse_immediate(&p, &align))
        error(".align: number expected");
      vec_push(irs, new_ir_align(align));
    }
    break;

  case DT_BYTE:
  case DT_WORD:
  case DT_LONG:
  case DT_QUAD:
    {
      long value;
      if (parse_immediate(&p, &value)) {
        // TODO: Target endian.
        int size = 1 << (dir - DT_BYTE);
        unsigned char *buf = malloc(size);
        for (int i = 0; i < size; ++i)
          buf[i] = value >> (8 * i);
        vec_push(irs, new_ir_data(buf, size));
      } else {
        const char *label = parse_label(&p);
        if (label != NULL) {
          if (dir == DT_QUAD) {
            vec_push(irs, new_ir_abs_quad(label));
          } else {
            error("label can use only in .quad");
          }
        } else {
          error(".quad: number or label expected");
        }
      }
    }
    break;

  case DT_SECTION:
  case DT_GLOBL:
  case DT_EXTERN:
    break;

  default:
    fprintf(stderr, "Unhandled directive: %d, %s\n", dir, p);
    break;
  }
}
#include "ir_asm.h"

#include <assert.h>
#include <stdlib.h>  // malloc

#include "gen.h"
#include "inst.h"
#include "util.h"

#define WORD_SIZE  (8)

IR *new_ir_label(const char *label) {
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

IR *new_ir_abs_quad(const char *label) {
  IR *ir = malloc(sizeof(*ir));
  ir->kind = IR_ABS_QUAD;
  ir->label = label;
  return ir;
}

static uintptr_t align_next_section(enum SectionType sec, uintptr_t address) {
  static const int kAlignTable[] = {0, 4096, 16};
  int align = kAlignTable[sec];
  if (align > 1)
    address = ALIGN(address, align);
  return address;
}

void calc_label_address(uintptr_t start_address, Vector **section_irs, Map *label_map) {
  uintptr_t address = start_address;
  for (int sec = 0; sec < SECTION_COUNT; ++sec) {
    address = align_next_section(sec, address);

    Vector *irs = section_irs[sec];
    for (int i = 0, len = irs->len; i < len; ++i) {
      IR *ir = irs->data[i];
      ir->address = address;
      switch (ir->kind) {
      case IR_LABEL:
        map_put(label_map, ir->label, (void*)address);
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
        address = ALIGN(address, ir->align);
        break;
      case IR_ABS_QUAD:
        address += WORD_SIZE;
        break;
      default:  assert(false); break;
      }
    }
  }
}

static void put_value(unsigned char *p, intptr_t value, int size) {
  for (int i = 0; i < size; ++i) {
    *p++ = value;
    value >>= 8;
  }
}

static void put_unresolved(Map **pp, const char *label) {
  Map *map = *pp;
  if (map == NULL)
    *pp = map = new_map();

  void *dummy;
  if (map_try_get(map, label, &dummy))
    return;
  map_put(map, label, NULL);
}

bool resolve_relative_address(Vector **section_irs, Map *label_map) {
  Map *unresolved_labels = NULL;
  bool size_upgraded = false;
  for (int sec = 0; sec < SECTION_COUNT; ++sec) {
    Vector *irs = section_irs[sec];
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
                inst->src.indirect.offset == 0 &&
                inst->src.indirect.label != NULL) {
              void *dst;
              if (map_try_get(label_map, inst->src.indirect.label, &dst)) {
                intptr_t offset = (intptr_t)dst - ((intptr_t)address + ir->code.len);
                put_value(ir->code.buf + 3, offset, sizeof(int32_t));
              } else {
                put_unresolved(&unresolved_labels, inst->src.indirect.label);
              }
            }
            break;
          case JMP:
          case JO: case JNO: case JB:  case JAE:
          case JE: case JNE: case JBE: case JA:
          case JS: case JNS: case JP:  case JNP:
          case JL: case JGE: case JLE: case JG:
            if (inst->src.type == LABEL) {
              void *dst;
              if (map_try_get(label_map, inst->src.label, &dst)) {
                intptr_t offset = (intptr_t)dst - ((intptr_t)address + ir->code.len);
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
                put_unresolved(&unresolved_labels, inst->src.label);
              }
            }
            break;
          case CALL:
           if (inst->src.type == LABEL) {
              void *dst;
              if (map_try_get(label_map, inst->src.label, &dst)) {
                intptr_t offset = (intptr_t)dst - ((intptr_t)address + ir->code.len);
                put_value(ir->code.buf + 1, offset, sizeof(int32_t));
              } else {
                put_unresolved(&unresolved_labels, inst->src.label);
              }
            }
            break;
          default:
            break;
          }
        }
        break;
      case IR_ABS_QUAD:
        {
          void *dst;
          if (!map_try_get(label_map, ir->label, &dst))
            put_unresolved(&unresolved_labels, ir->label);
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

  if (unresolved_labels != NULL) {
    for (int i = 0, len = unresolved_labels->keys->len; i < len; ++i) {
      const char *label = unresolved_labels->keys->data[i];
      fprintf(stderr, "Undefined reference: `%s'\n", label);
    }
    exit(1);
  }

  return !size_upgraded;
}

void emit_irs(Vector **section_irs, Map *label_map) {
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
      case IR_ABS_QUAD:
        {
          void *dst;
          bool result = map_try_get(label_map, ir->label, &dst);
          UNUSED(result);
          assert(result);
          assert(sizeof(dst) == WORD_SIZE);
          add_section_data(sec, &dst, sizeof(dst));  // TODO: Target endian
        }
        break;
      default:  assert(false); break;
      }
    }
  }
}
#include "gen.h"

#include "assert.h"
#include "stdlib.h"
#include "string.h"

#include "util.h"

typedef struct {
  uintptr_t start_address;
  Buffer buf;
} Section;

static Section sections[SECTION_COUNT - 1];  // -1 for BBS
static uintptr_t bss_start_address;
static size_t bss_size;
static int bss_align = 1;

typedef struct {
  enum SectionType section;
  uintptr_t offset;
} LabelInfo;

void add_bss(size_t size) {
  bss_size += size;
}

void align_section_size(enum SectionType secno, int align) {
  if (secno != SEC_BSS) {
    Section *sec = &sections[secno];
    buf_align(&sec->buf, align);
  } else {
    if (align > bss_align)
      bss_align = align;
    bss_size = ALIGN(bss_size, align);
  }
}

void add_section_data(enum SectionType secno, const void* data, size_t bytes) {
  assert(secno != SEC_BSS);
  Section *sec = &sections[secno];
  buf_put(&sec->buf, data, bytes);
}

void add_code(const void* buf, size_t bytes) {
  add_section_data(SEC_CODE, buf, bytes);
}

void fix_section_size(uintptr_t start_address) {
  sections[SEC_CODE].start_address = start_address;
  sections[SEC_DATA].start_address = ALIGN(sections[SEC_CODE].start_address + sections[SEC_CODE].buf.size, 4096);
  bss_start_address = sections[SEC_DATA].start_address + ALIGN(sections[SEC_DATA].buf.size, bss_align);
}

void get_section_size(int section, size_t *pfilesz, size_t *pmemsz, uintptr_t *ploadadr) {
  size_t size = sections[section].buf.size;
  *pfilesz = size;
  *ploadadr = sections[section].start_address;
  switch (section) {
  case SEC_CODE:
    *pmemsz = size;
    break;
  case SEC_DATA:
    *pmemsz = ALIGN(size, bss_align) + bss_size;  // Include bss.
    break;
  default:
    assert(!"Illegal");
    break;
  }
}

void output_section(FILE* fp, int section) {
  Section *sec = &sections[section];
  const void *data = sec->buf.data;
  fwrite(data, sec->buf.size, 1, fp);
}
#include "util.h"

#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc
#include <string.h>  // strcmp
#include <assert.h>

char *strdup_(const char *str) {
  return strndup_(str, strlen(str));
}

char *strndup_(const char *str, size_t size) {
  char *dup = malloc(size + 1);
  strncpy(dup, str, size);
  dup[size] = '\0';
  return dup;
}

static char label_prefix[8] = "L";

char *alloc_label(void) {
  static int label_no;
  ++label_no;
  //char buf[sizeof(int) * 3 + 1];
  char buf[32];
  snprintf(buf, sizeof(buf), ".%s%d", label_prefix, label_no);
  return strdup_(buf);
}

void set_local_label_prefix(const char *prefix) {
  if (strlen(prefix) >= sizeof(label_prefix) - 1)
    error("Label prefix too long");
  strncpy(label_prefix, prefix, sizeof(label_prefix));
}

char *cat_path(const char *base_dir, const char *rel_path) {
  if (*rel_path == '/')  // Absolute path?
    return strdup_(rel_path);

  size_t dirlen = strlen(base_dir);
  size_t fnlen = strlen(rel_path);
  char *path = malloc(dirlen + fnlen + 2);
  strcpy(path, base_dir);
  strcpy(path + dirlen, "/");
  strcpy(path + dirlen + 1, rel_path);
  path[dirlen + 1 + fnlen] = '\0';
  return path;
}

ssize_t getline_(char **lineptr, size_t *pcapa, FILE *stream, size_t start) {
  const int ADD = 16;
  ssize_t capa = *pcapa;
  ssize_t size = start;
  char *top = *lineptr;
  for (;;) {
    int c = fgetc(stream);
    if (c == EOF) {
      if (size == 0)
        return EOF;
      break;
    }

    if (size + 1 >= capa) {
      ssize_t newcapa = capa + ADD;
      top = realloc(top, newcapa);
      if (top == NULL) {
        error("Out of memory");
        return EOF;
      }
      capa = newcapa;
    }

    if (c == '\n')
      break;

    assert(size < capa);
    top[size++] = c;
  }

  assert(size < capa);
  top[size] = '\0';
  *lineptr = top;
  *pcapa = capa;
  return size;
}

char *abspath(const char *root, const char *path) {
  if (*path == '/')
    return strdup_(path);

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

void error(const char* fmt, ...) {
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

// Container

#define BUF_MIN    (16 / 2)
#define BUF_ALIGN  (16)

void buf_put(Buffer *buf, const void *data, size_t bytes) {
  size_t size = buf->size;
  size_t newsize = size + bytes;

  if (newsize > buf->capa) {
    size_t newcapa = ALIGN(MAX(newsize, (size_t)BUF_MIN) * 2, BUF_ALIGN);
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

  void* zero = calloc(add, 1);
  buf_put(buf, zero, add);
  free(zero);

  assert(buf->size == aligned_size);
}

Vector *new_vector(void) {
  Vector *vec = malloc(sizeof(Vector));
  vec->data = malloc(sizeof(void *) * 16);
  vec->capacity = 16;
  vec->len = 0;
  return vec;
}

void vec_clear(Vector *vec) {
  vec->len = 0;
}

void vec_push(Vector *vec, const void *elem) {
  if (vec->capacity == vec->len) {
    vec->capacity *= 2;
    vec->data = realloc(vec->data, sizeof(void *) * vec->capacity);
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

bool vec_contains(Vector *vec, void* elem) {
  for (int i = 0, len = vec->len; i < len; ++i) {
    if (vec->data[i] == elem)
      return true;
  }
  return false;
}

//

Map *new_map(void) {
  Map *map = malloc(sizeof(Map));
  map->keys = new_vector();
  map->vals = new_vector();
  return map;
}

void map_clear(Map *map) {
  vec_clear(map->keys);
  vec_clear(map->vals);
}

int map_count(Map *map) {
  return map->keys->len;
}

static int map_find(Map *map, const char *key) {
  for (int i = map->keys->len - 1; i >= 0; --i)
    if (strcmp(map->keys->data[i], key) == 0)
      return i;
  return -1;
}

void map_put(Map *map, const char *key, const void *val) {
  int i = map_find(map, key);
  if (i >= 0) {
    map->vals->data[i] = (void*)val;
  } else {
    vec_push(map->keys, key);
    vec_push(map->vals, val);
  }
}

bool map_remove(Map *map, const char *key) {
  int i = map_find(map, key);
  if (i < 0)
    return false;

  // Compaction
  int d = map->keys->len - (i + 1);
  if (d > 0) {
    memmove(&map->keys->data[i], &map->keys->data[i + 1], d * sizeof(*map->keys->data));
    memmove(&map->vals->data[i], &map->vals->data[i + 1], d * sizeof(*map->vals->data));
  }
  --map->keys->len;
  --map->vals->len;
  return true;
}

void *map_get(Map *map, const char *key) {
  int i = map_find(map, key);
  return i >= 0 ? map->vals->data[i] : NULL;
}

bool map_try_get(Map *map, const char *key, void **output) {
  int i = map_find(map, key);
  if (i < 0)
    return false;
  *output = map->vals->data[i];
  return true;
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
  elem->len = end != NULL ? (size_t)(end - start) : (size_t)strlen(start);
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
#include "elfutil.h"

#ifndef ELF_NOT_SUPPORTED

#include <stdio.h>
#include <stdlib.h>  // calloc

#if defined(__XV6)
// XV6
#include "../kernel/types.h"
#include "../kernel/elf.h"

#elif defined(__linux__)
// Linux
#include <elf.h>

#endif

void out_elf_header(FILE* fp, uintptr_t entry, int phnum) {
  Elf64_Ehdr ehdr = {
    .e_ident     = { ELFMAG0, ELFMAG1, ELFMAG2 ,ELFMAG3,
                     ELFCLASS64, ELFDATA2LSB, EV_CURRENT, ELFOSABI_SYSV },
    .e_type      = ET_EXEC,
    .e_machine   = EM_X86_64,
    .e_version   = EV_CURRENT,
    .e_entry     = entry,
    .e_phoff     = sizeof(Elf64_Ehdr),
    .e_shoff     = 0, // dummy
    .e_flags     = 0x0,
    .e_ehsize    = sizeof(Elf64_Ehdr),
    .e_phentsize = sizeof(Elf64_Phdr),
    .e_phnum     = phnum,
    .e_shentsize = 0, // dummy
    .e_shnum     = 0,
    .e_shstrndx  = 0, // dummy
  };

  fwrite(&ehdr, sizeof(Elf64_Ehdr), 1, fp);
}

void out_program_header(FILE* fp, int sec, uintptr_t offset, uintptr_t vaddr,
                        uintptr_t filesz, uintptr_t memsz) {
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
