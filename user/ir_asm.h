// Intermediate Representation for assembly

#pragma once

#include <stddef.h>  // size_t
#include <stdint.h>  // uintptr_t

#include "asm_x86.h"  // Code

typedef struct Expr Expr;
typedef struct Name Name;
typedef struct Table Table;
typedef struct Vector Vector;

#define LF_GLOBAL   (1 << 0)
#define LF_DEFINED  (1 << 1)

typedef struct {
  int section;
  int flag;
  uintptr_t address;
} LabelInfo;

LabelInfo *new_label(int section, uintptr_t address);
bool add_label_table(Table *label_table, const Name *label, int section, bool define, bool global);

enum UnresolvedKind {
  UNRES_EXTERN,
  UNRES_EXTERN_PC32,
  UNRES_OTHER_SECTION,
  UNRES_ABS64,
};

typedef struct {
  const Name *label;
  uintptr_t offset;
  int src_section;
  int add;
  enum UnresolvedKind kind;
} UnresolvedInfo;

typedef struct {
  size_t len;
  unsigned char *buf;
} Data;

enum IrKind {
  IR_LABEL,
  IR_CODE,
  IR_DATA,
  IR_BSS,
  IR_ALIGN,
  IR_EXPR_BYTE,
  IR_EXPR_WORD,
  IR_EXPR_LONG,
  IR_EXPR_QUAD,
};

typedef struct {
  enum IrKind kind;
  union {
    const Name *label;
    Code code;
    Data data;
    const Expr *expr;
    size_t bss;
    int align;
    int section;
  };
  uintptr_t address;
} IR;

IR *new_ir_label(const Name *label);
IR *new_ir_code(const Code *code);
IR *new_ir_data(const void *data, size_t size);
IR *new_ir_bss(size_t size);
IR *new_ir_align(int align);
IR *new_ir_expr(enum IrKind kind, const Expr *expr);

bool calc_label_address(uintptr_t start_address, Vector **section_irs, Table *label_table);
bool resolve_relative_address(Vector **section_irs, Table *label_table, Vector *unresolved);
void emit_irs(Vector **section_irs, Table *label_table);
