// Intermediate Representation

#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t
#include <stdint.h>  // intptr_t

typedef struct BB BB;
typedef struct Function Function;
typedef struct RegAlloc RegAlloc;
typedef struct Type Type;
typedef struct Vector Vector;

#define REG_COUNT  (7 - 1)
#define SPILLED_REG_NO  (REG_COUNT)

// Virtual register

typedef struct VReg {
  int v;
  int r;
  const Type *type;
  int offset;  // Local offset for spilled register.
} VReg;

VReg *new_vreg(int vreg_no, const Type *type);
void vreg_spill(VReg *vreg);

// Intermediate Representation

enum IrKind {
  IR_IMM,   // Immediate value
  IR_BOFS,  // basereg+ofs
  IR_IOFS,  // label(rip)
  IR_LOAD,
  IR_STORE,
  IR_MEMCPY,
  IR_ADD,
  IR_SUB,
  IR_MUL,
  IR_DIV,
  IR_MOD,
  IR_BITAND,
  IR_BITOR,
  IR_BITXOR,
  IR_LSHIFT,
  IR_RSHIFT,
  IR_CMP,
  IR_INC,
  IR_DEC,
  IR_NEG,
  IR_NOT,
  IR_BITNOT,
  IR_SET,   // SETxx: flag => 0 or 1
  IR_TEST,
  IR_JMP,
  IR_PRECALL,
  IR_PUSHARG,
  IR_CALL,
  IR_ADDSP,
  IR_CAST,
  IR_MOV,
  IR_CLEAR,
  IR_RESULT,
  IR_ASM,

  IR_LOAD_SPILLED,
  IR_STORE_SPILLED,
};

enum ConditionKind {
  COND_ANY,
  COND_EQ,
  COND_NE,
  COND_LT,
  COND_LE,
  COND_GE,
  COND_GT,
};

typedef struct {
  enum IrKind kind;
  VReg *dst;
  VReg *opr1;
  VReg *opr2;
  int size;
  intptr_t value;

  union {
    struct {
      const char *label;
      bool global;
    } iofs;
    struct {
      enum ConditionKind cond;
    } set;
    struct {
      BB *bb;
      enum ConditionKind cond;
    } jmp;
    struct {
      const char *label;
      bool *stack_aligned;
      int arg_count;
      bool global;
    } call;
    struct {
      int srcsize;
    } cast;
    struct {
      const char *str;
    } asm_;
  };
} IR;

VReg *new_ir_imm(intptr_t value, const Type *type);
VReg *new_ir_bop(enum IrKind kind, VReg *opr1, VReg *opr2, const Type *type);
VReg *new_ir_unary(enum IrKind kind, VReg *opr, const Type *type);
void new_ir_mov(VReg *dst, VReg *src, int size);
VReg *new_ir_bofs(VReg *src);
VReg *new_ir_iofs(const char *label, bool global);
void new_ir_store(VReg *dst, VReg *src, int size);
void new_ir_memcpy(VReg *dst, VReg *src, int size);
void new_ir_cmp(VReg *opr1, VReg *opr2, int size);
void new_ir_test(VReg *reg, int size);
void new_ir_incdec(enum IrKind kind, VReg *reg, int size, intptr_t value);
VReg *new_ir_set(enum ConditionKind cond);
void new_ir_jmp(enum ConditionKind cond, BB *bb);
void new_ir_precall(int arg_count, bool *stack_aligned);
void new_ir_pusharg(VReg *vreg);
VReg *new_ir_call(const char *label, bool global, VReg *freg, int arg_count, const Type *result_type, bool *stack_aligned);
void new_ir_addsp(int value);
VReg *new_ir_cast(VReg *vreg, const Type *dsttype, int srcsize);
void new_ir_clear(VReg *reg, size_t size);
void new_ir_result(VReg *reg, int size);
void new_ir_asm(const char *asm_);

IR *new_ir_load_spilled(int offset, int size);
IR *new_ir_store_spilled(int offset, int size);

#if !defined(SELF_HOSTING)
void dump_func_ir(Function *func);
#endif

// Register allocator

extern RegAlloc *curra;

// Basci Block:
//   Chunk of IR codes without branching in the middle (except at the bottom).

typedef struct BB {
  struct BB *next;
  const char *label;
  Vector *irs;  // <IR*>

  Vector *in_regs;  // <VReg*>
  Vector *out_regs;  // <VReg*>
  Vector *assigned_regs;  // <VReg*>
} BB;

extern BB *curbb;

BB *new_bb(void);
BB *bb_split(BB *bb);
void bb_insert(BB *bb, BB *cc);

// Basic blocks in a function
typedef struct BBContainer {
  Vector *bbs;  // <BB*>
} BBContainer;

BBContainer *new_func_blocks(void);
void remove_unnecessary_bb(BBContainer *bbcon);
void push_callee_save_regs(Function *func);
void pop_callee_save_regs(Function *func);
void emit_bb_irs(BBContainer *bbcon);
