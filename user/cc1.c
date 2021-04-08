#define __NO_FLONUM
 #define __NO_ELF_OBJ

#include "ast.h"

#include <assert.h>
#include <stdlib.h>  // malloc

#include "type.h"
#include "util.h"

bool is_const(Expr *expr) {
  // TODO: Handle constant variable.

  switch (expr->kind) {
  case EX_FIXNUM:
#ifndef __NO_FLONUM
  case EX_FLONUM:
#endif
  case EX_STR:
    return true;
  default:
    return false;
  }
}

bool is_zero(Expr *expr) {
  return expr->kind == EX_FIXNUM && expr->fixnum == 0;
}

static Expr *new_expr(enum ExprKind kind, const Type *type, const Token *token) {
  Expr *expr = malloc(sizeof(*expr));
  expr->kind = kind;
  expr->type = type;
  expr->token = token;
  return expr;
}

Expr *new_expr_fixlit(const Type *type, const Token *token, const Fixnum fixnum) {
  assert(type->kind == TY_FIXNUM);
  Expr *expr = new_expr(EX_FIXNUM, type, token);
  expr->fixnum = fixnum;
  return expr;
}

#ifndef __NO_FLONUM
Expr *new_expr_flolit(const Type *type, const Token *token, double flonum) {
  assert(type->kind == TY_FLONUM);
  Expr *expr = new_expr(EX_FLONUM, type, token);
  expr->flonum = flonum;
  return expr;
}
#endif

Expr *new_expr_str(const Token *token, const char *str, size_t size) {
  Type *type = malloc(sizeof(*type));
  type->kind = TY_ARRAY;
  type->qualifier = TQ_CONST;
  type->pa.ptrof = &tyChar;
  type->pa.length = size;

  Expr *expr = new_expr(EX_STR, type, token);
  expr->str.buf = str;
  expr->str.size = size;
  return expr;
}

Expr *new_expr_variable(const Name *name, const Type *type, const Token *token, Scope *scope) {
  Expr *expr = new_expr(EX_VAR, type, token);
  expr->var.name = name;
  expr->var.scope = scope;
  return expr;
}

Expr *new_expr_bop(enum ExprKind kind, const Type *type, const Token *token, Expr *lhs, Expr *rhs) {
  Expr *expr = new_expr(kind, type, token);
  expr->bop.lhs = lhs;
  expr->bop.rhs = rhs;
  return expr;
}

Expr *new_expr_unary(enum ExprKind kind, const Type *type, const Token *token, Expr *sub) {
  Expr *expr = new_expr(kind, type, token);
  expr->unary.sub = sub;
  return expr;
}

Expr *new_expr_deref(const Token *token, Expr *sub) {
  assert(sub->type != NULL);
  assert(sub->type->kind == TY_PTR || sub->type->kind == TY_ARRAY);
  const Type *type = sub->type->pa.ptrof;
  return new_expr_unary(EX_DEREF, type, token, sub);
}

Expr *new_expr_ternary(const Token *token, Expr *cond, Expr *tval, Expr *fval, const Type *type) {
  Expr *expr = new_expr(EX_TERNARY, type, token);
  expr->ternary.cond = cond;
  expr->ternary.tval = tval;
  expr->ternary.fval = fval;
  return expr;
}

Expr *new_expr_member(const Token *token, const Type *type, Expr *target, const Token *ident,
                      int index) {
  Expr *expr = new_expr(EX_MEMBER, type, token);
  expr->member.target = target;
  expr->member.ident = ident;
  expr->member.index = index;
  return expr;
}

Expr *new_expr_funcall(const Token *token, Expr *func, const Type *functype, Vector *args) {
  Expr *expr = new_expr(EX_FUNCALL, functype->func.ret, token);
  expr->funcall.func = func;
  expr->funcall.args = args;
  return expr;
}

Expr *new_expr_cast(const Type *type, const Token *token, Expr *sub) {
  Expr *expr = new_expr(EX_CAST, type, token);
  expr->unary.sub = sub;
  return expr;
}

Expr *new_expr_complit(const Type *type, const Token *token, Expr *var, Vector *inits) {
  Expr *expr = new_expr(EX_COMPLIT, type, token);
  expr->complit.var = var;
  expr->complit.inits = inits;
  return expr;
}

// ================================================

VarDecl *new_vardecl(const Type *type, const Token *ident, Initializer *init, int storage) {
  VarDecl *decl = malloc(sizeof(*decl));
  decl->type = type;
  decl->ident = ident;
  decl->init = init;
  decl->storage = storage;
  return decl;
}

Stmt *new_stmt(enum StmtKind kind, const Token *token) {
  Stmt *stmt = malloc(sizeof(Stmt));
  stmt->kind = kind;
  stmt->token = token;
  return stmt;
}

Stmt *new_stmt_expr(Expr *e) {
  Stmt *stmt = new_stmt(ST_EXPR, e->token);
  stmt->expr = e;
  return stmt;
}

Stmt *new_stmt_block(const Token *token, Vector *stmts, Scope *scope) {
  Stmt *stmt = new_stmt(ST_BLOCK, token);
  stmt->block.scope = scope;
  stmt->block.stmts = stmts;
  return stmt;
}

Stmt *new_stmt_if(const Token *token, Expr *cond, Stmt *tblock, Stmt *fblock) {
  Stmt *stmt = new_stmt(ST_IF, token);
  stmt->if_.cond = cond;
  stmt->if_.tblock = tblock;
  stmt->if_.fblock = fblock;
  return stmt;
}

Stmt *new_stmt_switch(const Token *token, Expr *value) {
  Stmt *stmt = new_stmt(ST_SWITCH, token);
  stmt->switch_.value = value;
  stmt->switch_.body = NULL;
  stmt->switch_.cases = new_vector();
  stmt->switch_.default_ = NULL;
  stmt->switch_.break_bb = NULL;
  return stmt;
}

Stmt *new_stmt_case(const Token *token, Expr *value) {
  Stmt *stmt = new_stmt(ST_CASE, token);
  stmt->case_.value = value;
  stmt->case_.bb = NULL;
  return stmt;
}

Stmt *new_stmt_default(const Token *token) {
  Stmt *stmt = new_stmt(ST_DEFAULT, token);
  stmt->case_.value = NULL;
  stmt->case_.bb = NULL;
  return stmt;
}

Stmt *new_stmt_while(const Token *token, Expr *cond, Stmt *body) {
  Stmt *stmt = new_stmt(ST_WHILE, token);
  stmt->while_.cond = cond;
  stmt->while_.body = body;
  return stmt;
}

Stmt *new_stmt_do_while(Stmt *body, const Token *token, Expr *cond) {
  Stmt *stmt = new_stmt(ST_DO_WHILE, token);
  stmt->while_.body = body;
  stmt->while_.cond = cond;
  return stmt;
}

Stmt *new_stmt_for(const Token *token, Expr *pre, Expr *cond, Expr *post, Stmt *body) {
  Stmt *stmt = new_stmt(ST_FOR, token);
  stmt->for_.pre = pre;
  stmt->for_.cond = cond;
  stmt->for_.post = post;
  stmt->for_.body = body;
  return stmt;
}

Stmt *new_stmt_return(const Token *token, Expr *val) {
  Stmt *stmt = new_stmt(ST_RETURN, token);
  stmt->return_.val = val;
  return stmt;
}

Stmt *new_stmt_goto(const Token *tok, const Token *label) {
  Stmt *stmt = new_stmt(ST_GOTO, tok);
  stmt->goto_.label = label;
  return stmt;
}

Stmt *new_stmt_label(const Token *label, Stmt *follow) {
  Stmt *stmt = new_stmt(ST_LABEL, label);
  stmt->label.stmt = follow;
  return stmt;
}

Stmt *new_stmt_vardecl(Vector *decls, Vector *inits) {
  Stmt *stmt = new_stmt(ST_VARDECL, NULL);
  stmt->vardecl.decls = decls;
  stmt->vardecl.inits = inits;
  return stmt;
}

Stmt *new_stmt_asm(const Token *token, Expr *str) {
  Stmt *stmt = new_stmt(ST_ASM, token);
  stmt->asm_.str = str;
  return stmt;
}

//

static Declaration *new_decl(enum DeclKind kind) {
  Declaration *decl = malloc(sizeof(*decl));
  decl->kind = kind;
  return decl;
}

Declaration *new_decl_defun(Function *func) {
  Declaration *decl = new_decl(DCL_DEFUN);
  decl->defun.func = func;
  return decl;
}

Declaration *new_decl_vardecl(Vector *decls) {
  Declaration *decl = new_decl(DCL_VARDECL);
  decl->vardecl.decls = decls;
  return decl;
}

// ================================================

// Function

Function *new_func(const Type *type, const Name *name) {
  assert(type->kind == TY_FUNC);
  Function *func = malloc(sizeof(*func));
  func->type = type;
  func->name = name;

  func->scopes = NULL;
  func->stmts = NULL;
  func->label_table = NULL;
  func->gotos = NULL;

  func->ra = NULL;
  func->bbcon = NULL;
  func->ret_bb = NULL;
  func->retval = NULL;

  return func;
}
#include <assert.h>

#include "ast.h"
#include "lexer.h"
#include "parser.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

static Expr *proc_builtin_va_start(const Token *ident) {
  consume(TK_LPAR, "`(' expected");

  Token *token;
  Vector *args = parse_args(&token);
  if (args == NULL || args->len != 2) {
    parse_error(token, "two arguments expected");
    return NULL;
  }

  //#define va_start(ap, param)  (void)(ap = (va_list)&(param))

  const Type *tyvalist = find_typedef(curscope, alloc_name("__builtin_va_list", NULL, false), NULL);
  assert(tyvalist != NULL);

  Expr *ap = args->data[0];
  Expr *param = args->data[1];
  Expr *paramref = make_refer(param->token, param);
  Expr *assign = new_expr_bop(EX_ASSIGN, ap->type, ap->token, ap,
                              make_cast(tyvalist, paramref->token, paramref, true));
  return new_expr_cast(&tyVoid, ident, assign);
}

static Expr *proc_builtin_va_end(const Token *ident) {
  consume(TK_LPAR, "`(' expected");

  Token *token;
  Vector *args = parse_args(&token);
  if (args == NULL || args->len != 1) {
    parse_error(token, "one arguments expected");
    return NULL;
  }

  //#define va_end(ap)           (void)(ap = 0)

  Expr *ap = args->data[0];
  Expr *assign = new_expr_bop(EX_ASSIGN, ap->type, ap->token, ap,
                              new_expr_fixlit(&tyInt, ident, 0));
  return new_expr_cast(&tyVoid, ident, assign);
}

static Expr *proc_builtin_va_arg(const Token *ident) {
  consume(TK_LPAR, "`(' expected");
  Expr *ap = parse_assign();
  consume(TK_COMMA, "`,' expected");
  const Type *type = parse_full_type(NULL, NULL);
  consume(TK_RPAR, "`)' expected");

  //#define va_arg(v,l)     (*(type*)(ap += 1))  // Assume little endian

  Expr *add = new_expr_unary(EX_MODIFY, ap->type, ap->token,
                             new_expr_bop(EX_PTRADD, ap->type, ap->token, ap,
                                          new_expr_fixlit(&tySize, ident, 1)));
  Expr *deref = new_expr_deref(ap->token, make_cast(ptrof(type), ap->token, add, true));
  return deref;
}

static Expr *proc_builtin_va_copy(const Token *ident) {
  consume(TK_LPAR, "`(' expected");

  Token *token;
  Vector *args = parse_args(&token);
  if (args == NULL || args->len != 2) {
    parse_error(token, "two arguments expected");
    return NULL;
  }

  //#define va_start(ap, param)  (void)(ap = (va_list)&(param))

  Expr *dst = args->data[0];
  Expr *src = args->data[1];
  Expr *assign = new_expr_bop(EX_ASSIGN, dst->type, dst->token, dst, src);
  return new_expr_cast(&tyVoid, ident, assign);
}

void install_builtins(void) {
  // __builtin_va_list
  {
    const Type *type = ptrof(&tyVoidPtr);
    const Name *name = alloc_name("__builtin_va_list", NULL, false);
    add_typedef(global_scope, name, type);
  }

  add_builtin_expr_ident("__builtin_va_start", proc_builtin_va_start);
  add_builtin_expr_ident("__builtin_va_end", proc_builtin_va_end);
  add_builtin_expr_ident("__builtin_va_arg", proc_builtin_va_arg);
  add_builtin_expr_ident("__builtin_va_copy", proc_builtin_va_copy);
}
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "codegen.h"
#include "emit.h"
#include "emit_code.h"
#include "ir_debug.h"
#include "lexer.h"
#include "parser.h"
#include "type.h"
#include "util.h"
#include "var.h"

////////////////////////////////////////////////

extern void install_builtins(void);

static void init_compiler(FILE *ofp) {
  init_lexer();
  init_global();
  init_emit(ofp);

  //set_fixnum_size(FX_CHAR,  1, 1);
  //set_fixnum_size(FX_SHORT, 2, 2);
  //set_fixnum_size(FX_INT,   4, 4);
  //set_fixnum_size(FX_LONG,  8, 8);
  //set_fixnum_size(FX_LLONG, 8, 8);
  //set_fixnum_size(FX_ENUM,  4, 4);

  install_builtins();
}

static void compile1(FILE *ifp, const char *filename, Vector *toplevel) {
  set_source_file(ifp, filename);
  parse(toplevel);
}

static const char LOCAL_LABEL_PREFIX[] = "--local-label-prefix=";

int main(int argc, char *argv[]) {
  int iarg;
  bool dump_ir = false;

  for (iarg = 1; iarg < argc; ++iarg) {
    char *arg = argv[iarg];
    if (*arg != '-')
      break;

    if (starts_with(arg, LOCAL_LABEL_PREFIX)) {
      set_local_label_prefix(&argv[iarg][sizeof(LOCAL_LABEL_PREFIX) - 1]);
    } else if (strcmp(arg, "--dump-ir") == 0) {
#if !defined(SELF_HOSTING) && !defined(__XV6)
      dump_ir = true;
#else
      fprintf(stderr, "option not supported: %s\n", arg);
      return 1;
#endif
    } else if (strcmp(arg, "--version") == 0) {
      show_version("cc1");
      return 0;
    } else {
      fprintf(stderr, "Unknown option: %s\n", arg);
      return 1;
    }
  }

  // Compile.
  init_compiler(stdout);

  toplevel = new_vector();
  if (iarg < argc) {
    for (int i = iarg; i < argc; ++i) {
      const char *filename = argv[i];
      FILE *ifp = fopen(filename, "r");
      if (ifp == NULL)
        error("Cannot open file: %s\n", filename);
      compile1(ifp, filename, toplevel);
      fclose(ifp);
    }
  } else {
    compile1(stdin, "*stdin*", toplevel);
  }
  gen(toplevel);

  if (!dump_ir) {
    emit_code(toplevel);
  } else {
#if !defined(SELF_HOSTING) && !defined(__XV6)
    do_dump_ir(toplevel);
#endif
  }

  return 0;
}
#include "codegen.h"

#include <assert.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "ir.h"
#include "lexer.h"
#include "regalloc.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

#include "parser.h"  // curscope

const int FRAME_ALIGN = 8;
const int STACK_PARAM_BASE_OFFSET = (2 - MAX_REG_ARGS) * WORD_SIZE;

const char RET_VAR_NAME[] = ".ret";

static void gen_stmt(Stmt *stmt);
static void gen_expr_stmt(Expr *expr);

void set_curbb(BB *bb) {
  assert(curfunc != NULL);
  if (curbb != NULL)
    curbb->next = bb;
  curbb = bb;
  vec_push(curfunc->bbcon->bbs, bb);
}

//

static BB *s_break_bb;
static BB *s_continue_bb;

static void pop_break_bb(BB *save) {
  s_break_bb = save;
}

static void pop_continue_bb(BB *save) {
  s_continue_bb = save;
}

static BB *push_continue_bb(BB **save) {
  *save = s_continue_bb;
  BB *bb = new_bb();
  s_continue_bb = bb;
  return bb;
}

static BB *push_break_bb(BB **save) {
  *save = s_break_bb;
  BB *bb = new_bb();
  s_break_bb = bb;
  return bb;
}

static void alloc_variable_registers(Function *func) {
  assert(func->type->kind == TY_FUNC);

  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = func->scopes->data[i];
    if (scope->vars == NULL)
      continue;

    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      if (varinfo->storage & (VS_STATIC | VS_EXTERN | VS_ENUM_MEMBER)) {
        // Static entity is allocated in global, not on stack.
        // Extern doesn't have its entity.
        // Enum members are replaced to constant value.
        continue;
      }

      VReg *vreg = add_new_reg(varinfo->type, VRF_LOCAL);
      if (varinfo->storage & VS_REF_TAKEN)
        vreg->flag |= VRF_REF;
      varinfo->local.reg = vreg;
    }
  }

  // Handle if return value is on the stack.
  const Type *rettype = func->type->func.ret;
  const Name *retval_name = NULL;
  int param_index_offset = 0;
  if (is_stack_param(rettype)) {
    // Insert vreg for return value pointer into top of the function scope.
    retval_name = alloc_name(RET_VAR_NAME, NULL, false);
    const Type *retptrtype = ptrof(rettype);
    Scope *top_scope = func->scopes->data[0];
    if (top_scope->vars == NULL)
      top_scope->vars = new_vector();
    VarInfo *varinfo = var_add(top_scope->vars, retval_name, retptrtype, 0, NULL);
    VReg *vreg = add_new_reg(varinfo->type, VRF_LOCAL | VRF_PARAM);
    vreg->param_index = 0;
    varinfo->local.reg = vreg;
    func->retval = vreg;
    ++param_index_offset;
  }

  // Add flag to parameters.
  if (func->type->func.params != NULL) {
    for (int j = 0; j < func->type->func.params->len; ++j) {
      VarInfo *varinfo = func->type->func.params->data[j];
      VReg *vreg = varinfo->local.reg;
      vreg->flag |= VRF_PARAM;
      vreg->param_index = j + param_index_offset;
    }
  }
}

static void gen_asm(Stmt *stmt) {
  assert(stmt->asm_.str->kind == EX_STR);
  new_ir_asm(stmt->asm_.str->str.buf);
}

void gen_stmts(Vector *stmts) {
  if (stmts == NULL)
    return;

  for (int i = 0, len = stmts->len; i < len; ++i) {
    Stmt *stmt = stmts->data[i];
    if (stmt == NULL)
      continue;
    gen_stmt(stmt);
  }
}

static void gen_block(Stmt *stmt) {
  if (stmt->block.scope != NULL) {
    assert(curscope == stmt->block.scope->parent);
    curscope = stmt->block.scope;
  }
  gen_stmts(stmt->block.stmts);
  if (stmt->block.scope != NULL)
    curscope = curscope->parent;
}

static void gen_return(Stmt *stmt) {
  assert(curfunc != NULL);
  BB *bb = new_bb();
  if (stmt->return_.val != NULL) {
    Expr *val = stmt->return_.val;
    VReg *reg = gen_expr(val);
    VReg *retval = curfunc->retval;
    if (retval == NULL) {
      new_ir_result(reg);
    } else {
      size_t size = type_size(val->type);
      if (size > 0) {
        // Allocate new register to avoid both spilled.
        VReg *tmp = add_new_reg(&tyVoidPtr, 0);
        new_ir_mov(tmp, reg);
        new_ir_memcpy(retval, tmp, size);
        new_ir_result(retval);
      }
    }
  }
  new_ir_jmp(COND_ANY, curfunc->ret_bb);
  set_curbb(bb);
}

static void gen_if(Stmt *stmt) {
  BB *tbb = new_bb();
  BB *fbb = new_bb();
  gen_cond_jmp(stmt->if_.cond, false, fbb);
  set_curbb(tbb);
  gen_stmt(stmt->if_.tblock);
  if (stmt->if_.fblock == NULL) {
    set_curbb(fbb);
  } else {
    BB *nbb = new_bb();
    new_ir_jmp(COND_ANY, nbb);
    set_curbb(fbb);
    gen_stmt(stmt->if_.fblock);
    set_curbb(nbb);
  }
}

static int compare_cases(const void *pa, const void *pb) {
  const int ia = *(int *)pa;
  const int ib = *(int *)pb;
  Vector *cases = curswitch->switch_.cases;
  Stmt *ca = cases->data[ia];
  Stmt *cb = cases->data[ib];
  if (ca->case_.value == NULL)
    return 1;
  if (cb->case_.value == NULL)
    return -1;
  Fixnum d = ca->case_.value->fixnum - cb->case_.value->fixnum;
  return d > 0 ? 1 : d < 0 ? -1 : 0;
}

static void gen_switch_cond_recur(Stmt *stmt, VReg *reg, const VRegType *vtype, const int *order,
                                  int len) {
  Vector *cases = stmt->switch_.cases;
  if (len <= 2) {
    for (int i = 0; i < len; ++i) {
      BB *nextbb = new_bb();
      int index = order[i];
      Stmt *c = cases->data[index];
      VReg *num = new_const_vreg(c->case_.value->fixnum, vtype);
      new_ir_cmp(reg, num);
      new_ir_jmp(COND_EQ, c->case_.bb);
      set_curbb(nextbb);
    }
    Stmt *def = curswitch->switch_.default_;
    new_ir_jmp(COND_ANY, def != NULL ? def->case_.bb : curswitch->switch_.break_bb);
  } else {
    BB *bbne = new_bb();
    int m = len >> 1;
    int index = order[m];
      Stmt *c = cases->data[index];
    VReg *num = new_const_vreg(c->case_.value->fixnum, vtype);
    new_ir_cmp(reg, num);
    new_ir_jmp(COND_EQ, c->case_.bb);
    set_curbb(bbne);

    BB *bblt = new_bb();
    BB *bbgt = new_bb();
    new_ir_jmp(COND_GT, bbgt);
    set_curbb(bblt);
    gen_switch_cond_recur(stmt, reg, vtype, order, m);
    set_curbb(bbgt);
    gen_switch_cond_recur(stmt, reg, vtype, order + (m + 1), len - (m + 1));
  }
}

static void gen_switch_cond(Stmt *stmt) {
  Expr *value = stmt->switch_.value;
  VReg *reg = gen_expr(value);
  {  // Avoid spilled register.
    VReg *tmp = add_new_reg(value->type, 0);
    new_ir_mov(tmp, reg);
    reg = tmp;
  }

  Vector *cases = stmt->switch_.cases;
  int len = cases->len;
  if (len > 0) {
    // Sort cases in increasing order.
    int *order = malloc(sizeof(int) * len);
    for (int i = 0; i < len; ++i)
      order[i] = i;
    QSORT(order, len, sizeof(int), compare_cases);

    if (stmt->switch_.default_ != NULL)
      --len;  // Ignore default.
    gen_switch_cond_recur(stmt, reg, to_vtype(stmt->switch_.value->type), order, len);
    free(order);
  } else {
    Stmt *def = curswitch->switch_.default_;
    new_ir_jmp(COND_ANY, def != NULL ? def->case_.bb : curswitch->switch_.break_bb);
  }
  set_curbb(new_bb());
}

static void gen_switch(Stmt *stmt) {
  Stmt *save_switch = curswitch;
  BB *save_break;
  BB *break_bb = stmt->switch_.break_bb = push_break_bb(&save_break);

  Vector *cases = stmt->switch_.cases;
  for (int i = 0, len = cases->len; i < len; ++i) {
    BB *bb = new_bb();
    Stmt *c = cases->data[i];
    c->case_.bb = bb;
  }

  curswitch = stmt;

  gen_switch_cond(stmt);

  // No bb setting.

  gen_stmt(stmt->switch_.body);

  set_curbb(break_bb);

  curswitch = save_switch;
  pop_break_bb(save_break);
}

static void gen_case(Stmt *stmt) {
  set_curbb(stmt->case_.bb);
}

static void gen_while(Stmt *stmt) {
  BB *loop_bb = new_bb();

  BB *save_break, *save_cont;
  BB *cond_bb = push_continue_bb(&save_cont);
  BB *next_bb = push_break_bb(&save_break);

  new_ir_jmp(COND_ANY, cond_bb);

  set_curbb(loop_bb);
  gen_stmt(stmt->while_.body);

  set_curbb(cond_bb);
  gen_cond_jmp(stmt->while_.cond, true, loop_bb);

  set_curbb(next_bb);
  pop_continue_bb(save_cont);
  pop_break_bb(save_break);
}

static void gen_do_while(Stmt *stmt) {
  BB *loop_bb = new_bb();

  BB *save_break, *save_cont;
  BB *cond_bb = push_continue_bb(&save_cont);
  BB *next_bb = push_break_bb(&save_break);

  set_curbb(loop_bb);
  gen_stmt(stmt->while_.body);

  set_curbb(cond_bb);
  gen_cond_jmp(stmt->while_.cond, true, loop_bb);

  set_curbb(next_bb);
  pop_continue_bb(save_cont);
  pop_break_bb(save_break);
}

static void gen_for(Stmt *stmt) {
  BB *cond_bb = new_bb();
  BB *body_bb = new_bb();

  BB *save_break, *save_cont;
  BB *continue_bb = push_continue_bb(&save_cont);
  BB *next_bb = push_break_bb(&save_break);

  if (stmt->for_.pre != NULL)
    gen_expr_stmt(stmt->for_.pre);

  set_curbb(cond_bb);

  if (stmt->for_.cond != NULL)
    gen_cond_jmp(stmt->for_.cond, false, next_bb);

  set_curbb(body_bb);
  gen_stmt(stmt->for_.body);

  set_curbb(continue_bb);
  if (stmt->for_.post != NULL)
    gen_expr_stmt(stmt->for_.post);
  new_ir_jmp(COND_ANY, cond_bb);

  set_curbb(next_bb);
  pop_continue_bb(save_cont);
  pop_break_bb(save_break);
}

static void gen_break(void) {
  assert(s_break_bb != NULL);
  BB *bb = new_bb();
  new_ir_jmp(COND_ANY, s_break_bb);
  set_curbb(bb);
}

static void gen_continue(void) {
  assert(s_continue_bb != NULL);
  BB *bb = new_bb();
  new_ir_jmp(COND_ANY, s_continue_bb);
  set_curbb(bb);
}

static void gen_goto(Stmt *stmt) {
  assert(curfunc->label_table != NULL);
  BB *bb = table_get(curfunc->label_table, stmt->goto_.label->ident);
  assert(bb != NULL);
  new_ir_jmp(COND_ANY, bb);
}

static void gen_label(Stmt *stmt) {
  assert(curfunc->label_table != NULL);
  BB *bb = table_get(curfunc->label_table, stmt->token->ident);
  assert(bb != NULL);
  set_curbb(bb);
  gen_stmt(stmt->label.stmt);
}

static void gen_clear_local_var(const VarInfo *varinfo) {
  // Fill with zeros regardless of variable type.
  size_t size = type_size(varinfo->type);
  if (size <= 0)
    return;
  VReg *reg = new_ir_bofs(varinfo->local.reg);
  new_ir_clear(reg, size);
}

static void gen_vardecl(Vector *decls, Vector *inits) {
  if (curfunc != NULL) {
    for (int i = 0; i < decls->len; ++i) {
      VarDecl *decl = decls->data[i];
      if (decl->init == NULL)
        continue;
      VarInfo *varinfo = scope_find(curscope, decl->ident->ident, NULL);
      if (varinfo == NULL || (varinfo->storage & (VS_STATIC | VS_EXTERN)) ||
          !(varinfo->type->kind == TY_STRUCT ||
            varinfo->type->kind == TY_ARRAY))
        continue;
      gen_clear_local_var(varinfo);
    }
  }
  gen_stmts(inits);
}

static void gen_expr_stmt(Expr *expr) {
  gen_expr(expr);
}

void gen_stmt(Stmt *stmt) {
  if (stmt == NULL)
    return;

  switch (stmt->kind) {
  case ST_EXPR:  gen_expr_stmt(stmt->expr); break;
  case ST_RETURN:  gen_return(stmt); break;
  case ST_BLOCK:  gen_block(stmt); break;
  case ST_IF:  gen_if(stmt); break;
  case ST_SWITCH:  gen_switch(stmt); break;
  case ST_CASE: case ST_DEFAULT:  gen_case(stmt); break;
  case ST_WHILE:  gen_while(stmt); break;
  case ST_DO_WHILE:  gen_do_while(stmt); break;
  case ST_FOR:  gen_for(stmt); break;
  case ST_BREAK:  gen_break(); break;
  case ST_CONTINUE:  gen_continue(); break;
  case ST_GOTO:  gen_goto(stmt); break;
  case ST_LABEL:  gen_label(stmt); break;
  case ST_VARDECL:  gen_vardecl(stmt->vardecl.decls, stmt->vardecl.inits); break;
  case ST_ASM:  gen_asm(stmt); break;

  default:
    error("Unhandled stmt: %d", stmt->kind);
    break;
  }
}

////////////////////////////////////////////////

static void gen_defun(Function *func) {
  if (func->scopes == NULL)  // Prototype definition
    return;

  curfunc = func;
  func->bbcon = new_func_blocks();
  set_curbb(new_bb());
  func->ra = curra = new_reg_alloc(PHYSICAL_REG_MAX);
#ifndef __NO_FLONUM
  func->ra->fphys_max = PHYSICAL_FREG_MAX;
#endif

  // Allocate BBs for goto labels.
  if (func->label_table != NULL) {
    Table *label_table = func->label_table;
    for (int i = 0;;) {
      const Name *name;
      i = table_iterate(label_table, i, &name, NULL);
      if (i < 0)
        break;
      table_put(label_table, name, new_bb());
    }
  }

  alloc_variable_registers(func);

  curscope = func->scopes->data[0];
  func->ret_bb = new_bb();

  // Statements
  gen_stmts(func->stmts);

  set_curbb(func->ret_bb);
  curbb = NULL;

  prepare_register_allocation(func);
  convert_3to2(func->bbcon);
  alloc_physical_registers(func->ra, func->bbcon);

  remove_unnecessary_bb(func->bbcon);

  curfunc = NULL;
  curscope = global_scope;
  curra = NULL;
}

void gen_decl(Declaration *decl) {
  if (decl == NULL)
    return;

  switch (decl->kind) {
  case DCL_DEFUN:
    gen_defun(decl->defun.func);
    break;
  case DCL_VARDECL:
    break;

  default:
    error("Unhandled decl: %d", decl->kind);
    break;
  }
}

void gen(Vector *decls) {
  if (decls == NULL)
    return;

  for (int i = 0, len = decls->len; i < len; ++i) {
    Declaration *decl = decls->data[i];
    if (decl == NULL)
      continue;
    gen_decl(decl);
  }
}
#include "codegen.h"

#include <assert.h>
#include <limits.h>  // CHAR_BIT
#include <stdlib.h>  // malloc

#include "ast.h"
#include "ir.h"
#include "lexer.h"
#include "regalloc.h"
#include "type.h"
#include "util.h"
#include "var.h"

#include "parser.h"  // curfunc

VRegType *to_vtype(const Type *type) {
  VRegType *vtype = malloc(sizeof(*vtype));
  vtype->size = type_size(type);
  vtype->align = align_size(type);

  int flag = 0;
  bool is_unsigned = is_fixnum(type->kind) ? type->fixnum.is_unsigned : true;
#ifndef __NO_FLONUM
  if (is_flonum(type)) {
    flag |= VRTF_FLONUM;
    is_unsigned = false;
  }
#endif
  if (is_unsigned)
    flag |= VRTF_UNSIGNED;
  vtype->flag = flag;

  return vtype;
}

VReg *add_new_reg(const Type *type, int flag) {
  return reg_alloc_spawn(curfunc->ra, to_vtype(type), flag);
}

static enum ConditionKind swap_cond(enum ConditionKind cond) {
  assert(COND_EQ <= cond && cond <= COND_GT);
  if (cond >= COND_LT)
    cond = COND_GT - (cond - COND_LT);
  return cond;
}

static enum ConditionKind gen_compare_expr(enum ExprKind kind, Expr *lhs, Expr *rhs) {
  assert(lhs->type->kind == rhs->type->kind);

  enum ConditionKind cond = kind + (COND_EQ - EX_EQ);
  assert(cond >= COND_EQ && cond < COND_ULT);
  if (is_const(lhs)) {
    assert(!is_const(rhs));
    Expr *tmp = lhs;
    lhs = rhs;
    rhs = tmp;
    cond = swap_cond(cond);
  }

  if (cond > COND_NE &&
      ((is_fixnum(lhs->type->kind) && lhs->type->fixnum.is_unsigned) ||
#ifndef __NO_FLONUM
        is_flonum(lhs->type) ||
#endif
       lhs->type->kind == TY_PTR)) {
    // unsigned
    cond += COND_ULT - COND_LT;
  }

  VReg *lhs_reg = gen_expr(lhs);
  if (rhs->kind == EX_FIXNUM && rhs->fixnum == 0 &&
      (cond == COND_EQ || cond == COND_NE)) {
    new_ir_test(lhs_reg);
  } else if (rhs->kind == EX_FIXNUM &&
             ((is_fixnum(lhs->type->kind) && lhs->type->fixnum.kind < FX_LONG) ||
               is_im32(rhs->fixnum))) {
    VReg *num = new_const_vreg(rhs->fixnum, to_vtype(rhs->type));
    new_ir_cmp(lhs_reg, num);
  } else {
    switch (lhs->type->kind) {
    case TY_FIXNUM: case TY_PTR:
#ifndef __NO_FLONUM
    case TY_FLONUM:
#endif
      break;
    default: assert(false); break;
    }

    VReg *rhs_reg = gen_expr(rhs);
    // Allocate new register to avoid comparing spilled registers.
    VReg *tmp = add_new_reg(lhs->type, 0);
    new_ir_mov(tmp, lhs_reg);
    new_ir_cmp(tmp, rhs_reg);
  }

  return cond;
}

void gen_cond_jmp(Expr *cond, bool tf, BB *bb) {
  enum ExprKind ck = cond->kind;
  switch (ck) {
  case EX_FIXNUM:
    if (cond->fixnum == 0)
      tf = !tf;
    if (tf)
      new_ir_jmp(COND_ANY, bb);
    return;
  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_LE:
  case EX_GE:
  case EX_GT:
    if (!tf) {
      if (ck <= EX_NE)
        ck = (EX_EQ + EX_NE) - ck;
      else
        ck = EX_LT + ((ck - EX_LT) ^ 2);
    }
    new_ir_jmp(gen_compare_expr(ck, cond->bop.lhs, cond->bop.rhs), bb);
    return;
  case EX_LOGAND:
    if (!tf) {
      BB *bb1 = new_bb();
      BB *bb2 = new_bb();
      gen_cond_jmp(cond->bop.lhs, false, bb);
      set_curbb(bb1);
      gen_cond_jmp(cond->bop.rhs, false, bb);
      set_curbb(bb2);
    } else {
      BB *bb1 = new_bb();
      BB *bb2 = new_bb();
      gen_cond_jmp(cond->bop.lhs, false, bb2);
      set_curbb(bb1);
      gen_cond_jmp(cond->bop.rhs, true, bb);
      set_curbb(bb2);
    }
    return;
  case EX_LOGIOR:
    if (tf) {
      BB *bb1 = new_bb();
      BB *bb2 = new_bb();
      gen_cond_jmp(cond->bop.lhs, true, bb);
      set_curbb(bb1);
      gen_cond_jmp(cond->bop.rhs, true, bb);
      set_curbb(bb2);
    } else {
      BB *bb1 = new_bb();
      BB *bb2 = new_bb();
      gen_cond_jmp(cond->bop.lhs, true, bb2);
      set_curbb(bb1);
      gen_cond_jmp(cond->bop.rhs, false, bb);
      set_curbb(bb2);
    }
    return;
  default:
    assert(false);
    break;
  }
}

static VReg *gen_cast(VReg *reg, const Type *dst_type) {
  if (dst_type->kind == TY_VOID)
    return NULL;  // Assume void value is not used.

  if (reg->flag & VRF_CONST) {
#ifndef __NO_FLONUM
    assert(!(reg->vtype->flag & VRTF_FLONUM));  // No const vreg for flonum.
#endif
    intptr_t value = reg->fixnum;
    size_t dst_size = type_size(dst_type);
    if (dst_size < (size_t)reg->vtype->size && dst_size < sizeof(intptr_t)) {
      // Assume that integer is represented in Two's complement
      size_t bit = dst_size * CHAR_BIT;
      intptr_t mask = (-1UL) << bit;
      if (dst_type->kind == TY_FIXNUM && !dst_type->fixnum.is_unsigned &&  // signed
          (value & (1 << (bit - 1))))  // negative
        value |= mask;
      else
        value &= ~mask;
    }

    VRegType *vtype = to_vtype(dst_type);
    return new_const_vreg(value, vtype);
  }

  int dst_size = type_size(dst_type);
  bool lu = dst_type->kind == TY_FIXNUM ? dst_type->fixnum.is_unsigned : dst_type->kind == TY_PTR;
  bool ru = (reg->vtype->flag & VRTF_UNSIGNED) ? true : false;
  if (dst_size == reg->vtype->size && lu == ru
#ifndef __NO_FLONUM
      && is_flonum(dst_type) == ((reg->vtype->flag & VRTF_FLONUM) != 0)
#endif
  )
    return reg;

  return new_ir_cast(reg, to_vtype(dst_type));
}

static VReg *gen_lval(Expr *expr) {
  switch (expr->kind) {
  case EX_VAR:
    {
      Scope *scope;
      const VarInfo *varinfo = scope_find(expr->var.scope, expr->var.name, &scope);
      assert(varinfo != NULL && scope == expr->var.scope);
      if (is_global_scope(scope)) {
        return new_ir_iofs(expr->var.name, (varinfo->storage & VS_STATIC) == 0);
      } else {
        if (varinfo->storage & VS_STATIC)
          return new_ir_iofs(varinfo->static_.gvar->name, false);
        else if (varinfo->storage & VS_EXTERN)
          return new_ir_iofs(expr->var.name, true);
        else
          return new_ir_bofs(varinfo->local.reg);
      }
    }
  case EX_DEREF:
    return gen_expr(expr->unary.sub);
  case EX_MEMBER:
    {
      const Type *type = expr->member.target->type;
      if (ptr_or_array(type))
        type = type->pa.ptrof;
      assert(type->kind == TY_STRUCT);
      const Vector *members = type->struct_.info->members;
      const VarInfo *member = members->data[expr->member.index];

      VReg *reg = gen_expr(expr->member.target);
      if (member->struct_member.offset == 0)
        return reg;
      VRegType *vtype = to_vtype(&tySize);
      VReg *imm = new_const_vreg(member->struct_member.offset, vtype);
      VReg *result = new_ir_bop(IR_ADD, reg, imm, vtype);
      return result;
    }
  case EX_COMPLIT:
    {
      Expr *var = expr->complit.var;
      assert(var->var.scope != NULL);
      const VarInfo *varinfo = scope_find(var->var.scope, var->var.name, NULL);
      assert(varinfo != NULL);
      assert(varinfo->local.reg != NULL);
      varinfo->local.reg->flag |= VRF_REF;

      gen_stmts(expr->complit.inits);
      return gen_lval(expr->complit.var);
    }
  default:
    assert(false);
    break;
  }
  return NULL;
}

static VReg *gen_variable(Expr *expr) {
  switch (expr->type->kind) {
  case TY_FIXNUM:
  case TY_PTR:
#ifndef __NO_FLONUM
  case TY_FLONUM:
#endif
    {
      Scope *scope;
      const VarInfo *varinfo = scope_find(expr->var.scope, expr->var.name, &scope);
      assert(varinfo != NULL && scope == expr->var.scope);
      if (!is_global_scope(scope) && !(varinfo->storage & (VS_STATIC | VS_EXTERN))) {
        assert(varinfo->local.reg != NULL);
        return varinfo->local.reg;
      }

      VReg *reg = gen_lval(expr);
      VReg *result = new_ir_unary(IR_LOAD, reg, to_vtype(expr->type));
      return result;
    }
  default:
    assert(false);
    // Fallthrough to suppress compile error.
  case TY_ARRAY:   // Use variable address as a pointer.
  case TY_STRUCT:  // struct value is handled as a pointer.
  case TY_FUNC:
    return gen_lval(expr);
  }
}

static VReg *gen_ternary(Expr *expr) {
  BB *tbb = new_bb();
  BB *fbb = new_bb();
  BB *nbb = new_bb();
  bool no_value = expr->type->kind == TY_VOID;

  VReg *result = add_new_reg(expr->type, 0);
  gen_cond_jmp(expr->ternary.cond, false, fbb);

  set_curbb(tbb);
  VReg *tval = gen_expr(expr->ternary.tval);
  if (!no_value)
    new_ir_mov(result, tval);
  new_ir_jmp(COND_ANY, nbb);

  set_curbb(fbb);
  VReg *fval = gen_expr(expr->ternary.fval);
  if (!no_value)
    new_ir_mov(result, fval);

  set_curbb(nbb);
  return result;
}

bool is_stack_param(const Type *type) {
  return type->kind == TY_STRUCT;
}

typedef struct {
  int reg_index;
  int offset;
  int size;
  bool stack_arg;
#ifndef __NO_FLONUM
  bool is_flonum;
#endif
} ArgInfo;

static VReg *gen_funcall(Expr *expr) {
  Expr *func = expr->funcall.func;
  Vector *args = expr->funcall.args;
  int arg_count = args != NULL ? args->len : 0;

  int offset = 0;

  VReg *retvar_reg = NULL;  // Return value is on the stack.
  if (is_stack_param(expr->type)) {
    const Token *ident = alloc_ident(alloc_label(), NULL, NULL);
    VarInfo *ret_varinfo = scope_add(curscope, ident, expr->type, 0);
    ret_varinfo->local.reg = retvar_reg = add_new_reg(expr->type, VRF_LOCAL);
  }

  VRegType **arg_vtypes = (retvar_reg == NULL && arg_count <= 0) ? NULL :
    calloc(arg_count + (retvar_reg != NULL ? 1 : 0), sizeof(*arg_vtypes));

  ArgInfo *arg_infos = NULL;
  int stack_arg_count = 0;
  if (args != NULL) {
    bool vaargs = false;
    if (func->kind == EX_VAR && is_global_scope(func->var.scope)) {
      vaargs = func->type->func.vaargs;
    } else {
      // TODO:
    }

    int arg_start = retvar_reg != NULL ? 1 : 0;
    int ireg_index = arg_start;
#ifndef __NO_FLONUM
    int freg_index = 0;
#endif

    // Check stack arguments.
    arg_infos = malloc(sizeof(*arg_infos) * arg_count);
    for (int i = 0; i < arg_count; ++i) {
      ArgInfo *p = &arg_infos[i];
      p->reg_index = -1;
      p->offset = -1;
      Expr *arg = args->data[i];
      assert(arg->type->kind != TY_ARRAY);
      p->size = type_size(arg->type);
#ifndef __NO_FLONUM
      p->is_flonum = is_flonum(arg->type);
#endif
      p->stack_arg = is_stack_param(arg->type);
      bool reg_arg = !p->stack_arg;
      if (reg_arg) {
#ifndef __NO_FLONUM
        if (p->is_flonum)
          reg_arg = freg_index < MAX_FREG_ARGS;
        else
#endif
          reg_arg = ireg_index < MAX_REG_ARGS;
      }
      if (!reg_arg) {
        if (ireg_index >= MAX_REG_ARGS && vaargs) {
          parse_error(((Expr*)args->data[ireg_index])->token,
                      "Param count exceeds %d", MAX_REG_ARGS);
        }

        offset = ALIGN(offset, align_size(arg->type));
        p->offset = offset;
        offset += ALIGN(p->size, WORD_SIZE);
        ++stack_arg_count;
      } else {
#ifndef __NO_FLONUM
        if (p->is_flonum)
          p->reg_index = freg_index++;
        else
#endif
          p->reg_index = ireg_index++;
      }
    }

    for (int i = 0; i < arg_count; ++i) {
      Expr *arg = args->data[i];
      arg_vtypes[i + arg_start] = to_vtype(arg->type);
    }
  }
  offset = ALIGN(offset, 8);

  IR *precall = new_ir_precall(arg_count - stack_arg_count, offset);

  int reg_arg_count = 0;
  if (offset > 0)
    new_ir_addsp(-offset);
  if (args != NULL) {
    // Register arguments.
    for (int i = arg_count; --i >= 0; ) {
      Expr *arg = args->data[i];
      VReg *reg = gen_expr(arg);
      const ArgInfo *p = &arg_infos[i];
      if (p->offset < 0) {
        new_ir_pusharg(reg, to_vtype(arg->type));
        ++reg_arg_count;
      } else {
        VRegType offset_type = {.size = 4, .align = 4, .flag = 0};  // TODO:
        VReg *dst = new_ir_sofs(new_const_vreg(p->offset + reg_arg_count * WORD_SIZE,
                                               &offset_type));
        if (p->stack_arg) {
          new_ir_memcpy(dst, reg, type_size(arg->type));
        } else {
          if (reg->flag & VRF_CONST) {
            // Allocate new register to avoid constant register.
            VReg *tmp = add_new_reg(arg->type, 0);
            new_ir_mov(tmp, reg);
            reg = tmp;
          }
          new_ir_store(dst, reg);
        }
      }
    }
  }
  if (retvar_reg != NULL) {
    // gen_lval(retvar)
    VReg *dst = new_ir_bofs(retvar_reg);
    VRegType *vtype = to_vtype(ptrof(expr->type));
    new_ir_pusharg(dst, vtype);
    arg_vtypes[0] = vtype;
    ++reg_arg_count;
  }

  bool label_call = false;
  bool global = false;
  if (func->kind == EX_VAR) {
    const VarInfo *varinfo = scope_find(func->var.scope, func->var.name, NULL);
    assert(varinfo != NULL);
    label_call = varinfo->type->kind == TY_FUNC;
    global = !(varinfo->storage & VS_STATIC);
  }

  VReg *result_reg = NULL;
  {
    const Type *type = expr->type;
    if (retvar_reg != NULL)
      type = ptrof(type);
    VRegType *ret_vtype = to_vtype(type);
    if (label_call) {
      result_reg = new_ir_call(func->var.name, global, NULL, reg_arg_count, ret_vtype,
                               precall, arg_vtypes);
    } else {
      VReg *freg = gen_expr(func);
      result_reg = new_ir_call(NULL, false, freg, reg_arg_count, ret_vtype, precall, arg_vtypes);
    }
  }

  free(arg_infos);

  return result_reg;
}

VReg *gen_arith(enum ExprKind kind, const Type *type, VReg *lhs, VReg *rhs) {
  switch (kind) {
  case EX_ADD:
  case EX_SUB:
  case EX_MUL:
#ifndef __NO_FLONUM
    if (is_flonum(type)) {
      return new_ir_bop(kind + (IR_ADD - EX_ADD), lhs, rhs, to_vtype(type));
    }
#endif
    return new_ir_bop(kind + (IR_ADD - EX_ADD), lhs, rhs, to_vtype(type));
  case EX_BITAND:
  case EX_BITOR:
  case EX_BITXOR:
  case EX_LSHIFT:
  case EX_RSHIFT:
    return new_ir_bop(kind + (IR_ADD - EX_ADD), lhs, rhs, to_vtype(type));

  case EX_DIV:
  case EX_MOD:
    assert(is_number(type));
#ifndef __NO_FLONUM
    if (is_flonum(type)) {
      return new_ir_bop(kind + (IR_DIV - EX_DIV), lhs, rhs, to_vtype(type));
    }
#endif
    return new_ir_bop(kind + ((type->fixnum.is_unsigned ? IR_DIVU : IR_DIV) - EX_DIV), lhs, rhs, to_vtype(type));

  default:
    assert(false);
    return NULL;
  }
}

VReg *gen_ptradd(enum ExprKind kind, const Type *type, VReg *lreg, Expr *rhs) {
  size_t scale = type_size(type->pa.ptrof);

  VReg *rreg = gen_expr(rhs);
  if (rreg->flag & VRF_CONST) {
    intptr_t rval = rreg->fixnum;
    if (kind == EX_PTRSUB)
      rval = -rval;
    return new_ir_ptradd(rval * scale, lreg, NULL, 1, to_vtype(type));
  }
  if (kind == EX_PTRSUB) {
    rreg = new_ir_unary(IR_NEG, rreg, to_vtype(rhs->type));
#if 1
  } else {  // To avoid both spilled registers, add temporary register.
    VReg *tmp = add_new_reg(rhs->type, 0);
    new_ir_mov(tmp, rreg);
    rreg = tmp;
#endif
  }
  if (scale > 8 || !IS_POWER_OF_2(scale)) {
    VRegType *vtype = to_vtype(rhs->type);
    VReg *sreg = new_const_vreg(scale, vtype);
    rreg = new_ir_bop(IR_MUL, rreg, sreg, vtype);
    scale = 1;
  }
  rreg = new_ir_cast(rreg, to_vtype(&tySize));
  return new_ir_ptradd(0, lreg, rreg, scale, to_vtype(type));
}

#ifndef __NO_FLONUM
VReg *gen_const_flonum(Expr *expr) {
  assert(expr->type->kind == TY_FLONUM);
  Initializer *init = malloc(sizeof(*init));
  init->kind = IK_SINGLE;
  init->single = expr;
  init->token = expr->token;

  assert(curscope != NULL);
  const Type *type = qualified_type(expr->type, TQ_CONST);
  const Token *ident = alloc_ident(alloc_label(), NULL, NULL);
  VarInfo *varinfo = scope_add(curscope, ident, type, VS_STATIC);
  VarInfo *gvarinfo = is_global_scope(curscope) ? varinfo : varinfo->static_.gvar;
  gvarinfo->global.init = init;

  VReg *src = new_ir_iofs(gvarinfo->name, false);
  return new_ir_unary(IR_LOAD, src, to_vtype(type));
}
#endif

VReg *gen_expr(Expr *expr) {
  switch (expr->kind) {
  case EX_FIXNUM:
    {
      assert(expr->type->kind == TY_FIXNUM);
      VReg *reg = new_const_vreg(expr->fixnum, to_vtype(expr->type));
      if (!is_im32(expr->fixnum)) {
        // Large constant value is not allowed in x86,
        // so use mov instruction.
        VReg *tmp = add_new_reg(expr->type, 0);
        new_ir_mov(tmp, reg);
        reg = tmp;
      }
      return reg;
    }
#ifndef __NO_FLONUM
  case EX_FLONUM:
    return gen_const_flonum(expr);
#endif

  case EX_STR:
    assert(!"should be handled in parser");

  case EX_VAR:
    return gen_variable(expr);

  case EX_REF:
    return gen_lval(expr->unary.sub);

  case EX_DEREF:
    {
      VReg *reg = gen_expr(expr->unary.sub);
      VReg *result;
      switch (expr->type->kind) {
      case TY_FIXNUM:
      case TY_PTR:
#ifndef __NO_FLONUM
      case TY_FLONUM:
#endif
        result = new_ir_unary(IR_LOAD, reg, to_vtype(expr->type));
        return result;

      default:
        assert(false);
        // Fallthrough to suppress compile error.
      case TY_ARRAY:
      case TY_STRUCT:
      case TY_FUNC:
        // array, struct and func values are handled as a pointer.
        return reg;
      }
    }

  case EX_MEMBER:
    {
      VReg *reg = gen_lval(expr);
      VReg *result;
      switch (expr->type->kind) {
      case TY_FIXNUM:
      case TY_PTR:
#ifndef __NO_FLONUM
      case TY_FLONUM:
#endif
        result = new_ir_unary(IR_LOAD, reg, to_vtype(expr->type));
        break;
      default:
        assert(false);
        // Fallthrough to suppress compile error.
      case TY_ARRAY:
      case TY_STRUCT:
        result = reg;
        break;
      }
      return result;
    }

  case EX_COMMA:
    gen_expr(expr->bop.lhs);
    return gen_expr(expr->bop.rhs);

  case EX_TERNARY:
    return gen_ternary(expr);

  case EX_CAST:
    return gen_cast(gen_expr(expr->unary.sub), expr->type);

  case EX_ASSIGN:
    {
      VReg *src = gen_expr(expr->bop.rhs);
      if (expr->bop.lhs->kind == EX_VAR) {
        Expr *lhs = expr->bop.lhs;
        switch (lhs->type->kind) {
        case TY_FIXNUM:
        case TY_PTR:
#ifndef __NO_FLONUM
        case TY_FLONUM:
#endif
          {
            Scope *scope;
            const VarInfo *varinfo = scope_find(lhs->var.scope, lhs->var.name, &scope);
            assert(varinfo != NULL);
            if (!is_global_scope(scope) && !(varinfo->storage & (VS_STATIC | VS_EXTERN))) {
              assert(varinfo->local.reg != NULL);
              new_ir_mov(varinfo->local.reg, src);
              return src;
            }
          }
          break;
        default:
          break;
        }
      }

      VReg *dst = gen_lval(expr->bop.lhs);

      switch (expr->type->kind) {
      default:
        assert(false);
        // Fallthrough to suppress compiler error.
      case TY_FIXNUM:
      case TY_PTR:
#ifndef __NO_FLONUM
      case TY_FLONUM:
#endif
#if 0
        new_ir_store(dst, src);
#else
        // To avoid both spilled registers, add temporary register.
        {
          VReg *tmp = add_new_reg(expr->type, 0);
          new_ir_mov(tmp, src);
          new_ir_store(dst, tmp);
        }
#endif
        break;
      case TY_STRUCT:
        if (expr->type->struct_.info->size > 0) {
          VReg *tmp = add_new_reg(&tyVoidPtr, 0);
          new_ir_mov(tmp, src);
          new_ir_memcpy(dst, tmp, expr->type->struct_.info->size);
        }
        break;
      }
      return src;
    }

  case EX_MODIFY:
    {
      Expr *sub = expr->unary.sub;
      switch (sub->kind) {
      case EX_PTRADD:
      case EX_PTRSUB:
        if (sub->bop.lhs->kind == EX_VAR && !is_global_scope(sub->bop.lhs->var.scope)) {
          VReg *lhs = gen_expr(sub->bop.lhs);
          VReg *result = gen_ptradd(sub->kind, sub->type, lhs, sub->bop.rhs);
          new_ir_mov(lhs, result);
          return result;
        } else {
          VReg *lval = gen_lval(sub->bop.lhs);
          VReg *lhs = new_ir_unary(IR_LOAD, lval, to_vtype(sub->bop.lhs->type));
          VReg *result = gen_ptradd(sub->kind, sub->type, lhs, sub->bop.rhs);
          VReg *cast = gen_cast(result, expr->type);
          new_ir_store(lval, cast);
          return result;
        }
      default:
        if (sub->bop.lhs->kind == EX_VAR && !is_global_scope(sub->bop.lhs->var.scope)) {
          VReg *lhs = gen_expr(sub->bop.lhs);
          VReg *rhs = gen_expr(sub->bop.rhs);
          VReg *result = gen_arith(sub->kind, sub->type, lhs, rhs);
          new_ir_mov(lhs, result);
          return result;
        } else {
          VReg *lval = gen_lval(sub->bop.lhs);
          VReg *rhs = gen_expr(sub->bop.rhs);
          VReg *lhs = new_ir_unary(IR_LOAD, lval, to_vtype(sub->bop.lhs->type));
          VReg *result = gen_arith(sub->kind, sub->type, lhs, rhs);
          VReg *cast = gen_cast(result, expr->type);
          new_ir_store(lval, cast);
          return result;
        }
      }
    }

  case EX_PREINC:
  case EX_PREDEC:
    {
      size_t value = 1;
      if (expr->type->kind == TY_PTR)
        value = type_size(expr->type->pa.ptrof);

      VRegType *vtype = to_vtype(expr->type);
      Expr *sub = expr->unary.sub;
      if (sub->kind == EX_VAR && !is_global_scope(sub->var.scope)) {
        const VarInfo *varinfo = scope_find(sub->var.scope, sub->var.name, NULL);
        assert(varinfo != NULL);
        if (!(varinfo->storage & (VS_STATIC | VS_EXTERN))) {
#ifndef __NO_FLONUM
          if (is_flonum(sub->type)) {
            VReg *one = gen_const_flonum(new_expr_flolit(sub->type, NULL, 1));
            VReg *result = new_ir_bop(expr->kind == EX_PREINC ? IR_ADD : IR_SUB,
                                      varinfo->local.reg, one, vtype);
            new_ir_mov(varinfo->local.reg, result);
            return result;
          }
#endif
          VReg *num = new_const_vreg(value, vtype);
          VReg *result = new_ir_bop(expr->kind == EX_PREINC ? IR_ADD : IR_SUB,
                                    varinfo->local.reg, num, vtype);
          new_ir_mov(varinfo->local.reg, result);
          return result;
        }
      }

      VReg *lval = gen_lval(sub);
#ifndef __NO_FLONUM
      if (is_flonum(sub->type)) {
        VReg *val = new_ir_unary(IR_LOAD, lval, vtype);
        VReg *one = gen_const_flonum(new_expr_flolit(sub->type, NULL, value));
        VReg *result = new_ir_bop(expr->kind == EX_PREINC ? IR_ADD : IR_SUB,
                                  val, one, vtype);
        new_ir_store(lval, result);
        return result;
      }
#endif
      new_ir_incdec(expr->kind == EX_PREINC ? IR_INC : IR_DEC,
                    lval, type_size(expr->type), value);
      VReg *result = new_ir_unary(IR_LOAD, lval, vtype);
      return result;
    }

  case EX_POSTINC:
  case EX_POSTDEC:
    {
      size_t value = 1;
      if (expr->type->kind == TY_PTR)
        value = type_size(expr->type->pa.ptrof);

      VRegType *vtype = to_vtype(expr->type);
      Expr *sub = expr->unary.sub;
      if (sub->kind == EX_VAR && !is_global_scope(sub->var.scope)) {
        const VarInfo *varinfo = scope_find(sub->var.scope, sub->var.name, NULL);
        assert(varinfo != NULL);
        if (!(varinfo->storage & (VS_STATIC | VS_EXTERN))) {
#ifndef __NO_FLONUM
          if (is_flonum(sub->type)) {
            VReg *org_val = add_new_reg(sub->type, 0);
            new_ir_mov(org_val, varinfo->local.reg);
            VReg *one = gen_const_flonum(new_expr_flolit(sub->type, NULL, 1));
            VReg *result = new_ir_bop(expr->kind == EX_POSTINC ? IR_ADD : IR_SUB,
                                      varinfo->local.reg, one, vtype);
            new_ir_mov(varinfo->local.reg, result);
            return org_val;
          }
#endif
          VReg *org_val = add_new_reg(sub->type, 0);
          new_ir_mov(org_val, varinfo->local.reg);
          VReg *num = new_const_vreg(value, vtype);
          VReg *result = new_ir_bop(expr->kind == EX_POSTINC ? IR_ADD : IR_SUB,
                                    varinfo->local.reg, num, vtype);
          new_ir_mov(varinfo->local.reg, result);
          return org_val;
        }
      }

      VReg *lval = gen_lval(expr->unary.sub);
#ifndef __NO_FLONUM
      if (is_flonum(sub->type)) {
        VReg *val = new_ir_unary(IR_LOAD, lval, vtype);
        VReg *one = gen_const_flonum(new_expr_flolit(sub->type, NULL, value));
        VReg *result = new_ir_bop(expr->kind == EX_POSTINC ? IR_ADD : IR_SUB,
                                  val, one, vtype);
        new_ir_store(lval, result);
        return val;
      }
#endif
      VReg *result = new_ir_unary(IR_LOAD, lval, vtype);
      new_ir_incdec(expr->kind == EX_POSTINC ? IR_INC : IR_DEC,
                    lval, type_size(expr->type), value);
      return result;
    }

  case EX_FUNCALL:
    return gen_funcall(expr);

  case EX_POS:
    return gen_expr(expr->unary.sub);

  case EX_NEG:
    {
      VReg *reg = gen_expr(expr->unary.sub);
#ifndef __NO_FLONUM
      if (is_flonum(expr->type)) {
        VReg *zero = gen_expr(new_expr_flolit(expr->type, NULL, 0.0));
        return gen_arith(EX_SUB, expr->type, zero, reg);
      }
#endif
      VReg *result = new_ir_unary(IR_NEG, reg, to_vtype(expr->type));
      return result;
    }

  case EX_BITNOT:
    {
      VReg *reg = gen_expr(expr->unary.sub);
      VReg *result = new_ir_unary(IR_BITNOT, reg, to_vtype(expr->type));
      return result;
    }

  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_GT:
  case EX_LE:
  case EX_GE:
    {
      enum ConditionKind cond = gen_compare_expr(expr->kind, expr->bop.lhs, expr->bop.rhs);
      switch (cond) {
      case COND_NONE:
      case COND_ANY:
        return new_const_vreg(cond == COND_ANY, to_vtype(&tyBool));
      default:
        return new_ir_cond(cond);
      }
    }

  case EX_LOGAND:
    {
      BB *bb1 = new_bb();
      BB *bb2 = new_bb();
      BB *false_bb = new_bb();
      BB *next_bb = new_bb();
      gen_cond_jmp(expr->bop.lhs, false, false_bb);
      set_curbb(bb1);
      gen_cond_jmp(expr->bop.rhs, false, false_bb);
      set_curbb(bb2);
      VRegType *vtbool = to_vtype(&tyBool);
      VReg *result = add_new_reg(&tyBool, 0);
      new_ir_mov(result, new_const_vreg(true, vtbool));
      new_ir_jmp(COND_ANY, next_bb);
      set_curbb(false_bb);
      new_ir_mov(result, new_const_vreg(false, vtbool));
      set_curbb(next_bb);
      return result;
    }

  case EX_LOGIOR:
    {
      BB *bb1 = new_bb();
      BB *bb2 = new_bb();
      BB *true_bb = new_bb();
      BB *next_bb = new_bb();
      gen_cond_jmp(expr->bop.lhs, true, true_bb);
      set_curbb(bb1);
      gen_cond_jmp(expr->bop.rhs, true, true_bb);
      set_curbb(bb2);
      VRegType *vtbool = to_vtype(&tyBool);
      VReg *result = add_new_reg(&tyBool, 0);
      new_ir_mov(result, new_const_vreg(false, vtbool));
      new_ir_jmp(COND_ANY, next_bb);
      set_curbb(true_bb);
      new_ir_mov(result, new_const_vreg(true, vtbool));
      set_curbb(next_bb);
      return result;
    }

  case EX_ADD:
  case EX_SUB:
  case EX_MUL:
  case EX_DIV:
  case EX_MOD:
  case EX_LSHIFT:
  case EX_RSHIFT:
  case EX_BITAND:
  case EX_BITOR:
  case EX_BITXOR:
    {
      VReg *lhs = gen_expr(expr->bop.lhs);
      VReg *rhs = gen_expr(expr->bop.rhs);
      return gen_arith(expr->kind, expr->type, lhs, rhs);
    }

  case EX_PTRADD:
  case EX_PTRSUB:
    {
      assert(expr->type->kind == TY_PTR);
      VReg *lreg = gen_expr(expr->bop.lhs);
      return gen_ptradd(expr->kind, expr->type, lreg, expr->bop.rhs);
    }

  case EX_COMPLIT:
    gen_stmts(expr->complit.inits);
    return gen_expr(expr->complit.var);

  default:
    fprintf(stderr, "Expr kind=%d, ", expr->kind);
    assert(!"Unhandled in gen_expr");
    break;
  }

  return NULL;
}
#include "emit.h"

#include <assert.h>
#include <inttypes.h>  // PRIdPTR
#include <stdarg.h>
#include <stdint.h>  // intptr_t

#include "table.h"
#include "util.h"

#ifdef __APPLE__
#define MANGLE_PREFIX  "_"
#endif

static FILE *emit_fp;

char *fmt(const char *s, ...) {
  static char buf[4][64];
  static int index;
  char *p = buf[index];
  if (++index >= 4)
    index = 0;
  va_list ap;
  va_start(ap, s);
  vsnprintf(p, sizeof(buf[0]), s, ap);
  va_end(ap);
  return p;
}

char *fmt_name(const Name *name) {
  return fmt("%.*s", name->bytes, name->chars);
}

char *num(intptr_t x) {
  return fmt("%" PRIdPTR, x);
}

char *hexnum(intptr_t x) {
  return fmt("0x%" PRIxPTR, x);
}

#ifndef __NO_FLONUM
char *flonum(double x) {
  return fmt("%.16g", x);
}
#endif

char *im(intptr_t x) {
  return fmt("$%" PRIdPTR, x);
}

char *indirect(const char *base, const char *index, int scale) {
  if (index == NULL) {
    return fmt("(%s)", base);
  } else {
    if (scale == 1)
      return fmt("(%s,%s)", base, index);
    else
      return fmt("(%s,%s,%d)", base, index, scale);
  }
}

char *offset_indirect(int offset, const char *base, const char *index, int scale) {
  if (offset == 0)
    return indirect(base, index, scale);

  if (index == NULL) {
    return fmt("%d(%s)", offset, base);
  } else {
    if (scale == 1)
      return fmt("%d(%s,%s)", offset, base, index);
    else
      return fmt("%d(%s,%s,%d)", offset, base, index, scale);
  }
}

char *label_indirect(const char *label, const char *reg) {
  return fmt("%s(%s)", label, reg);
}

const char *mangle(const char *label) {
#ifdef MANGLE_PREFIX
  return fmt(MANGLE_PREFIX "%s", label);
#else
  return label;
#endif
}

void emit_asm2(const char *op, const char *operand1, const char *operand2) {
  if (operand1 == NULL) {
    fprintf(emit_fp, "\t%s\n", op);
  } else if (operand2 == NULL) {
    fprintf(emit_fp, "\t%s %s\n", op, operand1);
  } else {
    fprintf(emit_fp, "\t%s %s, %s\n", op, operand1, operand2);
  }
}

void emit_label(const char *label) {
  fprintf(emit_fp, "%s:\n", label);
}

void emit_comment(const char *comment, ...) {
  if (comment == NULL) {
    fprintf(emit_fp, "\n");
    return;
  }

  va_list ap;
  va_start(ap, comment);
  fprintf(emit_fp, "// ");
  vfprintf(emit_fp, comment, ap);
  fprintf(emit_fp, "\n");
  va_end(ap);
}

void emit_align(int align) {
  if (align <= 1)
    return;
  fprintf(emit_fp, "\t.align %d\n", align);
}

void emit_align_p2(int align) {
  if (align <= 1)
    return;

  // On Apple platform,
  // .align directive is actually .p2align,
  // so it has to find power of 2.
  assert(IS_POWER_OF_2(align));
  int bit, x = align;
  for (bit = 0;; ++bit) {
    x >>= 1;
    if (x <= 0)
      break;
  }
  fprintf(emit_fp, "\t.p2align %d\n", bit);
}

void init_emit(FILE *fp) {
  emit_fp = fp;
}
#include "emit_code.h"

#include <assert.h>
#include <inttypes.h>  // PRIdPTR
#include <limits.h>  // CHAR_BIT
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "codegen.h"
#include "ir.h"
#include "lexer.h"
#include "regalloc.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"
#include "x86_64.h"

static void construct_initial_value(const Type *type, const Initializer *init) {
  assert(init == NULL || init->kind != IK_DOT);

  switch (type->kind) {
  case TY_FIXNUM:
    {
      intptr_t v = 0;
      if (init != NULL) {
        assert(init->kind == IK_SINGLE);
        Expr *value = init->single;
        if (!(is_const(value) && is_fixnum(value->type->kind)))
          error("Illegal initializer: constant number expected");
        v = value->fixnum;
      }

      switch (type->fixnum.kind) {
      case FX_CHAR:  _BYTE(NUM(v)); break;
      case FX_SHORT: _WORD(NUM(v)); break;
      case FX_LONG:  _QUAD(NUM(v)); break;
      case FX_LLONG: _QUAD(NUM(v)); break;
      default:
        assert(false);
        // Fallthrough
      case FX_INT: case FX_ENUM:
        _LONG(NUM(v));
        break;
      }
    }
    break;
#ifndef __NO_FLONUM
  case TY_FLONUM:
    switch (type->flonum.kind) {
    case FL_DOUBLE:
      {
        union {double f; uint64_t h;} v;
        v.f = 0;
        if (init != NULL) {
          assert(init->kind == IK_SINGLE);
          Expr *value = init->single;
          if (!(is_const(value) && is_flonum(value->type)))
            error("Illegal initializer: constant number expected");
          v.f = value->flonum;
        }
#if 0
        _DOUBLE(FLONUM(v.d));
#else
        _QUAD(HEXNUM(v.h));
#endif
      }
      break;
    case FL_FLOAT:
      {
        union {float f; uint32_t h;} v;
        v.f = 0;
        if (init != NULL) {
          assert(init->kind == IK_SINGLE);
          Expr *value = init->single;
          if (!(is_const(value) && is_flonum(value->type)))
            error("Illegal initializer: constant number expected");
          v.f = value->flonum;
        }
#if 0
        _FLOAT(FLONUM(v.f));
#else
        _LONG(HEXNUM(v.h));
#endif
      }
      break;
    }
    break;
#endif
  case TY_PTR:
    if (init != NULL) {
      assert(init->kind == IK_SINGLE);
      Expr *value = init->single;
      while (value->kind == EX_CAST)
        value = value->unary.sub;
      if (value->kind == EX_REF || value->kind == EX_VAR) {
        if (value->kind == EX_REF)
          value = value->unary.sub;

        assert(value->kind == EX_VAR);

        const Name *name = value->var.name;
        Scope *scope;
        VarInfo *varinfo = scope_find(value->var.scope, name, &scope);
        assert(varinfo != NULL);
        if (!is_global_scope(scope) && varinfo->storage & VS_STATIC) {
          varinfo = varinfo->static_.gvar;
          assert(varinfo != NULL);
          name = varinfo->name;
        }

        const char *label = fmt_name(name);
        if ((varinfo->storage & VS_STATIC) == 0)
          label = MANGLE(label);
        _QUAD(label);
      } else if (value->kind == EX_STR) {
        assert(!"should be handled in parser");
      } else if (is_const(value) && value->kind == EX_FIXNUM) {
        intptr_t x = value->fixnum;
        _QUAD(NUM(x));
      } else {
        assert(!"initializer type error");
      }
    } else {
      _QUAD(NUM(0));
    }
    break;
  case TY_ARRAY:
    if (init == NULL || init->kind == IK_MULTI) {
      const Type *elem_type = type->pa.ptrof;
      if (init != NULL) {
        Vector *init_array = init->multi;
        size_t index = 0;
        size_t len = init_array->len;
        for (size_t i = 0; i < len; ++i, ++index) {
          const Initializer *init_elem = init_array->data[i];
          if (init_elem->kind == IK_ARR) {
            size_t next = init_elem->arr.index->fixnum;
            for (size_t j = index; j < next; ++j)
              construct_initial_value(elem_type, NULL);
            index = next;
            init_elem = init_elem->arr.value;
          }
          construct_initial_value(elem_type, init_elem);
        }
        // Padding
        for (size_t i = index, n = type->pa.length; i < n; ++i)
          construct_initial_value(elem_type, NULL);
      }
    } else {
      if (init->kind == IK_SINGLE && is_char_type(type->pa.ptrof) && init->single->kind == EX_STR) {
        size_t src_size = init->single->str.size;
        size_t size = type_size(type);
        assert(size >= src_size);

        UNUSED(size);
        StringBuffer sb;
        sb_init(&sb);
        sb_append(&sb, "\"", NULL);
        escape_string(init->single->str.buf, src_size, &sb);
        if (size > src_size) {
          const char NULCHR[] = "\\0";
          for (size_t i = 0, n = size - src_size; i < n; ++i)
            sb_append(&sb, NULCHR, NULL);
        }
        sb_append(&sb, "\"", NULL);
        _ASCII(sb_to_string(&sb));
      } else {
        error("Illegal initializer");
      }
    }
    break;
  case TY_STRUCT:
    {
      assert(init == NULL || init->kind == IK_MULTI);

      const StructInfo *sinfo = type->struct_.info;
      int count = 0;
      int offset = 0;
      for (int i = 0, n = sinfo->members->len; i < n; ++i) {
        const VarInfo* member = sinfo->members->data[i];
        const Initializer *mem_init;
        if (init == NULL) {
          if (sinfo->is_union)
            continue;
          mem_init = NULL;
        } else {
          mem_init = init->multi->data[i];
        }
        if (mem_init != NULL || !sinfo->is_union) {
          int align = align_size(member->type);
          if (offset % align != 0) {
            EMIT_ALIGN(align);
            offset = ALIGN(offset, align);
          }
          construct_initial_value(member->type, mem_init);
          ++count;
          offset = ALIGN(offset, align);
          offset += type_size(member->type);
        }
      }
      if (sinfo->is_union && count <= 0) {
        const VarInfo* member = sinfo->members->data[0];
        construct_initial_value(member->type, NULL);
        offset += type_size(member->type);
      }

      size_t size = type_size(type);
      if (size != (size_t)offset) {
        // Put padding.
        int d = size - offset;
        switch (d) {
        case 1:  _BYTE(NUM(0)); break;
        case 2:  _WORD(NUM(0)); break;
        case 4:  _LONG(NUM(0)); break;
        case 8:  _QUAD(NUM(0)); break;
        default:
          for (int i = 0; i < d; ++i)
            _BYTE(NUM(0));
          break;
        }
      }
    }
    break;
  default:
    fprintf(stderr, "Global initial value for type %d not implemented (yet)\n", type->kind);
    assert(false);
    break;
  }
}

static void emit_varinfo(const VarInfo *varinfo, const Initializer *init) {
  const Name *name = varinfo->name;
  if (init != NULL) {
    if (varinfo->type->qualifier & TQ_CONST)
      _RODATA();
    else
      _DATA();
  }

  const char *label = fmt_name(name);
  if ((varinfo->storage & VS_STATIC) == 0) {  // global
    label = MANGLE(label);
    _GLOBL(label);
  }

  if (init != NULL) {
    EMIT_ALIGN(align_size(varinfo->type));
    EMIT_LABEL(label);
    //size_t size = type_size(varinfo->type);
    construct_initial_value(varinfo->type, init);
  } else {
    size_t size = type_size(varinfo->type);
    if (size < 1)
      size = 1;

    size_t align = align_size(varinfo->type);
    if (align <= 1)
      _COMM(label, NUM(size));
    else
      _COMM(label, fmt("%" PRIdPTR ",%" PRIdPTR, size, align));
  }
}

////////////////////////////////////////////////

static bool is_asm(Stmt *stmt) {
  return stmt->kind == ST_ASM;
}

static VarInfo *find_ret_var(Scope *scope) {
  const Name *retval_name = alloc_name(RET_VAR_NAME, NULL, false);
  return scope_find(scope, retval_name, NULL);
}

static void put_args_to_stack(Function *func) {
  static const char *kReg8s[] = {DIL, SIL, DL, CL, R8B, R9B};
  static const char *kReg16s[] = {DI, SI, DX, CX, R8W, R9W};
  static const char *kReg32s[] = {EDI, ESI, EDX, ECX, R8D, R9D};
  static const char *kReg64s[] = {RDI, RSI, RDX, RCX, R8, R9};
  static const char **kRegTable[] = {NULL, kReg8s, kReg16s, NULL, kReg32s, NULL, NULL, NULL, kReg64s};
#ifndef __NO_FLONUM
  static const char *kFReg64s[] = {XMM0, XMM1, XMM2, XMM3, XMM4, XMM5};
#endif

  int arg_index = 0;
  if (is_stack_param(func->type->func.ret)) {
    Scope *top_scope = func->scopes->data[0];
    VarInfo *varinfo = find_ret_var(top_scope);
    assert(varinfo != NULL);
    const Type *type = varinfo->type;
    int size = type_size(type);
    int offset = varinfo->local.reg->offset;
    assert(size < (int)(sizeof(kRegTable) / sizeof(*kRegTable)) &&
           kRegTable[size] != NULL);
    MOV(kRegTable[size][0], OFFSET_INDIRECT(offset, RBP, NULL, 1));
    ++arg_index;
  }

  // Store arguments into local frame.
  const Vector *params = func->type->func.params;
  if (params == NULL)
    return;

  int len = params->len;
  int n = len;
  if (func->type->func.vaargs && n < MAX_REG_ARGS)
    n = MAX_REG_ARGS;
#ifndef __NO_FLONUM
  int farg_index = 0;
#endif
  for (int i = 0; i < n; ++i) {
    const Type *type;
    int offset;
    if (i < len) {
      const VarInfo *varinfo = params->data[i];
      type = varinfo->type;
      offset = varinfo->local.reg->offset;
    } else {  // vaargs
      type = get_fixnum_type(FX_LONG, false, 0);
      offset = (i - MAX_REG_ARGS) * WORD_SIZE;
    }

    if (is_stack_param(type))
      continue;

#ifndef __NO_FLONUM
    if (is_flonum(type)) {
      if (farg_index < MAX_FREG_ARGS) {
        switch (type->flonum.kind) {
        case FL_FLOAT:   MOVSS(kFReg64s[farg_index], OFFSET_INDIRECT(offset, RBP, NULL, 1)); break;
        case FL_DOUBLE:  MOVSD(kFReg64s[farg_index], OFFSET_INDIRECT(offset, RBP, NULL, 1)); break;
        default: assert(false); break;
        }
        ++farg_index;
      }
      continue;
    }
#endif

    switch (type->kind) {
    case TY_FIXNUM:
    case TY_PTR:
      break;
    default: assert(false); break;
    }

    if (arg_index < MAX_REG_ARGS) {
      int size = type_size(type);
      assert(size < (int)(sizeof(kRegTable) / sizeof(*kRegTable)) &&
            kRegTable[size] != NULL);
      MOV(kRegTable[size][arg_index], OFFSET_INDIRECT(offset, RBP, NULL, 1));
      ++arg_index;
    }
  }
}

static void emit_defun(Function *func) {
  if (func->scopes == NULL)  // Prototype definition
    return;

  assert(stackpos == 8);

  emit_comment(NULL);
  _TEXT();

  bool global = true;
  const VarInfo *varinfo = scope_find(global_scope, func->name, NULL);
  if (varinfo != NULL) {
    global = (varinfo->storage & VS_STATIC) == 0;
  }

  const char *label = fmt_name(func->name);
  if (global) {
    const char *gl = MANGLE(label);
    _GLOBL(gl);
    EMIT_LABEL(gl);
  } else {
    emit_comment("%.*s: static func", func->name->bytes, func->name->chars);
    EMIT_LABEL(label);
  }

  bool no_stmt = true;
  if (func->stmts != NULL) {
    for (int i = 0; i < func->stmts->len; ++i) {
      Stmt *stmt = func->stmts->data[i];
      if (stmt == NULL)
        continue;
      if (!is_asm(stmt)) {
        no_stmt = false;
        break;
      }
    }
  }

  // Prologue
  // Allocate variable bufer.
  if (!no_stmt) {
    PUSH(RBP); PUSH_STACK_POS();
    MOV(RSP, RBP);
    if (func->ra->frame_size > 0) {
      SUB(IM(func->ra->frame_size), RSP);
      stackpos += func->ra->frame_size;
    }

    put_args_to_stack(func);

    // Callee save.
    push_callee_save_regs(func->ra->used_reg_bits);
  }

  emit_bb_irs(func->bbcon);

  // Epilogue
  if (!no_stmt) {
    pop_callee_save_regs(func->ra->used_reg_bits);

    MOV(RBP, RSP);
    stackpos -= func->ra->frame_size;
    POP(RBP); POP_STACK_POS();
  }

  RET();

  // Output static local variables.
  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = func->scopes->data[i];
    if (scope->vars == NULL)
      continue;
    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      if (!(varinfo->storage & VS_STATIC))
        continue;
      VarInfo *gvarinfo = varinfo->static_.gvar;
      assert(gvarinfo != NULL);
      emit_varinfo(gvarinfo, gvarinfo->global.init);
    }
  }

  assert(stackpos == 8);
}

void emit_code(Vector *toplevel) {
  for (int i = 0, len = toplevel->len; i < len; ++i) {
    Declaration *decl = toplevel->data[i];
    if (decl == NULL)
      continue;

    switch (decl->kind) {
    case DCL_DEFUN:
      emit_defun(decl->defun.func);
      break;
    case DCL_VARDECL:
      {
        emit_comment(NULL);
        Vector *decls = decl->vardecl.decls;
        for (int i = 0; i < decls->len; ++i) {
          VarDecl *vd = decls->data[i];
          if ((vd->storage & VS_EXTERN) != 0)
            continue;
          const Name *name = vd->ident->ident;
          const VarInfo *varinfo = scope_find(global_scope, name, NULL);
          assert(varinfo != NULL);

          emit_varinfo(varinfo, varinfo->global.init);
        }
      }
      break;

    default:
      error("Unhandled decl in emit_code: %d", decl->kind);
      break;
    }
  }
}
#include "ir.h"

#include <assert.h>
#include <stdlib.h>  // malloc

#include "regalloc.h"
#include "table.h"
#include "util.h"
#include "x86_64.h"

#define WORK_REG_NO  (PHYSICAL_REG_MAX)

static void push_caller_save_regs(unsigned short living, int base);
static void pop_caller_save_regs(unsigned short living);

static VRegType vtVoidPtr = {.size = WORD_SIZE, .align = WORD_SIZE, .flag = 0};
static VRegType vtBool    = {.size = 4, .align = 4, .flag = 0};

int stackpos = 8;

static enum ConditionKind invert_cond(enum ConditionKind cond) {
  assert(COND_EQ <= cond && cond <= COND_UGT);
  if (cond <= COND_NE)
    return COND_NE + COND_EQ - cond;
  if (cond <= COND_ULT)
    return COND_LT + ((cond - COND_LT) ^ 2);
  return COND_ULT + ((cond - COND_ULT) ^ 2);
}

// Virtual register

VReg *new_vreg(int vreg_no, const VRegType *vtype, int flag) {
  VReg *vreg = malloc(sizeof(*vreg));
  vreg->virt = vreg_no;
  vreg->phys = -1;
  vreg->fixnum = 0;
  vreg->vtype = vtype;
  vreg->flag = flag;
  vreg->param_index = -1;
  vreg->offset = 0;
  return vreg;
}

// Register allocator

const char *kRegSizeTable[][7] = {
  { BL, R10B, R11B, R12B, R13B, R14B, R15B},
  { BX, R10W, R11W, R12W, R13W, R14W, R15W},
  {EBX, R10D, R11D, R12D, R13D, R14D, R15D},
  {RBX, R10,  R11,  R12,  R13,  R14,  R15},
};

#define kReg8s   (kRegSizeTable[0])
#define kReg32s  (kRegSizeTable[2])
#define kReg64s  (kRegSizeTable[3])

const char *kRegATable[] = {AL, AX, EAX, RAX};
const char *kRegDTable[] = {DL, DX, EDX, RDX};

#ifndef __NO_FLONUM
#define SZ_FLOAT   (4)
#define SZ_DOUBLE  (8)
const char *kFReg64s[7] = {XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14};
#endif

#define CALLEE_SAVE_REG_COUNT  ((int)(sizeof(kCalleeSaveRegs) / sizeof(*kCalleeSaveRegs)))
const int kCalleeSaveRegs[] = {
  0,  // RBX
  3,  // R12
  4,  // R13
  5,  // R14
};

#define CALLER_SAVE_REG_COUNT  ((int)(sizeof(kCallerSaveRegs) / sizeof(*kCallerSaveRegs)))
const int kCallerSaveRegs[] = {
  1,  // R10
  2,  // R11
};

#ifndef __NO_FLONUM
#define CALLER_SAVE_FREG_COUNT  ((int)(sizeof(kCallerSaveFRegs) / sizeof(*kCallerSaveFRegs)))
const int kCallerSaveFRegs[] = {0, 1, 2, 3, 4, 5};
#endif

//
RegAlloc *curra;

// Intermediate Representation

static IR *new_ir(enum IrKind kind) {
  IR *ir = malloc(sizeof(*ir));
  ir->kind = kind;
  ir->dst = ir->opr1 = ir->opr2 = NULL;
  ir->size = -1;
  if (curbb != NULL)
    vec_push(curbb->irs, ir);
  return ir;
}

static const int kPow2Table[] = {-1, 0, 1, -1, 2, -1, -1, -1, 3};
#define kPow2TableSize  ((int)(sizeof(kPow2Table) / sizeof(*kPow2Table)))

VReg *new_const_vreg(intptr_t value, const VRegType *vtype) {
  VReg *vreg = reg_alloc_spawn(curra, vtype, VRF_CONST);
  vreg->fixnum = value;
  return vreg;
}

static intptr_t clamp_value(intptr_t value, const VRegType *vtype) {
  if (vtype->flag & VRTF_UNSIGNED) {
    switch (vtype->size) {
    case 1:  value = (unsigned char)value; break;
    case 2:  value = (unsigned short)value; break;
    case 4:  value = (unsigned int)value; break;
    default:  break;
    }
  } else {
    switch (vtype->size) {
    case 1:  value = (char)value; break;
    case 2:  value = (short)value; break;
    case 4:  value = (int)value; break;
    default:  break;
    }
  }
  return value;
}

VReg *new_ir_bop(enum IrKind kind, VReg *opr1, VReg *opr2, const VRegType *vtype) {
  if (opr1->flag & VRF_CONST) {
    if (opr2->flag & VRF_CONST) {
      intptr_t value = 0;
      switch (kind) {
      case IR_ADD:     value = opr1->fixnum + opr2->fixnum; break;
      case IR_SUB:     value = opr1->fixnum - opr2->fixnum; break;
      case IR_MUL:     value = opr1->fixnum * opr2->fixnum; break;

      case IR_DIV:
      case IR_DIVU:
      case IR_MOD:
      case IR_MODU:
        if (opr2->fixnum == 0)
          error("Divide by 0");
        switch (kind) {
        case IR_DIV:  value = opr1->fixnum / opr2->fixnum; break;
        case IR_DIVU: value = (uintptr_t)opr1->fixnum / opr2->fixnum; break;
        case IR_MOD:  value = opr1->fixnum / opr2->fixnum; break;
        case IR_MODU: value = (uintptr_t)opr1->fixnum / opr2->fixnum; break;
        default: assert(false); break;
        }
        break;

      case IR_BITAND:  value = opr1->fixnum & opr2->fixnum; break;
      case IR_BITOR:   value = opr1->fixnum | opr2->fixnum; break;
      case IR_BITXOR:  value = opr1->fixnum ^ opr2->fixnum; break;
      case IR_LSHIFT:  value = opr1->fixnum << opr2->fixnum; break;
      case IR_RSHIFT:
        //assert(opr1->type->kind == TY_FIXNUM);
        if (opr1->vtype->flag & VRTF_UNSIGNED)
          value = (uintptr_t)opr1->fixnum >> opr2->fixnum;
        else
          value = opr1->fixnum >> opr2->fixnum;
        break;
      default: assert(false); break;
      }
      return new_const_vreg(clamp_value(value, vtype), vtype);
    } else {
      switch (kind) {
      case IR_ADD:
      case IR_SUB:
        if (opr1->fixnum == 0)
          return opr2;
        break;
      case IR_MUL:
        if (opr1->fixnum == 1)
          return opr2;
        break;
      case IR_DIV:
      case IR_DIVU:
      case IR_MOD:
      case IR_MODU:
        if (opr1->fixnum == 0)
          return opr1;  // TODO: whether opr2 is zero.
        break;
      case IR_BITAND:
        if (opr1->fixnum == 0)
          return opr1;
        break;
      case IR_BITOR:
        if (opr1->fixnum == 0)
          return opr2;
        break;
      case IR_BITXOR:
        if (opr1->fixnum == 0)
          return opr2;
        break;
      case IR_LSHIFT:
      case IR_RSHIFT:
        if (opr1->fixnum == 0)
          return opr1;
        break;
      default:
        break;
      }
    }
  } else {
    if (opr2->flag & VRF_CONST) {
      switch (kind) {
      case IR_ADD:
      case IR_SUB:
        if (opr2->fixnum == 0)
          return opr1;
        break;
      case IR_MUL:
      case IR_DIV:
      case IR_DIVU:
        if (opr2->fixnum == 0)
          error("Divide by 0");
        if (opr2->fixnum == 1)
          return opr1;
        break;
      case IR_BITAND:
        if (opr2->fixnum == 0)
          return opr2;
        break;
      case IR_BITOR:
        if (opr2->fixnum == 0)
          return opr1;
        break;
      case IR_BITXOR:
        if (opr2->fixnum == 0)
          return opr1;
        break;
      case IR_LSHIFT:
      case IR_RSHIFT:
        if (opr2->fixnum == 0)
          return opr1;
        break;
      default:
        break;
      }
    }
  }

  IR *ir = new_ir(kind);
  ir->opr1 = opr1;
  ir->opr2 = opr2;
  ir->size = vtype->size;
  return ir->dst = reg_alloc_spawn(curra, vtype, 0);
}

VReg *new_ir_unary(enum IrKind kind, VReg *opr, const VRegType *vtype) {
  if (opr->flag & VRF_CONST) {
    intptr_t value = 0;
    switch (kind) {
    case IR_NEG:     value = -opr->fixnum; break;
    case IR_BITNOT:  value = ~opr->fixnum; break;
    default: assert(false); break;
    }
    return new_const_vreg(clamp_value(value, vtype), vtype);
  }

  IR *ir = new_ir(kind);
  ir->opr1 = opr;
  ir->size = vtype->size;
  return ir->dst = reg_alloc_spawn(curra, vtype, 0);
}

VReg *new_ir_ptradd(int offset, VReg *base, VReg *index, int scale, const VRegType *vtype) {
  IR *ir = new_ir(IR_PTRADD);
  ir->opr1 = base;
  ir->opr2 = index;
  ir->size = vtype->size;
  ir->ptradd.offset = offset;
  ir->ptradd.scale = scale;
  return ir->dst = reg_alloc_spawn(curra, vtype, 0);
}

VReg *new_ir_bofs(VReg *src) {
  IR *ir = new_ir(IR_BOFS);
  ir->opr1 = src;
  ir->size = WORD_SIZE;
  return ir->dst = reg_alloc_spawn(curra, &vtVoidPtr, 0);
}

VReg *new_ir_iofs(const Name *label, bool global) {
  IR *ir = new_ir(IR_IOFS);
  ir->iofs.label = label;
  ir->iofs.global = global;
  ir->size = WORD_SIZE;
  return ir->dst = reg_alloc_spawn(curra, &vtVoidPtr, 0);
}

VReg *new_ir_sofs(VReg *src) {
  IR *ir = new_ir(IR_SOFS);
  ir->opr1 = src;
  ir->size = WORD_SIZE;
  return ir->dst = reg_alloc_spawn(curra, &vtVoidPtr, 0);
}

void new_ir_store(VReg *dst, VReg *src) {
  IR *ir = new_ir(IR_STORE);
  ir->opr1 = src;
  ir->size = src->vtype->size;
  ir->opr2 = dst;  // `dst` is used by indirect, so it is not actually `dst`.
}

void new_ir_cmp(VReg *opr1, VReg *opr2) {
  IR *ir = new_ir(IR_CMP);
  ir->opr1 = opr1;
  ir->opr2 = opr2;
  ir->size = opr1->vtype->size;
}

void new_ir_test(VReg *reg) {
  IR *ir = new_ir(IR_TEST);
  ir->opr1 = reg;
  ir->size = reg->vtype->size;
}

void new_ir_incdec(enum IrKind kind, VReg *reg, int size, intptr_t value) {
  IR *ir = new_ir(kind);
  ir->opr1 = reg;
  ir->size = size;
  ir->value = value;
}

VReg *new_ir_cond(enum ConditionKind cond) {
  IR *ir = new_ir(IR_COND);
  ir->cond.kind = cond;
  return ir->dst = reg_alloc_spawn(curra, &vtBool, 0);
}

void new_ir_jmp(enum ConditionKind cond, BB *bb) {
  if (cond == COND_NONE)
    return;
  IR *ir = new_ir(IR_JMP);
  ir->jmp.bb = bb;
  ir->jmp.cond = cond;
}

void new_ir_pusharg(VReg *vreg, const VRegType *vtype) {
  IR *ir = new_ir(IR_PUSHARG);
  ir->opr1 = vreg;
  ir->size = vtype->size;
}

IR *new_ir_precall(int arg_count, int stack_args_size) {
  IR *ir = new_ir(IR_PRECALL);
  ir->precall.arg_count = arg_count;
  ir->precall.stack_args_size = stack_args_size;
  ir->precall.stack_aligned = false;
  ir->precall.living_pregs = 0;
  return ir;
}

VReg *new_ir_call(const Name *label, bool global, VReg *freg, int reg_arg_count,
                  const VRegType *result_type, IR *precall, VRegType **arg_vtypes) {
  IR *ir = new_ir(IR_CALL);
  ir->call.label = label;
  ir->call.global = global;
  ir->opr1 = freg;
  ir->call.precall = precall;
  ir->call.reg_arg_count = reg_arg_count;
  ir->call.arg_vtypes = arg_vtypes;
  ir->size = result_type->size;
  return ir->dst = reg_alloc_spawn(curra, result_type, 0);
}

void new_ir_result(VReg *reg) {
  IR *ir = new_ir(IR_RESULT);
  ir->opr1 = reg;
  ir->size = reg->vtype->size;
}

void new_ir_addsp(int value) {
  IR *ir = new_ir(IR_ADDSP);
  ir->value = value;
}

VReg *new_ir_cast(VReg *vreg, const VRegType *dsttype) {
  IR *ir = new_ir(IR_CAST);
  ir->opr1 = vreg;
  ir->size = dsttype->size;
  return ir->dst = reg_alloc_spawn(curra, dsttype, 0);
}

void new_ir_mov(VReg *dst, VReg *src) {
  IR *ir = new_ir(IR_MOV);
  ir->dst = dst;
  ir->opr1 = src;
  ir->size = dst->vtype->size;
}

void new_ir_memcpy(VReg *dst, VReg *src, int size) {
  if (size > 0) {
    IR *ir = new_ir(IR_MEMCPY);
    ir->opr1 = src;
    ir->opr2 = dst;
    ir->size = size;
  }
}

void new_ir_clear(VReg *reg, size_t size) {
  IR *ir = new_ir(IR_CLEAR);
  ir->size = size;
  ir->opr1 = reg;
}

void new_ir_asm(const char *asm_) {
  IR *ir = new_ir(IR_ASM);
  ir->asm_.str = asm_;
}

IR *new_ir_load_spilled(VReg *reg, int offset, int size, int flag) {
  IR *ir = new_ir(IR_LOAD_SPILLED);
  ir->value = offset;
  ir->size = size;
  ir->dst = reg;
  ir->spill.flag = flag;
  return ir;
}

IR *new_ir_store_spilled(VReg *reg, int offset, int size, int flag) {
  IR *ir = new_ir(IR_STORE_SPILLED);
  ir->value = offset;
  ir->size = size;
  ir->opr1 = reg;
  ir->spill.flag = flag;
  return ir;
}

static void ir_memcpy(int dst_reg, int src_reg, ssize_t size) {
  const char *dst = kReg64s[dst_reg];
  const char *src = kReg64s[src_reg];

  // Break %rcx, %dl
  switch (size) {
  case 1:
    MOV(INDIRECT(src, NULL, 1), DL);
    MOV(DL, INDIRECT(dst, NULL, 1));
    break;
  case 2:
    MOV(INDIRECT(src, NULL, 1), DX);
    MOV(DX, INDIRECT(dst, NULL, 1));
    break;
  case 4:
    MOV(INDIRECT(src, NULL, 1), EDX);
    MOV(EDX, INDIRECT(dst, NULL, 1));
    break;
  case 8:
    MOV(INDIRECT(src, NULL, 1), RDX);
    MOV(RDX, INDIRECT(dst, NULL, 1));
    break;
  default:
    {
      const Name *name = alloc_label();
      const char *label = fmt_name(name);
      PUSH(src);
      MOV(IM(size), RCX);
      EMIT_LABEL(label);
      MOV(INDIRECT(src, NULL, 1), DL);
      MOV(DL, INDIRECT(dst, NULL, 1));
      INC(src);
      INC(dst);
      DEC(RCX);
      JNE(label);
      POP(src);
    }
    break;
  }
}

static void ir_out(IR *ir) {
  switch (ir->kind) {
  case IR_BOFS:
    assert(!(ir->opr1->flag & VRF_CONST));
    LEA(OFFSET_INDIRECT(ir->opr1->offset, RBP, NULL, 1), kReg64s[ir->dst->phys]);
    break;

  case IR_IOFS:
    {
      const char *label = fmt_name(ir->iofs.label);
      if (ir->iofs.global)
        label = MANGLE(label);
      LEA(LABEL_INDIRECT(label, RIP), kReg64s[ir->dst->phys]);
    }
    break;

  case IR_SOFS:
    assert(ir->opr1->flag & VRF_CONST);
    LEA(OFFSET_INDIRECT(ir->opr1->fixnum, RSP, NULL, 1), kReg64s[ir->dst->phys]);
    break;

  case IR_LOAD:
#ifndef __NO_FLONUM
    if (ir->dst->vtype->flag & VRTF_FLONUM) {
      switch (ir->size) {
      case SZ_FLOAT:   MOVSS(INDIRECT(kReg64s[ir->opr1->phys], NULL, 1), kFReg64s[ir->dst->phys]); break;
      case SZ_DOUBLE:  MOVSD(INDIRECT(kReg64s[ir->opr1->phys], NULL, 1), kFReg64s[ir->dst->phys]); break;
      default: assert(false); break;
      }
      break;
    }
#endif
    {
      assert(!(ir->opr1->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(INDIRECT(kReg64s[ir->opr1->phys], NULL, 1), regs[ir->dst->phys]);
    }
    break;

  case IR_STORE:
#ifndef __NO_FLONUM
    if (ir->opr1->vtype->flag & VRTF_FLONUM) {
      switch (ir->size) {
      case SZ_FLOAT:   MOVSS(kFReg64s[ir->opr1->phys], INDIRECT(kReg64s[ir->opr2->phys], NULL, 1)); break;
      case SZ_DOUBLE:  MOVSD(kFReg64s[ir->opr1->phys], INDIRECT(kReg64s[ir->opr2->phys], NULL, 1)); break;
      default: assert(false); break;
      }
      break;
    }
#endif
    {
      assert(!(ir->opr1->flag & VRF_CONST));
      assert(!(ir->opr2->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(regs[ir->opr1->phys], INDIRECT(kReg64s[ir->opr2->phys], NULL, 1));
    }
    break;

  case IR_ADD:
    {
      assert(ir->dst->phys == ir->opr1->phys);
#ifndef __NO_FLONUM
      if (ir->dst->vtype->flag & VRTF_FLONUM) {
        const char **regs = kFReg64s;
        switch (ir->size) {
        case SZ_FLOAT:   ADDSS(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
        case SZ_DOUBLE:  ADDSD(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
        default: assert(false); break;
        }
        break;
      }
#endif
      assert(!(ir->opr1->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      if (ir->opr2->flag & VRF_CONST)
        ADD(im(ir->opr2->fixnum), regs[ir->dst->phys]);
      else
        ADD(regs[ir->opr2->phys], regs[ir->dst->phys]);
    }
    break;

  case IR_SUB:
    {
      assert(ir->dst->phys == ir->opr1->phys);
#ifndef __NO_FLONUM
      if (ir->dst->vtype->flag & VRTF_FLONUM) {
        const char **regs = kFReg64s;
        switch (ir->size) {
        case SZ_FLOAT:   SUBSS(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
        case SZ_DOUBLE:  SUBSD(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
        default: assert(false); break;
        }
        break;
      }
#endif
      assert(!(ir->opr1->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      if (ir->opr2->flag & VRF_CONST)
        SUB(im(ir->opr2->fixnum), regs[ir->dst->phys]);
      else
        SUB(regs[ir->opr2->phys], regs[ir->dst->phys]);
    }
    break;

  case IR_PTRADD:
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      VReg *base = ir->opr1;
      VReg *index = ir->opr2;
      if (index == NULL && ir->ptradd.scale == 1 && ir->ptradd.offset == 0) {
        if (ir->dst->phys == base->phys)
          ;  // No need to move.
        else
          MOV(regs[base->phys], regs[ir->dst->phys]);
      } else if (ir->dst->phys == base->phys && ir->ptradd.scale == 1 && ir->ptradd.offset == 0) {
        ADD(regs[index->phys], regs[ir->dst->phys]);
      } else if (index != NULL && ir->dst->phys == index->phys && ir->ptradd.scale == 1 && ir->ptradd.offset == 0) {
        ADD(regs[base->phys], regs[ir->dst->phys]);
      } else {
        LEA(OFFSET_INDIRECT(ir->ptradd.offset, regs[base->phys], index != NULL ? regs[index->phys] : NULL, ir->ptradd.scale), regs[ir->dst->phys]);
      }
    }
    break;

  case IR_MUL:
    {
#ifndef __NO_FLONUM
      if (ir->dst->vtype->flag & VRTF_FLONUM) {
        assert(ir->dst->phys == ir->opr1->phys);
        const char **regs = kFReg64s;
        switch (ir->size) {
        case SZ_FLOAT:   MULSS(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
        case SZ_DOUBLE:  MULSD(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
        default: assert(false); break;
        }
        break;
      }
#endif
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      assert(!(ir->opr1->flag & VRF_CONST));
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *a = kRegATable[pow];
      MOV(regs[ir->opr1->phys], a);
      const char *opr2;
      if (ir->opr2->flag & VRF_CONST) {
        MOV(im(ir->opr2->fixnum), regs[WORK_REG_NO]);
        opr2 = regs[WORK_REG_NO];
      } else {
        opr2 = regs[ir->opr2->phys];
      }
      MUL(opr2);
      MOV(a, regs[ir->dst->phys]);
    }
    break;

  case IR_DIV:
  case IR_DIVU:
    assert(!(ir->opr1->flag & VRF_CONST));
#ifndef __NO_FLONUM
    if (ir->dst->vtype->flag & VRTF_FLONUM) {
      assert(ir->dst->phys == ir->opr1->phys);
      const char **regs = kFReg64s;
      switch (ir->size) {
      case SZ_FLOAT:   DIVSS(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
      case SZ_DOUBLE:  DIVSD(regs[ir->opr2->phys], regs[ir->dst->phys]); break;
      default: assert(false); break;
      }
      break;
    }
#endif
    if (ir->size == 1) {
      if (ir->kind == IR_DIV) {
        MOVSX(kReg8s[ir->opr1->phys], AX);
        const char *opr2;
        if (ir->opr2->flag & VRF_CONST) {
          opr2 = kReg8s[WORK_REG_NO];
          MOV(im(ir->opr2->fixnum), opr2);
        } else {
          opr2 = kReg8s[ir->opr2->phys];
        }
        IDIV(opr2);
      } else {
        MOVZX(kReg8s[ir->opr1->phys], AX);
        const char *opr2;
        if (ir->opr2->flag & VRF_CONST) {
          opr2 = kReg8s[WORK_REG_NO];
          MOV(im(ir->opr2->fixnum), opr2);
        } else {
          opr2 = kReg8s[ir->opr2->phys];
        }
        DIV(opr2);
      }
      MOV(AL, kReg8s[ir->dst->phys]);
    } else {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *a = kRegATable[pow];
      MOV(regs[ir->opr1->phys], a);
      const char *opr2;
      if (ir->opr2->flag & VRF_CONST) {
        opr2 = regs[WORK_REG_NO];
        MOV(im(ir->opr2->fixnum), opr2);
      } else {
        opr2 = regs[ir->opr2->phys];
      }
      if (ir->kind == IR_DIV) {
        switch (pow) {
        case 1:  CWTL(); break;
        case 2:  CLTD(); break;
        case 3:  CQTO(); break;
        default: assert(false); break;
        }
        IDIV(opr2);
      } else {
        switch (pow) {
        case 1:  XOR(DX, DX); break;
        case 2:  XOR(EDX, EDX); break;
        case 3:  XOR(EDX, EDX); break;  // Clear 64bit register.
        default: assert(false); break;
        }
        DIV(opr2);
      }
      MOV(a, regs[ir->dst->phys]);
    }
    break;

  case IR_MOD:
  case IR_MODU:
    assert(!(ir->opr1->flag & VRF_CONST));
    if (ir->size == 1) {
      if (ir->kind == IR_MOD) {
        MOVSX(kReg8s[ir->opr1->phys], AX);
        const char *opr2;
        if (ir->opr2->flag & VRF_CONST) {
          opr2 = kReg8s[WORK_REG_NO];
          MOV(im(ir->opr2->fixnum), opr2);
        } else {
          opr2 = kReg8s[ir->opr2->phys];
        }
        IDIV(opr2);
      } else {
        MOVZX(kReg8s[ir->opr1->phys], AX);
        const char *opr2;
        if (ir->opr2->flag & VRF_CONST) {
          opr2 = kReg8s[WORK_REG_NO];
          MOV(im(ir->opr2->fixnum), opr2);
        } else {
          opr2 = kReg8s[ir->opr2->phys];
        }
        DIV(opr2);
      }
      MOV(AH, kReg8s[ir->dst->phys]);
    } else {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *a = kRegATable[pow];
      const char *d = kRegDTable[pow];
      MOV(regs[ir->opr1->phys], a);
      const char *opr2;
      if (ir->opr2->flag & VRF_CONST) {
        opr2 = regs[WORK_REG_NO];
        MOV(im(ir->opr2->fixnum), opr2);
      } else {
        opr2 = regs[ir->opr2->phys];
      }
      if (ir->kind == IR_MOD) {
        switch (pow) {
        case 1:  CWTL(); break;
        case 2:  CLTD(); break;
        case 3:  CQTO(); break;
        default: assert(false); break;
        }
        IDIV(opr2);
      } else {
        switch (pow) {
        case 1:  XOR(DX, DX); break;
        case 2:  XOR(EDX, EDX); break;
        case 3:  XOR(EDX, EDX); break;  // Clear 64bit register.
        default: assert(false); break;
        }
        DIV(opr2);
      }
      MOV(d, regs[ir->dst->phys]);
    }
    break;

  case IR_BITAND:
    {
      assert(ir->dst->phys == ir->opr1->phys);
      assert(!(ir->opr1->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      if (ir->opr2->flag & VRF_CONST)
        AND(im(ir->opr2->fixnum), regs[ir->dst->phys]);
      else
        AND(regs[ir->opr2->phys], regs[ir->dst->phys]);
    }
    break;

  case IR_BITOR:
    {
      assert(ir->dst->phys == ir->opr1->phys);
      assert(!(ir->opr1->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      if (ir->opr2->flag & VRF_CONST)
        OR(im(ir->opr2->fixnum), regs[ir->dst->phys]);
      else
        OR(regs[ir->opr2->phys], regs[ir->dst->phys]);
    }
    break;

  case IR_BITXOR:
    {
      assert(ir->dst->phys == ir->opr1->phys);
      assert(!(ir->opr1->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      if (ir->opr2->flag & VRF_CONST)
        XOR(im(ir->opr2->fixnum), regs[ir->dst->phys]);
      else
        XOR(regs[ir->opr2->phys], regs[ir->dst->phys]);
    }
    break;

  case IR_LSHIFT:
    {
      assert(ir->dst->phys == ir->opr1->phys);
      assert(!(ir->opr1->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      if (ir->opr2->flag & VRF_CONST) {
        SHL(im(ir->opr2->fixnum), regs[ir->dst->phys]);
      } else {
        MOV(kReg8s[ir->opr2->phys], CL);
        SHL(CL, regs[ir->dst->phys]);
      }
    }
    break;
  case IR_RSHIFT:
    {
      assert(ir->dst->phys == ir->opr1->phys);
      assert(!(ir->opr1->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      if (ir->opr1->vtype->flag & VRTF_UNSIGNED) {
        if (ir->opr2->flag & VRF_CONST) {
          SHR(im(ir->opr2->fixnum), regs[ir->dst->phys]);
        } else {
          MOV(kReg8s[ir->opr2->phys], CL);
          SHR(CL, regs[ir->dst->phys]);
        }
      } else {
        if (ir->opr2->flag & VRF_CONST) {
          SAR(im(ir->opr2->fixnum), regs[ir->dst->phys]);
        } else {
          MOV(kReg8s[ir->opr2->phys], CL);
          SAR(CL, regs[ir->dst->phys]);
        }
      }
    }
    break;

  case IR_CMP:
    {
#ifndef __NO_FLONUM
      if (ir->opr1->vtype->flag & VRTF_FLONUM) {
        assert(ir->opr2->vtype->flag & VRTF_FLONUM);
        switch (ir->size) {
        case SZ_FLOAT:   UCOMISS(kFReg64s[ir->opr2->phys], kFReg64s[ir->opr1->phys]); break;
        case SZ_DOUBLE:  UCOMISD(kFReg64s[ir->opr2->phys], kFReg64s[ir->opr1->phys]); break;
        default: assert(false); break;
        }
        break;
      }
#endif
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *opr1;
      if (ir->opr1->flag & VRF_CONST) {
        opr1 = kRegATable[pow];
        MOV(im(ir->opr1->fixnum), opr1);
      } else {
        opr1 = regs[ir->opr1->phys];
      }
      const char *opr2 = (ir->opr2->flag & VRF_CONST) ? im(ir->opr2->fixnum) : regs[ir->opr2->phys];
      CMP(opr2, opr1);
    }
    break;

  case IR_INC:
    {
      assert(!(ir->opr1->flag & VRF_CONST));
      const char *reg = INDIRECT(kReg64s[ir->opr1->phys], NULL, 1);
      if (ir->value == 1) {
        switch (ir->size) {
        case 1:  INCB(reg); break;
        case 2:  INCW(reg); break;
        case 4:  INCL(reg); break;
        case 8:  INCQ(reg); break;
        default:  assert(false); break;
        }
      } else {
        assert(ir->size == 8);
        intptr_t value = ir->value;
        if (value <= ((1L << 31) - 1)) {
          ADDQ(IM(value), reg);
        } else {
          MOV(IM(value), RAX);
          ADD(RAX, reg);
        }
      }
    }
    break;

  case IR_DEC:
    {
      assert(!(ir->opr1->flag & VRF_CONST));
      const char *reg = INDIRECT(kReg64s[ir->opr1->phys], NULL, 1);
      if (ir->value == 1) {
        switch (ir->size) {
        case 1:  DECB(reg); break;
        case 2:  DECW(reg); break;
        case 4:  DECL(reg); break;
        case 8:  DECQ(reg); break;
        default:  assert(false); break;
        }
      } else {
        assert(ir->size == 8);
        intptr_t value = ir->value;
        if (value <= ((1L << 31) - 1)) {
          SUBQ(IM(value), reg);
        } else {
          MOV(IM(value), RAX);
          SUB(RAX, reg);
        }
      }
    }
    break;

  case IR_NEG:
    {
      assert(ir->dst->phys == ir->opr1->phys);
      assert(!(ir->dst->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      NEG(regs[ir->dst->phys]);
    }
    break;

  case IR_BITNOT:
    {
      assert(ir->dst->phys == ir->opr1->phys);
      assert(!(ir->dst->flag & VRF_CONST));
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      NOT(regs[ir->dst->phys]);
    }
    break;

  case IR_COND:
    {
      assert(!(ir->dst->flag & VRF_CONST));
      const char *dst = kReg8s[ir->dst->phys];
      switch (ir->cond.kind) {
      case COND_EQ:  SETE(dst); break;
      case COND_NE:  SETNE(dst); break;
      case COND_LT:  SETL(dst); break;
      case COND_GT:  SETG(dst); break;
      case COND_LE:  SETLE(dst); break;
      case COND_GE:  SETGE(dst); break;
      case COND_ULT: SETB(dst); break;
      case COND_UGT: SETA(dst); break;
      case COND_ULE: SETBE(dst); break;
      case COND_UGE: SETAE(dst); break;
      default: assert(false); break;
      }
      MOVSX(dst, kReg32s[ir->dst->phys]);  // Assume bool is 4 byte.
    }
    break;

  case IR_TEST:
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *opr1;
      if (ir->opr1->flag & VRF_CONST) {
        opr1 = kRegATable[pow];
        MOV(im(ir->opr1->fixnum), opr1);
      } else {
        opr1 = regs[ir->opr1->phys];
      }
      TEST(opr1, opr1);
    }
    break;

  case IR_JMP:
    switch (ir->jmp.cond) {
    case COND_ANY:  JMP(fmt_name(ir->jmp.bb->label)); break;
    case COND_EQ:   JE(fmt_name(ir->jmp.bb->label)); break;
    case COND_NE:   JNE(fmt_name(ir->jmp.bb->label)); break;
    case COND_LT:   JL(fmt_name(ir->jmp.bb->label)); break;
    case COND_GT:   JG(fmt_name(ir->jmp.bb->label)); break;
    case COND_LE:   JLE(fmt_name(ir->jmp.bb->label)); break;
    case COND_GE:   JGE(fmt_name(ir->jmp.bb->label)); break;
    case COND_ULT:  JB(fmt_name(ir->jmp.bb->label)); break;
    case COND_UGT:  JA(fmt_name(ir->jmp.bb->label)); break;
    case COND_ULE:  JBE(fmt_name(ir->jmp.bb->label)); break;
    case COND_UGE:  JAE(fmt_name(ir->jmp.bb->label)); break;
    default:  assert(false); break;
    }
    break;

  case IR_PRECALL:
    {
      // Make room for caller save.
      int add = 0;
      unsigned short living_pregs = ir->precall.living_pregs;
      for (int i = 0; i < CALLER_SAVE_REG_COUNT; ++i) {
        int ireg = kCallerSaveRegs[i];
        if (living_pregs & (1 << ireg))
          add += WORD_SIZE;
      }
#ifndef __NO_FLONUM
      for (int i = 0; i < CALLER_SAVE_FREG_COUNT; ++i) {
        int freg = kCallerSaveFRegs[i];
        if (living_pregs & (1 << (freg + PHYSICAL_REG_MAX)))
          add += WORD_SIZE;
      }
#endif

      int align_stack = (16 - (stackpos + add + ir->precall.stack_args_size)) & 15;
      ir->precall.stack_aligned = align_stack;
      add += align_stack;

      if (add > 0) {
        SUB(IM(add), RSP); stackpos += add;
      }
    }
    break;

  case IR_PUSHARG:
#ifndef __NO_FLONUM
    if (ir->opr1->vtype->flag & VRTF_FLONUM) {
      SUB(IM(WORD_SIZE), RSP); PUSH_STACK_POS();
      switch (ir->opr1->vtype->size) {
      case SZ_FLOAT:   MOVSS(kFReg64s[ir->opr1->phys], INDIRECT(RSP, NULL, 1)); break;
      case SZ_DOUBLE:  MOVSD(kFReg64s[ir->opr1->phys], INDIRECT(RSP, NULL, 1)); break;
      default: assert(false); break;
      }
      break;
    }
#endif
    if (ir->opr1->flag & VRF_CONST) {
      if (is_im32(ir->opr1->fixnum)) {
        PUSH(im(ir->opr1->fixnum)); PUSH_STACK_POS();
      } else {
        MOV(im(ir->opr1->fixnum), RAX);  // TODO: Check.
        PUSH(RAX); PUSH_STACK_POS();
      }
    } else {
      PUSH(kReg64s[ir->opr1->phys]); PUSH_STACK_POS();
    }
    break;

  case IR_CALL:
    {
      int reg_args = ir->call.reg_arg_count;
      push_caller_save_regs(ir->call.precall->precall.living_pregs, reg_args * WORD_SIZE + ir->call.precall->precall.stack_args_size + ir->call.precall->precall.stack_aligned);

      static const char *kArgReg64s[] = {RDI, RSI, RDX, RCX, R8, R9};
#ifndef __NO_FLONUM
      static const char *kArgFReg64s[] = {XMM0, XMM1, XMM2, XMM3, XMM4, XMM5};
      int freg = 0;
#endif

      // Pop register arguments.
      int ireg = 0;
      for (int i = 0; i < reg_args; ++i) {
#ifndef __NO_FLONUM
        if (ir->call.arg_vtypes[i]->flag & VRTF_FLONUM) {
          switch (ir->call.arg_vtypes[i]->size) {
          case SZ_FLOAT:   MOVSS(INDIRECT(RSP, NULL, 1), kArgFReg64s[freg]); break;
          case SZ_DOUBLE:  MOVSD(INDIRECT(RSP, NULL, 1), kArgFReg64s[freg]); break;
          default: assert(false); break;
          }
          ++freg;
          ADD(IM(WORD_SIZE), RSP); POP_STACK_POS();
          continue;
        }
#endif
        POP(kArgReg64s[ireg++]); POP_STACK_POS();
      }

      if (ir->call.label != NULL) {
        const char *label = fmt_name(ir->call.label);
        if (ir->call.global)
          CALL(MANGLE(label));
        else
          CALL(label);
      } else {
        assert(!(ir->opr1->flag & VRF_CONST));
        CALL(fmt("*%s", kReg64s[ir->opr1->phys]));
      }

      int align_stack = ir->call.precall->precall.stack_aligned + ir->call.precall->precall.stack_args_size;
      if (align_stack != 0) {
        ADD(IM(align_stack), RSP);
        stackpos -= align_stack;
      }

      // Resore caller save registers.
      pop_caller_save_regs(ir->call.precall->precall.living_pregs);

#ifndef __NO_FLONUM
      if (ir->dst->vtype->flag & VRTF_FLONUM) {
        switch (ir->size) {
        case SZ_FLOAT:   MOVSS(XMM0, kFReg64s[ir->dst->phys]); break;
        case SZ_DOUBLE:  MOVSD(XMM0, kFReg64s[ir->dst->phys]); break;
        default: assert(false); break;
        }
        break;
      }
#endif
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      if (ir->size > 0) {
        int pow = kPow2Table[ir->size];
        assert(0 <= pow && pow < 4);
        const char **regs = kRegSizeTable[pow];
        MOV(kRegATable[pow], regs[ir->dst->phys]);
      }
    }
    break;

  case IR_RESULT:
#ifndef __NO_FLONUM
    if (ir->opr1->vtype->flag & VRTF_FLONUM) {
      switch (ir->size) {
      case SZ_FLOAT:   MOVSS(kFReg64s[ir->opr1->phys], XMM0); break;
      case SZ_DOUBLE:  MOVSD(kFReg64s[ir->opr1->phys], XMM0); break;
      default: assert(false); break;
      }
      break;
    }
#endif
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      if (ir->opr1->flag & VRF_CONST)
        MOV(im(ir->opr1->fixnum), kRegATable[pow]);
      else
        MOV(regs[ir->opr1->phys], kRegATable[pow]);
    }
    break;

  case IR_ADDSP:
    if (ir->value > 0)
      ADD(IM(ir->value), RSP);
    else if (ir->value < 0)
      SUB(IM(-ir->value), RSP);
    stackpos -= ir->value;
    break;

  case IR_CAST:
    assert((ir->opr1->flag & VRF_CONST) == 0);
#ifndef __NO_FLONUM
    if (ir->dst->vtype->flag & VRTF_FLONUM) {
      if (ir->opr1->vtype->flag & VRTF_FLONUM) {
        // flonum->flonum
        // Assume flonum are just two types.
        switch (ir->size) {
        case SZ_FLOAT:   CVTSD2SS(kFReg64s[ir->opr1->phys], kFReg64s[ir->dst->phys]); break;
        case SZ_DOUBLE:  CVTSS2SD(kFReg64s[ir->opr1->phys], kFReg64s[ir->dst->phys]); break;
        default:  assert(false); break;
        }
      } else {
        // fix->flonum
        assert(0 <= ir->opr1->vtype->size && ir->opr1->vtype->size < kPow2TableSize);
        int pows = kPow2Table[ir->opr1->vtype->size];
        if (pows < 2) {
          if (ir->opr1->vtype->flag & VRTF_UNSIGNED)
            MOVZX(kRegSizeTable[pows][ir->opr1->phys], kRegSizeTable[2][ir->opr1->phys]);
          else
            MOVSX(kRegSizeTable[pows][ir->opr1->phys], kRegSizeTable[2][ir->opr1->phys]);
          pows = 2;
        }
        switch (ir->size) {
        case SZ_FLOAT:   CVTSI2SS(kRegSizeTable[pows][ir->opr1->phys], kFReg64s[ir->dst->phys]); break;
        case SZ_DOUBLE:  CVTSI2SD(kRegSizeTable[pows][ir->opr1->phys], kFReg64s[ir->dst->phys]); break;
        default:  assert(false); break;
        }
      }
      break;
    } else if (ir->opr1->vtype->flag & VRTF_FLONUM) {
      // flonum->fix
      int powd = kPow2Table[ir->dst->vtype->size];
      switch (ir->opr1->vtype->size) {
      case SZ_FLOAT:   CVTTSS2SI(kFReg64s[ir->opr1->phys], kRegSizeTable[powd][ir->dst->phys]); break;
      case SZ_DOUBLE:  CVTTSD2SI(kFReg64s[ir->opr1->phys], kRegSizeTable[powd][ir->dst->phys]); break;
      default:  assert(false); break;
      }
      break;
    }
#endif
    if (ir->size <= ir->opr1->vtype->size) {
      if (ir->dst->phys != ir->opr1->phys) {
        assert(0 <= ir->size && ir->size < kPow2TableSize);
        int pow = kPow2Table[ir->size];
        assert(0 <= pow && pow < 4);
        const char **regs = kRegSizeTable[pow];
        MOV(regs[ir->opr1->phys], regs[ir->dst->phys]);
      }
    } else {
      assert(0 <= ir->opr1->vtype->size && ir->opr1->vtype->size < kPow2TableSize);
      int pows = kPow2Table[ir->opr1->vtype->size];
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int powd = kPow2Table[ir->size];
      assert(0 <= pows && pows < 4);
      assert(0 <= powd && powd < 4);
      if (ir->opr1->vtype->flag & VRTF_UNSIGNED) {
        if (pows == 2 && powd == 3) {
          // MOVZX %64bit, %32bit doesn't exist!
          MOV(kRegSizeTable[pows][ir->opr1->phys], kRegSizeTable[pows][ir->dst->phys]);
        } else {
          MOVZX(kRegSizeTable[pows][ir->opr1->phys], kRegSizeTable[powd][ir->dst->phys]);
        }
      } else {
        MOVSX(kRegSizeTable[pows][ir->opr1->phys], kRegSizeTable[powd][ir->dst->phys]);
      }
    }
    break;

  case IR_MOV:
    {
#ifndef __NO_FLONUM
      if (ir->dst->vtype->flag & VRTF_FLONUM) {
        if (ir->opr1->phys != ir->dst->phys) {
          switch (ir->size) {
          case SZ_FLOAT:   MOVSS(kFReg64s[ir->opr1->phys], kFReg64s[ir->dst->phys]); break;
          case SZ_DOUBLE:  MOVSD(kFReg64s[ir->opr1->phys], kFReg64s[ir->dst->phys]); break;
          default: assert(false); break;
          }
          break;
        }
      }
#endif
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      assert(!(ir->dst->flag & VRF_CONST));
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      if (ir->opr1->flag & VRF_CONST) {
        MOV(im(ir->opr1->fixnum), regs[ir->dst->phys]);
      } else {
        if (ir->opr1->phys != ir->dst->phys)
          MOV(regs[ir->opr1->phys], regs[ir->dst->phys]);
      }
    }
    break;

  case IR_MEMCPY:
    assert(!(ir->opr1->flag & VRF_CONST));
    assert(!(ir->opr2->flag & VRF_CONST));
    ir_memcpy(ir->opr2->phys, ir->opr1->phys, ir->size);
    break;

  case IR_CLEAR:
    {
      assert(!(ir->opr1->flag & VRF_CONST));
      const char *loop = fmt_name(alloc_label());
      MOV(kReg64s[ir->opr1->phys], RSI);
      MOV(IM(ir->size), EDI);
      XOR(AL, AL);
      EMIT_LABEL(loop);
      MOV(AL, INDIRECT(RSI, NULL, 1));
      INC(RSI);
      DEC(EDI);
      JNE(loop);
    }
    break;

  case IR_ASM:
    EMIT_ASM0(ir->asm_.str);
    break;

  case IR_LOAD_SPILLED:
#ifndef __NO_FLONUM
    if (ir->spill.flag & VRTF_FLONUM) {
      const char **regs = kFReg64s;
      switch (ir->size) {
      case SZ_FLOAT:   MOVSS(OFFSET_INDIRECT(ir->value, RBP, NULL, 1), regs[WORK_REG_NO]); break;
      case SZ_DOUBLE:  MOVSD(OFFSET_INDIRECT(ir->value, RBP, NULL, 1), regs[WORK_REG_NO]); break;
      default: assert(false); break;
      }
      break;
    }
#endif
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(OFFSET_INDIRECT(ir->value, RBP, NULL, 1), regs[WORK_REG_NO]);
    }
    break;

  case IR_STORE_SPILLED:
#ifndef __NO_FLONUM
    if (ir->spill.flag & VRTF_FLONUM) {
      const char **regs = kFReg64s;
      switch (ir->size) {
      case SZ_FLOAT:   MOVSS(regs[WORK_REG_NO], OFFSET_INDIRECT(ir->value, RBP, NULL, 1)); break;
      case SZ_DOUBLE:  MOVSD(regs[WORK_REG_NO], OFFSET_INDIRECT(ir->value, RBP, NULL, 1)); break;
      default: assert(false); break;
      }
      break;
    }
#endif
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(regs[WORK_REG_NO], OFFSET_INDIRECT(ir->value, RBP, NULL, 1));
    }
    break;

  default:
    assert(false);
    break;
  }
}

// Basic Block

BB *curbb;

BB *new_bb(void) {
  BB *bb = malloc(sizeof(*bb));
  bb->next = NULL;
  bb->label = alloc_label();
  bb->irs = new_vector();
  bb->in_regs = NULL;
  bb->out_regs = NULL;
  bb->assigned_regs = NULL;
  return bb;
}

//

BBContainer *new_func_blocks(void) {
  BBContainer *bbcon = malloc(sizeof(*bbcon));
  bbcon->bbs = new_vector();
  return bbcon;
}

static IR *is_last_jmp(BB *bb) {
  int len;
  IR *ir;
  if ((len = bb->irs->len) > 0 &&
      (ir = bb->irs->data[len - 1])->kind == IR_JMP)
    return ir;
  return NULL;
}

static IR *is_last_any_jmp(BB *bb) {
  IR *ir = is_last_jmp(bb);
  return ir != NULL && ir->jmp.cond == COND_ANY ? ir : NULL;
}

static void replace_jmp_destination(BBContainer *bbcon, BB *src, BB *dst) {
  Vector *bbs = bbcon->bbs;
  for (int j = 0; j < bbs->len; ++j) {
    BB *bb = bbs->data[j];
    if (bb == src)
      continue;

    IR *ir = is_last_jmp(bb);
    if (ir != NULL && ir->jmp.bb == src)
      ir->jmp.bb = dst;
  }
}

void remove_unnecessary_bb(BBContainer *bbcon) {
  Vector *bbs = bbcon->bbs;
  for (;;) {
    bool again = false;
    for (int i = 0; i < bbs->len - 1; ++i) {  // Make last one keeps alive.
      BB *bb = bbs->data[i];
      IR *ir;
      if (bb->irs->len == 0) {  // Empty BB.
        replace_jmp_destination(bbcon, bb, bb->next);
      } else if (bb->irs->len == 1 && (ir = is_last_any_jmp(bb)) != NULL && !equal_name(bb->label, ir->jmp.bb->label)) {  // jmp only.
        replace_jmp_destination(bbcon, bb, ir->jmp.bb);
        if (i == 0)
          continue;
        BB *pbb = bbs->data[i - 1];
        if (!is_last_jmp(pbb))
          continue;
        if (!is_last_any_jmp(pbb)) {  // Fallthrough pass exists.
          IR *ir0 = pbb->irs->data[pbb->irs->len - 1];
          if (ir0->jmp.bb != bb->next)  // Non skip jmp: Keep bb connection.
            continue;
          // Invert prev jmp condition and change jmp destination.
          ir0->jmp.cond = invert_cond(ir0->jmp.cond);
          ir0->jmp.bb = ir->jmp.bb;
        }
      } else {
        continue;
      }

      if (i > 0) {
        BB *pbb = bbs->data[i - 1];
        pbb->next = bb->next;
      }

      vec_remove_at(bbs, i);
      --i;
      again = true;
    }
    if (!again)
      break;
  }

  // Remove jmp to next instruction.
  for (int i = 0; i < bbs->len - 1; ++i) {  // Make last one keeps alive.
    BB *bb = bbs->data[i];
    IR *ir = is_last_any_jmp(bb);
    if (ir != NULL && ir->jmp.bb == bb->next)
      vec_pop(bb->irs);
  }
}

void push_callee_save_regs(unsigned short used) {
  for (int i = 0; i < CALLEE_SAVE_REG_COUNT; ++i) {
    int ireg = kCalleeSaveRegs[i];
    if (used & (1 << ireg)) {
      PUSH(kReg64s[ireg]); PUSH_STACK_POS();
    }
  }
}

void pop_callee_save_regs(unsigned short used) {
  for (int i = CALLEE_SAVE_REG_COUNT; --i >= 0;) {
    int ireg = kCalleeSaveRegs[i];
    if (used & (1 << ireg)) {
      POP(kReg64s[ireg]); POP_STACK_POS();
    }
  }
}

static void push_caller_save_regs(unsigned short living, int base) {
#ifndef __NO_FLONUM
  {
    for (int i = CALLER_SAVE_FREG_COUNT; i > 0; ) {
      int ireg = kCallerSaveFRegs[--i];
      if (living & (1U << (ireg + PHYSICAL_REG_MAX))) {
        // TODO: Detect register size.
        MOVSD(kFReg64s[ireg], OFFSET_INDIRECT(base, RSP, NULL, 1));
        base += WORD_SIZE;
      }
    }
  }
#endif

  for (int i = CALLER_SAVE_REG_COUNT; i > 0; ) {
    int ireg = kCallerSaveRegs[--i];
    if (living & (1 << ireg)) {
      MOV(kReg64s[ireg], OFFSET_INDIRECT(base, RSP, NULL, 1));
      base += WORD_SIZE;
    }
  }
}

static void pop_caller_save_regs(unsigned short living) {
#ifndef __NO_FLONUM
  {
    int count = 0;
    for (int i = CALLER_SAVE_FREG_COUNT; i > 0; ) {
      int ireg = kCallerSaveFRegs[--i];
      if (living & (1U << (ireg + PHYSICAL_REG_MAX))) {
        // TODO: Detect register size.
        MOVSD(OFFSET_INDIRECT(count * WORD_SIZE, RSP, NULL, 1), kFReg64s[ireg]);
        ++count;
      }
    }
    if (count > 0) {
      ADD(IM(WORD_SIZE * count), RSP); stackpos -= WORD_SIZE * count;
    }
  }
#endif

  for (int i = CALLER_SAVE_REG_COUNT; --i >= 0;) {
    int ireg = kCallerSaveRegs[i];
    if (living & (1 << ireg)) {
      POP(kReg64s[ireg]); POP_STACK_POS();
    }
  }
}

void emit_bb_irs(BBContainer *bbcon) {
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
#ifndef NDEBUG
    // Check BB connection.
    if (i < bbcon->bbs->len - 1) {
      BB *nbb = bbcon->bbs->data[i + 1];
      UNUSED(nbb);
      assert(bb->next == nbb);
    } else {
      assert(bb->next == NULL);
    }
#endif

    EMIT_LABEL(fmt_name(bb->label));
    for (int j = 0; j < bb->irs->len; ++j) {
      IR *ir = bb->irs->data[j];
      ir_out(ir);
    }
  }
}

// Rewrite `A = B op C` to `A = B; A = A op C`.
static void three_to_two(BB *bb) {
  Vector *irs = bb->irs;
  for (int i = 0; i < irs->len; ++i) {
    IR *ir = bb->irs->data[i];

    switch (ir->kind) {
    case IR_ADD:  // binops
    case IR_SUB:
    case IR_MUL:
    case IR_DIV:
    case IR_DIVU:
    case IR_MOD:
    case IR_MODU:
    case IR_BITAND:
    case IR_BITOR:
    case IR_BITXOR:
    case IR_LSHIFT:
    case IR_RSHIFT:
    case IR_NEG:  // unary ops
    case IR_BITNOT:
      {
        assert(!(ir->dst->flag & VRF_CONST));
        IR *ir2 = malloc(sizeof(*ir2));
        ir2->kind = IR_MOV;
        ir2->dst = ir->dst;
        ir2->opr1 = ir->opr1;
        ir2->opr2 = NULL;
        ir2->size = ir->size;
        vec_insert(irs, i, ir2);

        ir->opr1 = ir->dst;
        ++i;
      }
      break;

    default:
      break;
    }
  }
  bb->irs = irs;
}

void convert_3to2(BBContainer *bbcon) {
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    three_to_two(bb);
  }
}
#include "ir_debug.h"

#if !defined(SELF_HOSTING) && !defined(__XV6)
#include <assert.h>
#include <inttypes.h>
#include <stdio.h>

#include "ast.h"
#include "ir.h"
#include "regalloc.h"
#include "table.h"
#include "util.h"
#include "var.h"

static void dump_vreg(FILE *fp, VReg *vreg, int size) {
  assert(vreg != NULL);
  static char *kSize[] = {"0", "b", "w", "3", "d", "5", "6", "7", ""};
  if (vreg->flag & VRF_CONST) {
    fprintf(fp, "(%" PRIdPTR ")", vreg->fixnum);
  } else {
    char regtype = 'R';
#ifndef __NO_FLONUM
    if (vreg->vtype->flag & VRTF_FLONUM)
      regtype = 'F';
#endif
    fprintf(fp, "%c%d%s<v%d>", regtype, vreg->phys, kSize[size], vreg->virt);
  }
}

static void dump_ir(FILE *fp, IR *ir) {
  static char *kSize[] = {"0", "b", "w", "3", "d", "5", "6", "7", ""};
  static char *kCond[] = {"__", "MP", "EQ", "NE", "LT", "LE", "GE", "GT", "ULT", "ULE", "UGE", "UGT"};

  switch (ir->kind) {
  case IR_BOFS:   fprintf(fp, "\tBOFS\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = &[rbp %c %d]\n", ir->opr1->offset >= 0 ? '+' : '-', ir->opr1->offset > 0 ? ir->opr1->offset : -ir->opr1->offset); break;
  case IR_IOFS:   fprintf(fp, "\tIOFS\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = &%.*s\n", ir->iofs.label->bytes, ir->iofs.label->chars); break;
  case IR_SOFS:   fprintf(fp, "\tSOFS\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = &[rsp %c %ld]\n", ir->opr1->fixnum >= 0 ? '+' : '-', ir->opr1->fixnum > 0 ? ir->opr1->fixnum : -ir->opr1->fixnum); break;
  case IR_LOAD:   fprintf(fp, "\tLOAD\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = ["); dump_vreg(fp, ir->opr1, WORD_SIZE); fprintf(fp, "]\n"); break;
  case IR_STORE:  fprintf(fp, "\tSTORE\t["); dump_vreg(fp, ir->opr2, WORD_SIZE); fprintf(fp, "] = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, "\n"); break;
  case IR_ADD:    fprintf(fp, "\tADD\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, " + "); dump_vreg(fp, ir->opr2, ir->size); fprintf(fp, "\n"); break;
  case IR_SUB:    fprintf(fp, "\tSUB\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, " - "); dump_vreg(fp, ir->opr2, ir->size); fprintf(fp, "\n"); break;
  case IR_MUL:    fprintf(fp, "\tMUL\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, " * "); dump_vreg(fp, ir->opr2, ir->size); fprintf(fp, "\n"); break;
  case IR_DIV:    fprintf(fp, "\tDIV\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, " / "); dump_vreg(fp, ir->opr2, ir->size); fprintf(fp, "\n"); break;
  case IR_DIVU:   fprintf(fp, "\tDIVU\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, " / "); dump_vreg(fp, ir->opr2, ir->size); fprintf(fp, "\n"); break;
  case IR_MOD:    fprintf(fp, "\tMOD\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, " %% "); dump_vreg(fp, ir->opr2, ir->size); fprintf(fp, "\n"); break;
  case IR_MODU:   fprintf(fp, "\tMODU\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, " %% "); dump_vreg(fp, ir->opr2, ir->size); fprintf(fp, "\n"); break;
  case IR_PTRADD:
    fprintf(fp, "\tPTRADD\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size);
    if (ir->opr2 != NULL) {
      fprintf(fp, " + "); dump_vreg(fp, ir->opr2, ir->size);
      if (ir->ptradd.scale != 1)
        fprintf(fp, " * %d", ir->ptradd.scale);
    }
    if (ir->ptradd.offset != 0)
      fprintf(fp, " + %d", ir->ptradd.offset);
    fprintf(fp, "\n");
    break;
  case IR_BITAND: fprintf(fp, "\tBITAND\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, " & "); dump_vreg(fp, ir->opr2, ir->size); fprintf(fp, "\n"); break;
  case IR_BITOR:  fprintf(fp, "\tBITOR\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, " | "); dump_vreg(fp, ir->opr2, ir->size); fprintf(fp, "\n"); break;
  case IR_BITXOR: fprintf(fp, "\tBITXOR\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, " ^ "); dump_vreg(fp, ir->opr2, ir->size); fprintf(fp, "\n"); break;
  case IR_LSHIFT: fprintf(fp, "\tLSHIFT\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, " << "); dump_vreg(fp, ir->opr2, ir->size); fprintf(fp, "\n"); break;
  case IR_RSHIFT: fprintf(fp, "\tRSHIFT\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, " >> "); dump_vreg(fp, ir->opr2, ir->size); fprintf(fp, "\n"); break;
  case IR_CMP:    fprintf(fp, "\tCMP\t"); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, " - "); dump_vreg(fp, ir->opr2, ir->size); fprintf(fp, "\n"); break;
  case IR_INC:    fprintf(fp, "\tINC\t"); dump_vreg(fp, ir->opr1, WORD_SIZE); fprintf(fp, "%s += %" PRIdPTR "\n", kSize[ir->size], ir->value); break;
  case IR_DEC:    fprintf(fp, "\tDEC\t"); dump_vreg(fp, ir->opr1, WORD_SIZE); fprintf(fp, "%s -= %" PRIdPTR "\n", kSize[ir->size], ir->value); break;
  case IR_NEG:    fprintf(fp, "\tNEG\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = -"); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, "\n"); break;
  case IR_BITNOT: fprintf(fp, "\tBITNOT\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = ~"); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, "\n"); break;
  case IR_COND:    fprintf(fp, "\tCOND\t"); dump_vreg(fp, ir->dst, 4); fprintf(fp, " = %s\n", kCond[ir->cond.kind]); break;
  case IR_TEST:   fprintf(fp, "\tTEST\t"); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, "\n"); break;
  case IR_JMP:    fprintf(fp, "\tJ%s\t%.*s\n", kCond[ir->jmp.cond], ir->jmp.bb->label->bytes, ir->jmp.bb->label->chars); break;
  case IR_PRECALL: fprintf(fp, "\tPRECALL\n"); break;
  case IR_PUSHARG: fprintf(fp, "\tPUSHARG\t"); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, "\n"); break;
  case IR_CALL:
    if (ir->call.label != NULL) {
      fprintf(fp, "\tCALL\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = call %.*s(args=#%d)\n", ir->call.label->bytes, ir->call.label->chars, ir->call.reg_arg_count);
    } else {
      fprintf(fp, "\tCALL\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = *"); dump_vreg(fp, ir->opr1, WORD_SIZE); fprintf(fp, "(args=#%d)\n", ir->call.reg_arg_count);
    }
    break;
  case IR_RESULT: fprintf(fp, "\tRESULT\t"); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, "\n"); break;
  case IR_ADDSP:  fprintf(fp, "\tADDSP\t%" PRIdPTR "\n", ir->value); break;
  case IR_CAST:   fprintf(fp, "\tCAST\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->opr1->vtype->size); fprintf(fp, "\n"); break;
  case IR_MOV:    fprintf(fp, "\tMOV\t"); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = "); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, "\n"); break;
  case IR_MEMCPY: fprintf(fp, "\tMEMCPY(dst="); dump_vreg(fp, ir->opr2, WORD_SIZE); fprintf(fp, ", src="); dump_vreg(fp, ir->opr1, WORD_SIZE); fprintf(fp, ", size=%d)\n", ir->size); break;
  case IR_CLEAR:  fprintf(fp, "\tCLEAR\t"); dump_vreg(fp, ir->opr1, WORD_SIZE); fprintf(fp, ", %d\n", ir->size); break;
  case IR_ASM:    fprintf(fp, "\tASM \"%s\"\n", ir->asm_.str); break;
  case IR_LOAD_SPILLED:   fprintf(fp, "\tLOAD_SPILLED "); dump_vreg(fp, ir->dst, ir->size); fprintf(fp, " = [rbp %+d]\n", (int)ir->value); break;
  case IR_STORE_SPILLED:  fprintf(fp, "\tSTORE_SPILLED [rbp %+d] = ", (int)ir->value); dump_vreg(fp, ir->opr1, ir->size); fprintf(fp, "\n"); break;

  default: assert(false); break;
  }
}

static void dump_func_ir(Function *func) {
  FILE *fp = stdout;

  if (func->scopes == NULL)  // Prototype definition
    return;

  BBContainer *bbcon = func->bbcon;
  assert(bbcon != NULL);

  fprintf(fp, "### %.*s\n\n", func->name->bytes, func->name->chars);

  fprintf(fp, "params and locals:\n");
  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = func->scopes->data[i];
    if (scope->vars == NULL)
      continue;
    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      if (varinfo->storage & (VS_STATIC | VS_EXTERN | VS_ENUM_MEMBER) ||
          varinfo->local.reg == NULL)
        continue;
      fprintf(fp, "  V%3d (flag=%x): %.*s\n", varinfo->local.reg->virt, varinfo->local.reg->flag,
              varinfo->name->bytes, varinfo->name->chars);
    }
  }

  RegAlloc *ra = func->ra;
  fprintf(fp, "VREG: #%d\n", ra->vregs->len);
  LiveInterval **sorted_intervals = func->ra->sorted_intervals;
  for (int i = 0; i < ra->vregs->len; ++i) {
    LiveInterval *li = sorted_intervals[i];
    VReg *vreg = ra->vregs->data[li->virt];
    switch (li->state) {
    case LI_NORMAL:
      {
        char regtype = 'R';
#ifndef __NO_FLONUM
        if (vreg->vtype->flag & VRTF_FLONUM)
          regtype = 'F';
#endif
        fprintf(fp, "  V%3d (flag=%x): live %3d - %3d, => %c%3d\n", li->virt, vreg->flag, li->start, li->end, regtype, li->phys);
      }
      break;
    case LI_SPILL:
      fprintf(fp, "  V%3d (flag=%x): live %3d - %3d (spilled, offset=%d)\n", li->virt, vreg->flag, li->start, li->end, vreg->offset);
      break;
    case LI_CONST:
      fprintf(fp, "  V%3d (flag=%x): (const, value=%" PRIdPTR ")\n", li->virt, vreg->flag, vreg->fixnum);
      break;
    default:  assert(false); break;
    }
  }

  fprintf(fp, "BB: #%d\n", bbcon->bbs->len);
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    fprintf(fp, "// BB %d\n", i);
    fprintf(fp, "%.*s:\n", bb->label->bytes, bb->label->chars);
    for (int j = 0; j < bb->irs->len; ++j) {
      IR *ir = bb->irs->data[j];
      dump_ir(fp, ir);
    }
  }
  fprintf(fp, "\n");
}

void do_dump_ir(Vector *toplevel) {
  for (int i = 0, len = toplevel->len; i < len; ++i) {
    Declaration *decl = toplevel->data[i];
    if (decl == NULL)
      continue;

    switch (decl->kind) {
    case DCL_DEFUN:
      dump_func_ir(decl->defun.func);
      break;
    case DCL_VARDECL:
      break;

    default:
      assert(false);
      break;
    }
  }
}
#endif
#include "lexer.h"

#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdlib.h>  // malloc, strtoul
#include <string.h>
#include <sys/types.h>  // ssize_t

#include "table.h"
#include "util.h"

#define MAX_LOOKAHEAD  (2)

static const struct {
  const char *str;
  enum TokenKind kind;
} kReservedWords[] = {
  {"if", TK_IF},
  {"else", TK_ELSE},
  {"switch", TK_SWITCH},
  {"case", TK_CASE},
  {"default", TK_DEFAULT},
  {"do", TK_DO},
  {"while", TK_WHILE},
  {"for", TK_FOR},
  {"break", TK_BREAK},
  {"continue", TK_CONTINUE},
  {"goto", TK_GOTO},
  {"return", TK_RETURN},
  {"void", TK_VOID},
  {"char", TK_CHAR},
  {"short", TK_SHORT},
  {"int", TK_INT},
  {"long", TK_LONG},
  {"const", TK_CONST},
  {"unsigned", TK_UNSIGNED},
  {"signed", TK_SIGNED},
  {"static", TK_STATIC},
  {"extern", TK_EXTERN},
  {"volatile", TK_VOLATILE},
  {"struct", TK_STRUCT},
  {"union", TK_UNION},
  {"enum", TK_ENUM},
  {"sizeof", TK_SIZEOF},
  {"typedef", TK_TYPEDEF},
  {"__asm", TK_ASM},
#ifndef __NO_FLONUM
  {"float", TK_FLOAT},
  {"double", TK_DOUBLE},
#endif
};

static const struct {
  const char ident[4];
  enum TokenKind kind;
} kMultiOperators[] = {
  {"<<=", TK_LSHIFT_ASSIGN},
  {">>=", TK_RSHIFT_ASSIGN},
  {"...", TK_ELLIPSIS},
  {"==", TK_EQ},
  {"!=", TK_NE},
  {"<=", TK_LE},
  {">=", TK_GE},
  {"+=", TK_ADD_ASSIGN},
  {"-=", TK_SUB_ASSIGN},
  {"*=", TK_MUL_ASSIGN},
  {"/=", TK_DIV_ASSIGN},
  {"%=", TK_MOD_ASSIGN},
  {"&=", TK_AND_ASSIGN},
  {"|=", TK_OR_ASSIGN},
  {"^=", TK_HAT_ASSIGN},
  {"++", TK_INC},
  {"--", TK_DEC},
  {"->", TK_ARROW},
  {"&&", TK_LOGAND},
  {"||", TK_LOGIOR},
  {"<<", TK_LSHIFT},
  {">>", TK_RSHIFT},

  {"##", PPTK_CONCAT},
};

static const char kSingleOperatorTypeMap[128] = {  // enum TokenKind
  ['+'] = TK_ADD,
  ['-'] = TK_SUB,
  ['*'] = TK_MUL,
  ['/'] = TK_DIV,
  ['%'] = TK_MOD,
  ['&'] = TK_AND,
  ['|'] = TK_OR,
  ['^'] = TK_HAT,
  ['<'] = TK_LT,
  ['>'] = TK_GT,
  ['!'] = TK_NOT,
  ['('] = TK_LPAR,
  [')'] = TK_RPAR,
  ['{'] = TK_LBRACE,
  ['}'] = TK_RBRACE,
  ['['] = TK_LBRACKET,
  [']'] = TK_RBRACKET,
  ['='] = TK_ASSIGN,
  [':'] = TK_COLON,
  [';'] = TK_SEMICOL,
  [','] = TK_COMMA,
  ['.'] = TK_DOT,
  ['?'] = TK_QUESTION,
  ['~'] = TK_TILDA,

  ['#'] = PPTK_STRINGIFY,
};

typedef struct {
  FILE *fp;
  const char *filename;
  Line *line;
  const char *p;
  Token *fetched[MAX_LOOKAHEAD];
  int idx;
  int lineno;
} Lexer;

static Lexer lexer;

static Table reserved_word_table;

static void show_error_line(const char *line, const char *p, int len) {
  fprintf(stderr, "%s\n", line);
  size_t pos = p - line;
  if (pos <= strlen(line)) {
    for (size_t i = 0; i < pos; ++i)
      fputc(line[i] == '\t' ? '\t' : ' ', stderr);
    fprintf(stderr, "^");
    for (int i = 1; i < len; ++i)
      fprintf(stderr, "~");
    fprintf(stderr, "\n");
  }
}

static void lex_error(const char *p, const char *fmt, ...) {
  fprintf(stderr, "%s(%d): ", lexer.filename, lexer.lineno);

  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fprintf(stderr, "\n");

  show_error_line(lexer.line->buf, p, 1);

  exit(1);
}

void parse_error(const Token *token, const char *fmt, ...) {
  if (fmt != NULL) {
    if (token == NULL)
      token = fetch_token();
    if (token != NULL && token->line != NULL) {
      fprintf(stderr, "%s(%d): ", token->line->filename, token->line->lineno);
    }

    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    fprintf(stderr, "\n");
  }

  if (token != NULL && token->line != NULL && token->begin != NULL)
    show_error_line(token->line->buf, token->begin, token->end - token->begin);

  exit(1);
}

static Token *alloc_token(enum TokenKind kind, const char *begin, const char *end) {
  Token *token = malloc(sizeof(*token));
  token->kind = kind;
  token->line = lexer.line;
  token->begin = begin;
  token->end = end;
  return token;
}

Token *alloc_ident(const Name *name, const char *begin, const char *end) {
  Token *tok = alloc_token(TK_IDENT, begin, end);
  tok->ident = name;
  return tok;
}

static void init_reserved_word_table(void) {
  table_init(&reserved_word_table);

  // Reserved words.
  for (int i = 0, n = (int)(sizeof(kReservedWords) / sizeof(*kReservedWords)); i < n; ++i) {
    const Name *key = alloc_name(kReservedWords[i].str, NULL, false);
    table_put(&reserved_word_table, key, (void*)(intptr_t)kReservedWords[i].kind);
  }

  // Multi-char operators.
  for (int i = 0, n = (int)(sizeof(kMultiOperators) / sizeof(*kMultiOperators)); i < n; ++i) {
    const Name *key = alloc_name(kMultiOperators[i].ident, NULL, false);
    table_put(&reserved_word_table, key, (void*)(intptr_t)kMultiOperators[i].kind);
  }
}

static enum TokenKind reserved_word(const Name *name) {
  enum TokenKind result = -1;
  void *ptr = table_get(&reserved_word_table, name);
  if (ptr != NULL)
    result = (enum TokenKind)ptr;
  return result;
}

static char backslash(char c) {
  switch (c) {
  case '0':  return '\0';
  case 'n':  return '\n';
  case 't':  return '\t';
  case 'r':  return '\r';
  case 'f':  return '\f';
  case 'v':  return '\v';
  default:   return c;
  }
}

void init_lexer(void) {
  init_reserved_word_table();
}

void set_source_file(FILE *fp, const char *filename) {
  lexer.fp = fp;
  lexer.filename = filename;
  lexer.line = NULL;
  lexer.p = "";
  lexer.idx = -1;
  lexer.lineno = 0;
}

void set_source_string(const char *line, const char *filename, int lineno) {
  Line *p = malloc(sizeof(*p));
  p->filename = lexer.filename;
  p->buf = line;
  p->lineno = lineno;

  lexer.fp = NULL;
  lexer.filename = filename;
  lexer.line = p;
  lexer.p = line;
  lexer.idx = -1;
  lexer.lineno = lineno;
}

const char *get_lex_p(void) {
  if (lexer.idx < 0)
    return lexer.p;
  else
    return lexer.fetched[lexer.idx]->begin;
}

static int scan_linemarker(const char *line, long *pnum, char **pfn, int *pflag) {
  const char *p = line;
  if (p[0] != '#' || p[1] != ' ')
    return 0;
  p += 2;

  int n = 0;
  const char *next = p;
  unsigned long num = strtoul(next, (char**)&next, 10);
  if (next > p) {
    ++n;
    *pnum = num;
    p = next;

    if (p[0] == ' ' && p[1] == '"') {
      p += 2;
      const char *q = strchr(p, '"');
      if (q != NULL) {
        ++n;
        *pfn = strndup_(p, q - p);
        p = q + 1;

        if (p[0] == ' ') {
          p += 1;
          next = p;
          int flag = strtol(next, (char**)&next, 10);
          if (next > p) {
            ++n;
            *pflag = flag;
          }
        }
      }
    }
  }
  return n;
}

static void read_next_line(void) {
  if (lexer.fp == NULL) {
    lexer.p = NULL;
    lexer.line = NULL;
    return;
  }

  char *line = NULL;
  size_t capa = 0;
  for (;;) {
    ssize_t len = getline(&line, &capa, lexer.fp);
    if (len == -1) {
      lexer.p = NULL;
      lexer.line = NULL;
      return;
    }
    for (;;) {
      if (len > 0 && line[len - 1] == '\n')
        line[--len] = '\0';
      if (len < 1 || line[len - 1] != '\\')
        break;
      // Continue line.
      line[--len] = '\0';
      ssize_t nextlen = getline_cat(&line, &capa, lexer.fp, len);
      if (nextlen != -1)
        len = nextlen;
    }

    if (line[0] != '#')
      break;

    // linemarkers: # linenum filename flags
    long num = -1;
    char *fn;
    int flag = -1;
    int n = scan_linemarker(line, &num, &fn, &flag);
    if (n >= 2) {
      lexer.lineno = num - 1;
      lexer.filename = fn;
    }
  }

  Line *p = malloc(sizeof(*p));
  p->filename = lexer.filename;
  p->buf = line;
  p->lineno = ++lexer.lineno;
  lexer.line = p;
  lexer.p = lexer.line->buf;
}

static const char *skip_block_comment(const char *p) {
  for (;;) {
    char c = *p++;
    if (c == '\0') {
      read_next_line();
      p = lexer.p;
      if (p == NULL)
        return NULL;
      continue;
    } else if (c == '*' && *p == '/')
      return p + 1;
  }
}

static const char *skip_line_comment(void) {
  read_next_line();
  return lexer.p;
}

static const char *skip_whitespace_or_comment(const char *p) {
  for (;;) {
    p = skip_whitespaces(p);
    switch (*p) {
    case '\0':
      read_next_line();
      p = lexer.p;
      if (p == NULL)
        return NULL;
      continue;
    case '/':
      switch (p[1]) {
      case '*':
        p = skip_block_comment(p + 2);
        if (p == NULL)
          lex_error(p, "Block comment not closed");
        continue;
      case '/':
        p = skip_line_comment();
        if (p == NULL)
          return NULL;
        continue;
      default:  break;
      }
      break;
    default:  break;
    }
    break;
  }
  return p;
}

#ifndef __NO_FLONUM
static Token *read_flonum(const char **pp) {
  const char *start = *pp;
  char *next;
  double val = strtod(start, &next);
  enum TokenKind tk = TK_DOUBLELIT;
  if (tolower(*next) == 'f') {
    tk = TK_FLOATLIT;
    ++next;
  }
  Token *tok = alloc_token(tk, start, next);
  tok->flonum = val;
  *pp = next;
  return tok;
}
#endif

static Token *read_num(const char **pp) {
  const char *start = *pp, *p = start;
  int base = 10;
  bool is_unsigned = false;
  if (*p == '0') {
    char c = tolower(p[1]);
    if (c == 'x') {
      base = 16;
      p += 2;
      c = tolower(*p);
      if (!isxdigit(c))
        lex_error(p, "Hexadecimal expected");
    } else if (isdigit(c)) {
      if (c >= '8')
        lex_error(p, "Octal expected");
      base = 8;
    }
  }
  const char *q = p;
  unsigned long val = strtoul(p, (char**)&p, base);
  if (p == q)
    lex_error(p, "Illegal literal");

#ifndef __NO_FLONUM
  if (*p == '.' || tolower(*p)== 'e') {
    if (base != 10)
      lex_error(p, "Illegal literal");
    return read_flonum(pp);
  }
#endif
  enum TokenKind tt = TK_INTLIT;
  if (tolower(*p) == 'u') {
    is_unsigned = true;
    ++p;
  }
  if (tolower(*p) == 'l') {
    tt = TK_LONGLIT;
    ++p;
    if (tolower(*p) == 'l') {
      tt = TK_LLONGLIT;
      ++p;
    }
  }
  Token *tok = alloc_token(tt + (is_unsigned ? (TK_UINTLIT - TK_INTLIT) : 0), start, p);
  tok->fixnum = val;
  *pp = p;
  return tok;
}

const char *read_ident(const char *p) {
  if (!isalpha(*p) && *p != '_')
    return NULL;

  const char *q;
  for (q = p + 1; ; ++q) {
    if (!(isalnum(*q) || *q == '_'))
      break;
  }
  return q;
}

static Token *read_char(const char **pp) {
  const char *p = *pp;
  const char *begin = p++;
  char c = *p;
  if (c == '\'')
    lex_error(p, "Empty character");
  if (c == '\\') {
    c = *(++p);
    if (c == '\0')
      lex_error(p, "Character not closed");
    c = backslash(c);
  }
  if (*(++p) != '\'')
    lex_error(p, "Character not closed");

  ++p;
  Token *tok = alloc_token(TK_CHARLIT, begin, p);
  tok->fixnum = c;
  *pp = p;
  return tok;
}

static Token *read_string(const char **pp) {
  const int ADD = 16;
  const char *p = *pp;
  const char *begin, *end;
  size_t capa = 16, size = 0;
  char *str = malloc(capa);
  for (;;) {
    begin = p++;  // Skip first '"'
    for (char c; (c = *p++) != '"'; ) {
      if (c == '\0')
        lex_error(p - 1, "String not closed");
      if (size + 1 >= capa) {
        capa += ADD;
        str = realloc(str, capa);
        if (str == NULL)
          lex_error(p, "Out of memory");
      }

      if (c == '\\') {
        c = *p++;
        if (c == '\0')
          lex_error(p, "String not closed");
        c = backslash(c);
      }
      assert(size < capa);
      str[size++] = c;
    }
    end = p;

    // Continue string literal when next character is '"'
    p = skip_whitespace_or_comment(p);
    if (p == NULL || *p != '"')
      break;
  }
  assert(size < capa);
  str[size++] = '\0';
  Token *tok = alloc_token(TK_STR, begin, end);
  tok->str.buf = str;
  tok->str.size = size;
  *pp = p;
  return tok;
}

static Token *get_op_token(const char **pp) {
  const char *p = *pp;
  char c = *p;
  if (c >= 0 /*&& c < sizeof(kSingleOperatorTypeMap)*/) {
    enum TokenKind single = kSingleOperatorTypeMap[(int)c];
    if (single != 0) {
      int n;
      for (n = 1; n < 3; ++n) {
        if (kSingleOperatorTypeMap[(int)p[n]] == 0)
          break;
      }

      for (int len = n; len > 1; --len) {
        const Name *op = alloc_name(p, p + len, false);
        enum TokenKind kind = reserved_word(op);
        if ((int)kind != -1) {
          const char *q = p + len;
          *pp = q;
          return alloc_token(kind, p, q);
        }
      }

      const char *q = p + 1;
      *pp = q;
      return alloc_token(single, p, q);
    }
  }
  return NULL;
}

static Token *get_token(void) {
  static Token kEofToken = {.kind = TK_EOF};

  const char *p = lexer.p;
  if (p == NULL)
    return &kEofToken;

  p = skip_whitespace_or_comment(p);
  if (p == NULL)
    return &kEofToken;

  Token *tok = NULL;
  const char *begin = p;
  const char *ident_end = read_ident(p);
  if (ident_end != NULL) {
    const Name *name = alloc_name(begin, ident_end, false);
    enum TokenKind word = reserved_word(name);
    if ((int)word != -1) {
      tok = alloc_token(word, begin, ident_end);
    } else {
      tok = alloc_ident(name, begin, ident_end);
    }
    p = ident_end;
  } else if (isdigit(*p)) {
    tok = read_num(&p);
#ifndef __NO_FLONUM
  } else if (*p == '.' && isdigit(p[1])) {
    tok = read_flonum(&p);
#endif
  } else if ((tok = get_op_token(&p)) != NULL) {
    // Ok.
  } else if (*p == '\'') {
    tok = read_char(&p);
  } else if (*p == '"') {
    tok = read_string(&p);
  } else {
    lex_error(p, "Unexpected character `%c'(%d)", *p, *p);
    return NULL;
  }

  assert(tok != NULL);
  lexer.p = p;
  return tok;
}

Token *fetch_token(void) {
  if (lexer.idx < 0) {
    lexer.idx = 0;
    lexer.fetched[0] = get_token();
  }
  return lexer.fetched[lexer.idx];
}

Token *match(enum TokenKind kind) {
  Token *tok = fetch_token();
  if (tok->kind != kind && (int)kind != -1)
    return NULL;
  if (tok->kind != TK_EOF)
    --lexer.idx;
  return tok;
}

Token *consume(enum TokenKind kind, const char *error) {
  Token *tok = match(kind);
  if (tok == NULL)
    parse_error(tok, error);
  return tok;
}

void unget_token(Token *token) {
  if (token->kind == TK_EOF)
    return;
  ++lexer.idx;
  assert(lexer.idx < MAX_LOOKAHEAD);
  lexer.fetched[lexer.idx] = token;
}
#include "parser.h"

#include <assert.h>
#include <inttypes.h>  // PRIdPTR
#include <stdbool.h>
#include <stdlib.h>  // malloc

#include "ast.h"
#include "lexer.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

const int LF_BREAK = 1 << 0;
const int LF_CONTINUE = 1 << 0;

Function *curfunc;
static int curloopflag;
Stmt *curswitch;

static Stmt *parse_stmt(void);

void fix_array_size(Type *type, Initializer *init) {
  assert(init != NULL);
  assert(type->kind == TY_ARRAY);

  bool is_str = (is_char_type(type->pa.ptrof) &&
                 init->kind == IK_SINGLE &&
                 init->single->kind == EX_STR);
  if (!is_str && init->kind != IK_MULTI) {
    parse_error(init->token, "Error initializer");
  }

  size_t arr_len = type->pa.length;
  if (arr_len == (size_t)-1) {
    if (is_str) {
      type->pa.length = init->single->str.size;
    } else {
      size_t index = 0;
      size_t max_index = 0;
      size_t i, len = init->multi->len;
      for (i = 0; i < len; ++i) {
        Initializer *init_elem = init->multi->data[i];
        if (init_elem->kind == IK_ARR) {
          assert(init_elem->arr.index->kind == EX_FIXNUM);
          index = init_elem->arr.index->fixnum;
        }
        ++index;
        if (max_index < index)
          max_index = index;
      }
      type->pa.length = max_index;
    }
  } else {
    assert(!is_str || init->single->kind == EX_STR);
    size_t init_len = is_str ? init->single->str.size : (size_t)init->multi->len;
    if (init_len > arr_len)
      parse_error(NULL, "Initializer more than array size");
  }
}

static Stmt *build_memcpy(Expr *dst, Expr *src, size_t size) {
  assert(!is_global_scope(curscope));
  const Type *charptr_type = ptrof(&tyChar);
  VarInfo *dstvar = scope_add(curscope, alloc_ident(alloc_label(), NULL, NULL), charptr_type, 0);
  VarInfo *srcvar = scope_add(curscope, alloc_ident(alloc_label(), NULL, NULL), charptr_type, 0);
  VarInfo *sizevar = scope_add(curscope, alloc_ident(alloc_label(), NULL, NULL), &tySize, 0);
  Expr *dstexpr = new_expr_variable(dstvar->name, dstvar->type, NULL, curscope);
  Expr *srcexpr = new_expr_variable(srcvar->name, srcvar->type, NULL, curscope);
  Expr *sizeexpr = new_expr_variable(sizevar->name, sizevar->type, NULL, curscope);

  Fixnum size_num_lit = size;
  Expr *size_num = new_expr_fixlit(&tySize, NULL, size_num_lit);

  Fixnum zero = 0;
  Expr *zeroexpr = new_expr_fixlit(&tySize, NULL, zero);

  Vector *stmts = new_vector();
  vec_push(stmts, new_stmt_expr(new_expr_bop(EX_ASSIGN, charptr_type, NULL, dstexpr, dst)));
  vec_push(stmts, new_stmt_expr(new_expr_bop(EX_ASSIGN, charptr_type, NULL, srcexpr, src)));
  vec_push(stmts, new_stmt_for(
      NULL,
      new_expr_bop(EX_ASSIGN, &tySize, NULL, sizeexpr, size_num),    // for (_size = size;
      new_expr_bop(EX_GT, &tyBool, NULL, sizeexpr, zeroexpr),        //      _size > 0;
      new_expr_unary(EX_PREDEC, &tySize, NULL, sizeexpr),            //      --_size)
      new_stmt_expr(                                                 //   *_dst++ = *_src++;
          new_expr_bop(EX_ASSIGN, &tyChar, NULL,
                       new_expr_unary(EX_DEREF, &tyChar, NULL,
                                      new_expr_unary(EX_POSTINC, charptr_type, NULL, dstexpr)),
                       new_expr_unary(EX_DEREF, &tyChar, NULL,
                                      new_expr_unary(EX_POSTINC, charptr_type, NULL, srcexpr))))));
  return new_stmt_block(NULL, stmts, NULL);
}

// Convert string literal to global char-array variable reference.
Initializer *convert_str_to_ptr_initializer(const Type *type, Initializer *init) {
  assert(type->kind == TY_ARRAY && is_char_type(type->pa.ptrof));
  VarInfo *varinfo = str_to_char_array(type, init);
  VarInfo *gvarinfo = is_global_scope(curscope) ? varinfo : varinfo->static_.gvar;
  Initializer *init2 = malloc(sizeof(*init2));
  init2->kind = IK_SINGLE;
  init2->single = new_expr_variable(gvarinfo->name, type, NULL, global_scope);
  init2->token = init->token;
  return init2;
}

static Stmt *init_char_array_by_string(Expr *dst, Initializer *src) {
  // Initialize char[] with string literal (char s[] = "foo";).
  assert(src->kind == IK_SINGLE);
  const Expr *str = src->single;
  assert(str->kind == EX_STR);
  assert(dst->type->kind == TY_ARRAY && is_char_type(dst->type->pa.ptrof));

  size_t size = str->str.size;
  size_t dstsize = dst->type->pa.length;
  if (dstsize == (size_t)-1) {
    ((Type*)dst->type)->pa.length = dstsize = size;
  } else {
    if (dstsize < size)
      parse_error(NULL, "Buffer is shorter than string: %d for \"%s\"", (int)dstsize, str);
  }

  const Type *strtype = dst->type;
  VarInfo *varinfo = str_to_char_array(strtype, src);
  Expr *var = new_expr_variable(varinfo->name, strtype, NULL, curscope);
  return build_memcpy(dst, var, size);
}

static int compare_desig_start(const void *a, const void *b) {
  const size_t *pa = *(size_t**)a;
  const size_t *pb = *(size_t**)b;
  intptr_t d = *pa - *pb;
  return d > 0 ? 1 : d < 0 ? -1 : 0;
}

static Initializer *flatten_array_initializer(Initializer *init) {
  // Check whether IK_DOT or IK_ARR exists.
  int i = 0, len = init->multi->len;
  for (; i < len; ++i) {
    Initializer *init_elem = init->multi->data[i];
    if (init_elem->kind == IK_DOT)
      parse_error(NULL, "dot initializer for array");
    if (init_elem->kind == IK_ARR)
      break;
  }
  if (i >= len)  // IK_ARR not exits.
    return init;

  // Enumerate designated initializer.
  Vector *ranges = new_vector();  // <(start, count)>
  size_t lastStartIndex = 0;
  size_t lastStart = 0;
  size_t index = i;
  for (; i <= len; ++i, ++index) {  // '+1' is for last range.
    Initializer *init_elem = NULL;
    if (i >= len || (init_elem = init->multi->data[i])->kind == IK_ARR) {
      if (i < len && init_elem->arr.index->kind != EX_FIXNUM)
        parse_error(NULL, "Constant value expected");
      if ((size_t)i > lastStartIndex) {
        size_t *range = malloc(sizeof(size_t) * 3);
        range[0] = lastStart;
        range[1] = lastStartIndex;
        range[2] = index - lastStart;
        vec_push(ranges, range);
      }
      if (i >= len)
        break;
      lastStart = index = init_elem->arr.index->fixnum;
      lastStartIndex = i;
    } else if (init_elem->kind == IK_DOT)
      parse_error(NULL, "dot initializer for array");
  }

  // Sort
  QSORT(ranges->data, ranges->len, sizeof(size_t*), compare_desig_start);

  // Reorder
  Vector *reordered = new_vector();
  index = 0;
  for (int i = 0; i < ranges->len; ++i) {
    size_t *p = ranges->data[i];
    size_t start = p[0];
    size_t index = p[1];
    size_t count = p[2];
    if (i > 0) {
      size_t *q = ranges->data[i - 1];
      if (start < q[0] + q[2])
        parse_error(NULL, "Initializer for array overlapped");
    }
    for (size_t j = 0; j < count; ++j) {
      Initializer *elem = init->multi->data[index + j];
      if (j == 0 && index != start && elem->kind != IK_ARR) {
        Initializer *arr = malloc(sizeof(*arr));
        arr->kind = IK_ARR;
        Fixnum n = start;
        arr->arr.index = new_expr_fixlit(&tyInt, NULL, n);
        arr->arr.value = elem;
        elem = arr;
      }
      vec_push(reordered, elem);
    }
  }

  Initializer *init2 = malloc(sizeof(*init2));
  init2->kind = IK_MULTI;
  init2->multi = reordered;
  return init2;
}

static Initializer *flatten_initializer(const Type *type, Initializer *init) {
  if (init == NULL)
    return NULL;

  switch (type->kind) {
  case TY_STRUCT:
    if (init->kind == IK_MULTI) {
      const StructInfo *sinfo = type->struct_.info;
      int n = sinfo->members->len;
      int m = init->multi->len;
      if (n <= 0) {
        if (m > 0)
          parse_error(init->token, "Initializer for empty struct");
        return init;
      }
      if (sinfo->is_union && m > 1)
        parse_error(((Initializer*)init->multi->data[1])->token, "Initializer for union more than 1");

      Initializer **values = malloc(sizeof(Initializer*) * n);
      for (int i = 0; i < n; ++i)
        values[i] = NULL;

      int index = 0;
      for (int i = 0; i < m; ++i) {
        Initializer *value = init->multi->data[i];
        if (value->kind == IK_ARR)
          parse_error(NULL, "indexed initializer for struct");

        if (value->kind == IK_DOT) {
          const Name *name = value->dot.name;
          index = var_find(sinfo->members, name);
          if (index >= 0) {
            value = value->dot.value;
          } else {
            Vector *stack = new_vector();
            if (search_from_anonymous(type, name, NULL, stack) == NULL)
              parse_error(value->token, "`%.*s' is not member of struct", name->bytes, name->chars);

            index = (intptr_t)stack->data[0];
            Vector *multi = new_vector();
            vec_push(multi, value);
            Initializer *init2 = malloc(sizeof(*init2));
            init2->kind = IK_MULTI;
            init2->multi = multi;
            value = init2;
          }
        }
        if (index >= n)
          parse_error(NULL, "Too many init values");

        // Allocate string literal for char* as a char array.
        if (value->kind == IK_SINGLE && value->single->kind == EX_STR) {
          const VarInfo *member = sinfo->members->data[index];
          if (member->type->kind == TY_PTR &&
              is_char_type(member->type->pa.ptrof)) {
            value = convert_str_to_ptr_initializer(value->single->type, value);
          }
        }

        values[index++] = value;
      }

      Initializer *flat = malloc(sizeof(*flat));
      flat->kind = IK_MULTI;
      Vector *v = malloc(sizeof(*v));
      v->len = v->capacity = n;
      v->data = (void**)values;
      flat->multi = v;

      return flat;
    }
    break;
  case TY_ARRAY:
    switch (init->kind) {
    case IK_MULTI:
      init = flatten_array_initializer(init);
      break;
    case IK_SINGLE:
      // Special handling for string (char[]), and accept length difference.
      if (init->single->type->kind == TY_ARRAY &&
          can_cast(type->pa.ptrof, init->single->type->pa.ptrof, is_zero(init->single), false))
        break;
      // Fallthrough
    default:
      parse_error(NULL, "Illegal initializer");
      break;
    }
  default:
    break;
  }
  return init;
}

static Initializer *check_global_initializer(const Type *type, Initializer *init) {
  if (init == NULL)
    return NULL;

  init = flatten_initializer(type, init);

  switch (type->kind) {
  case TY_FIXNUM:
    if (init->kind == IK_SINGLE) {
      switch (init->single->kind) {
      case EX_FIXNUM:
        return init;
      default:
        parse_error(init->single->token, "Constant expression expected");
        break;
      }
    }
    break;
#ifndef __NO_FLONUM
  case TY_FLONUM:
    if (init->kind == IK_SINGLE) {
      switch (init->single->kind) {
      case EX_FIXNUM:
        {
          Fixnum fixnum = init->single->fixnum;
          init->single = new_expr_flolit(type, init->single->token, fixnum);
        }
        // Fallthrough
      case EX_FLONUM:
        return init;
      default:
        parse_error(init->single->token, "Constant expression expected");
        break;
      }
    }
    break;
#endif
  case TY_PTR:
    {
      if (init->kind != IK_SINGLE)
        parse_error(NULL, "initializer type error");

      Expr *value = init->single;
      while (value->kind == EX_CAST) {
        value = value->unary.sub;
      }

      switch (value->kind) {
      case EX_REF:
        {
          value = value->unary.sub;
          if (value->kind != EX_VAR)
            parse_error(value->token, "pointer initializer must be variable");
          const Name *name = value->var.name;
          Scope *scope;
          VarInfo *varinfo = scope_find(value->var.scope, name, &scope);
          assert(varinfo != NULL);
          if (!is_global_scope(scope)) {
            if (!(varinfo->storage & VS_STATIC))
              parse_error(value->token, "Allowed global reference only");
            varinfo = varinfo->static_.gvar;
            assert(varinfo != NULL);
          }

          if (!same_type(type->pa.ptrof, varinfo->type))
            parse_error(value->token, "Illegal type");

          return init;
        }
      case EX_VAR:
        {
          Scope *scope;
          VarInfo *varinfo = scope_find(value->var.scope, value->var.name, &scope);
          assert(varinfo != NULL);
          if (!is_global_scope(scope)) {
            if (!(varinfo->storage & VS_STATIC))
              parse_error(value->token, "Allowed global reference only");
            varinfo = varinfo->static_.gvar;
            assert(varinfo != NULL);
          }

          if ((varinfo->type->kind != TY_ARRAY && varinfo->type->kind != TY_FUNC) ||
              !can_cast(type, varinfo->type, is_zero(value), false))
            parse_error(value->token, "Illegal type");

          return init;
        }
      case EX_FIXNUM:
        {
          Initializer *init2 = malloc(sizeof(*init2));
          init2->kind = IK_SINGLE;
          init2->single = value;
          return init2;
        }
      case EX_STR:
        {
          if (!is_char_type(type->pa.ptrof))
            parse_error(value->token, "Illegal type");

          // Create string and point to it.
          Type* strtype = arrayof(type->pa.ptrof, value->str.size);
          return convert_str_to_ptr_initializer(strtype, init);
        }
      default:
        break;
      }
      parse_error(value->token, "initializer type error: kind=%d", value->kind);
    }
    break;
  case TY_ARRAY:
    switch (init->kind) {
    case IK_MULTI:
      {
        const Type *elemtype = type->pa.ptrof;
        Vector *multi = init->multi;
        for (int i = 0, len = multi->len; i < len; ++i) {
          Initializer *eleminit = multi->data[i];
          multi->data[i] = check_global_initializer(elemtype, eleminit);
        }
      }
      break;
    case IK_SINGLE:
      if (is_char_type(type->pa.ptrof) && init->single->kind == EX_STR) {
        assert(type->pa.length != (size_t)-1);
        if (type->pa.length < init->single->str.size) {
          parse_error(init->single->token, "Array size shorter than initializer");
        }
        break;
      }
      // Fallthrough
    case IK_DOT:
    default:
      parse_error(NULL, "Illegal initializer");
      break;
    }
    break;
  case TY_STRUCT:
    {
      assert(init->kind == IK_MULTI);
      const StructInfo *sinfo = type->struct_.info;
      for (int i = 0, n = sinfo->members->len; i < n; ++i) {
        const VarInfo* member = sinfo->members->data[i];
        Initializer *init_elem = init->multi->data[i];
        if (init_elem != NULL)
          init->multi->data[i] = check_global_initializer(member->type, init_elem);
      }
    }
    break;
  default:
    parse_error(NULL, "Global initial value for type %d not implemented (yet)\n", type->kind);
    break;
  }
  return init;
}

Vector *assign_initial_value(Expr *expr, Initializer *init, Vector *inits) {
  if (init == NULL)
    return inits;

  if (inits == NULL)
    inits = new_vector();

  Initializer *org_init = init;
  init = flatten_initializer(expr->type, init);

  switch (expr->type->kind) {
  case TY_ARRAY:
    switch (init->kind) {
    case IK_MULTI:
      {
        size_t arr_len = expr->type->pa.length;
        assert(arr_len != (size_t)-1);
        if ((size_t)init->multi->len > arr_len)
          parse_error(init->token, "Initializer more than array size");

        assert(!is_global_scope(curscope));
        const Type *ptr_type = array_to_ptr(expr->type);
        VarInfo *ptr_varinfo = scope_add(curscope, alloc_ident(alloc_label(), NULL, NULL), ptr_type, 0);
        Expr *ptr_var = new_expr_variable(ptr_varinfo->name, ptr_type, NULL, curscope);
        vec_push(inits, new_stmt_expr(new_expr_bop(EX_ASSIGN, ptr_type, NULL, ptr_var, expr)));

        size_t len = init->multi->len;
        size_t prev_index = 0, index = 0;
        for (size_t i = 0; i < len; ++i) {
          Initializer *init_elem = init->multi->data[i];
          if (init_elem->kind == IK_ARR) {
            Expr *ind = init_elem->arr.index;
            if (ind->kind != EX_FIXNUM)
              parse_error(init_elem->token, "Number required");
            index = ind->fixnum;
            init_elem = init_elem->arr.value;
          }

          size_t add = index - prev_index;
          if (add > 0) {
            Fixnum n = add;
            vec_push(inits, new_stmt_expr(
                new_expr_unary(EX_MODIFY, ptr_type, NULL,
                               new_expr_bop(EX_PTRADD, ptr_type, NULL, ptr_var,
                                            new_expr_fixlit(&tySize, NULL, n)))));
          }

          assign_initial_value(new_expr_deref(NULL, ptr_var), init_elem, inits);
          prev_index = index++;
        }
      }
      break;
    case IK_SINGLE:
      // Special handling for string (char[]).
      if (is_char_type(expr->type->pa.ptrof) &&
          init->single->kind == EX_STR) {
        vec_push(inits, init_char_array_by_string(expr, init));
        break;
      }
      // Fallthrough
    default:
      parse_error(init->token, "Error initializer");
      break;
    }
    break;
  case TY_STRUCT:
    {
      if (init->kind != IK_MULTI) {
        vec_push(inits,
                 new_stmt_expr(new_expr_bop(EX_ASSIGN, expr->type, init->token, expr,
                                            init->single)));
        break;
      }

      const StructInfo *sinfo = expr->type->struct_.info;
      if (!sinfo->is_union) {
        for (int i = 0, n = sinfo->members->len; i < n; ++i) {
          const VarInfo* member = sinfo->members->data[i];
          Expr *mem = new_expr_member(NULL, member->type, expr, NULL, i);
          Initializer *init_elem = init->multi->data[i];
          if (init_elem != NULL)
            assign_initial_value(mem, init_elem, inits);
        }
      } else {
        int n = sinfo->members->len;
        int m = init->multi->len;
        if (n <= 0 && m > 0)
          parse_error(init->token, "Initializer for empty union");
        if (org_init->multi->len > 1)
          parse_error(init->token, "More than one initializer for union");

        for (int i = 0; i < n; ++i) {
          Initializer *init_elem = init->multi->data[i];
          if (init_elem == NULL)
            continue;
          const VarInfo* member = sinfo->members->data[i];
          Expr *mem = new_expr_member(NULL, member->type, expr, NULL, i);
          assign_initial_value(mem, init_elem, inits);
          break;
        }
      }
    }
    break;
  default:
    switch (init->kind) {
    case IK_MULTI:
      if (init->multi->len != 1 || ((Initializer*)init->multi->data[0])->kind != IK_SINGLE) {
        parse_error(init->token, "Error initializer");
        break;
      }
      init = init->multi->data[0];
      // Fallthrough
    case IK_SINGLE:
      {
        Expr *value = str_to_char_array_var(init->single);
        vec_push(inits,
                 new_stmt_expr(new_expr_bop(EX_ASSIGN, expr->type, init->token, expr,
                                            make_cast(expr->type, init->token, value, false))));
      }
      break;
    default:
      parse_error(init->token, "Error initializer");
      break;
    }
    break;
  }

  return inits;
}

Vector *construct_initializing_stmts(Vector *decls) {
  Vector *inits = NULL;
  for (int i = 0; i < decls->len; ++i) {
    VarDecl *decl = decls->data[i];
    if (decl->storage & VS_STATIC)
      continue;
    Expr *var = new_expr_variable(decl->ident->ident, decl->type, NULL, curscope);
    inits = assign_initial_value(var, decl->init, inits);
  }
  return inits;
}

static Initializer *check_vardecl(const Type *type, const Token *ident, int storage, Initializer *init) {
  if (type->kind == TY_ARRAY && init != NULL)
    fix_array_size((Type*)type, init);
  if (type->kind == TY_STRUCT && !(storage & VS_EXTERN))
    ensure_struct((Type*)type, NULL, curscope);

  if (curfunc != NULL) {
    VarInfo *varinfo = scope_find(curscope, ident->ident, NULL);

    // TODO: Check `init` can be cast to `type`.
    if (storage & VS_STATIC) {
      VarInfo *gvarinfo = varinfo->static_.gvar;
      assert(gvarinfo != NULL);
      gvarinfo->global.init = init = check_global_initializer(type, init);
      // static variable initializer is handled in codegen, same as global variable.
    }
  } else {
    //intptr_t eval;
    //if (find_enum_value(ident->ident, &eval))
    //  parse_error(ident, "`%.*s' is already defined", ident->ident->bytes, ident->ident->chars);
    if (storage & VS_EXTERN && init != NULL)
      parse_error(init->token, "extern with initializer");
    // Toplevel
    VarInfo *varinfo = scope_find(global_scope, ident->ident, NULL);
    assert(varinfo != NULL);
    varinfo->global.init = init = check_global_initializer(type, init);
  }
  return init;
}

static void add_func_label(const Token *label) {
  assert(curfunc != NULL);
  Table *table = curfunc->label_table;
  if (table == NULL) {
    curfunc->label_table = table = malloc(sizeof(*table));
    table_init(table);
  }
  if (!table_put(table, label->ident, (void*)-1))  // Put dummy value.
    parse_error(label, "Label `%.*s' already defined", label->ident->bytes, label->ident->chars);
}

static void add_func_goto(Stmt *stmt) {
  assert(curfunc != NULL);
  if (curfunc->gotos == NULL)
    curfunc->gotos = new_vector();
  vec_push(curfunc->gotos, stmt);
}

// Scope

static Scope *enter_scope(Function *func, Vector *vars) {
  Scope *scope = new_scope(curscope, vars);
  curscope = scope;
  vec_push(func->scopes, scope);
  return scope;
}

static void exit_scope(void) {
  assert(!is_global_scope(curscope));
  curscope = curscope->parent;
}

// Initializer

Initializer *parse_initializer(void) {
  Initializer *result = malloc(sizeof(*result));
  const Token *lblace_tok;
  if ((lblace_tok = match(TK_LBRACE)) != NULL) {
    Vector *multi = new_vector();
    if (!match(TK_RBRACE)) {
      for (;;) {
        Initializer *init;
        const Token *tok;
        if (match(TK_DOT)) {  // .member=value
          Token *ident = consume(TK_IDENT, "`ident' expected for dotted initializer");
          consume(TK_ASSIGN, "`=' expected for dotted initializer");
          Initializer *value = parse_initializer();
          init = malloc(sizeof(*init));
          init->kind = IK_DOT;
          init->token = ident;
          init->dot.name = ident->ident;
          init->dot.value = value;
        } else if ((tok = match(TK_LBRACKET)) != NULL) {
          Expr *index = parse_const();
          consume(TK_RBRACKET, "`]' expected");
          match(TK_ASSIGN);  // both accepted: `[1] = 2`, and `[1] 2`
          Initializer *value = parse_initializer();
          init = malloc(sizeof(*init));
          init->kind = IK_ARR;
          init->token = tok;
          init->arr.index = index;
          init->arr.value = value;
        } else {
          init = parse_initializer();
        }
        vec_push(multi, init);

        if (match(TK_COMMA)) {
          if (match(TK_RBRACE))
            break;
        } else {
          consume(TK_RBRACE, "`}' or `,' expected");
          break;
        }
      }
    }
    result->kind = IK_MULTI;
    result->token = lblace_tok;
    result->multi = multi;
  } else {
    result->kind = IK_SINGLE;
    result->single = parse_assign();
    result->token = result->single->token;
  }
  return result;
}

static bool def_type(const Type *type, Token *ident) {
  const Name *name = ident->ident;
  Scope *scope;
  const Type *conflict = find_typedef(curscope, name, &scope);
  if (conflict != NULL && scope == curscope) {
    if (!same_type(type, conflict))
      parse_error(ident, "Conflict typedef");
  } else {
    conflict = NULL;
  }

  if (conflict == NULL || (type->kind == TY_STRUCT && type->struct_.info != NULL)) {
    add_typedef(curscope, name, type);
    return true;
  } else {
    return false;
  }
}

static Vector *parse_vardecl_cont(const Type *rawType, Type *type, int storage, Token *ident) {
  Vector *decls = NULL;
  bool first = true;
  do {
    int tmp_storage = storage;
    if (!first) {
      if (!parse_var_def(&rawType, (const Type**)&type, &tmp_storage, &ident) || ident == NULL) {
        parse_error(NULL, "`ident' expected");
        return NULL;
      }
    }
    first = false;

    Initializer *init = NULL;
    if (match(TK_LPAR)) {  // Function prototype.
      bool vaargs;
      Vector *params = parse_funparams(&vaargs);
      Vector *param_types = extract_varinfo_types(params);
      type = new_func_type(type, params, param_types, vaargs);
    } else {
      not_void(type, NULL);
    }

    if (type->kind == TY_FUNC /* && !is_global_scope(curscope)*/) {
      // Must be prototype.
      tmp_storage |= VS_EXTERN;
    }

    assert(!is_global_scope(curscope));
    scope_add(curscope, ident, type, tmp_storage);

    if (!(storage & VS_TYPEDEF) && type->kind != TY_FUNC && match(TK_ASSIGN)) {
      init = parse_initializer();
    }

    if (tmp_storage & VS_TYPEDEF) {
      def_type(type, ident);
    } else {
      init = check_vardecl(type, ident, tmp_storage, init);
      VarDecl *decl = new_vardecl(type, ident, init, tmp_storage);
      if (decls == NULL)
        decls = new_vector();
      vec_push(decls, decl);
    }
  } while (match(TK_COMMA));
  return decls;
}

static bool parse_vardecl(Stmt **pstmt) {
  const Type *rawType = NULL;
  Type *type;
  int storage;
  Token *ident;
  if (!parse_var_def(&rawType, (const Type**)&type, &storage, &ident))
    return false;

  *pstmt = NULL;
  if (ident == NULL) {
    if ((type->kind == TY_STRUCT ||
         (type->kind == TY_FIXNUM && type->fixnum.kind == FX_ENUM)) &&
         match(TK_SEMICOL)) {
      // Just struct/union or enum definition.
    } else {
      parse_error(NULL, "Ident expected");
    }
  } else {
    Vector *decls = parse_vardecl_cont(rawType, type, storage, ident);
    consume(TK_SEMICOL, "`;' expected 2");
    if (decls != NULL) {
      Vector *inits = !is_global_scope(curscope) ? construct_initializing_stmts(decls) : NULL;
      *pstmt = new_stmt_vardecl(decls, inits);
    }
  }
  return true;
}

static Stmt *parse_if(const Token *tok) {
  consume(TK_LPAR, "`(' expected");
  Expr *cond = make_cond(parse_expr());
  consume(TK_RPAR, "`)' expected");
  Stmt *tblock = parse_stmt();
  Stmt *fblock = NULL;
  if (match(TK_ELSE)) {
    fblock = parse_stmt();
  }
  return new_stmt_if(tok, cond, tblock, fblock);
}

static Stmt *parse_switch(const Token *tok) {
  consume(TK_LPAR, "`(' expected");
  Expr *value = parse_expr();
  not_void(value->type, value->token);
  consume(TK_RPAR, "`)' expected");

  Stmt *swtch = new_stmt_switch(tok, value);
  Stmt *save_switch = curswitch;
  int save_flag = curloopflag;
  curloopflag |= LF_BREAK;
  curswitch = swtch;

  swtch->switch_.body = parse_stmt();

  curloopflag = save_flag;
  curswitch = save_switch;

  return swtch;
}

static Stmt *parse_case(const Token *tok) {
  Expr *value = parse_const();
  consume(TK_COLON, "`:' expected");
  assert(value->kind == EX_FIXNUM);

  Stmt *stmt = new_stmt_case(tok, value);
  if (curswitch == NULL) {
    parse_error(tok, "`case' cannot use outside of `switch`");
  } else {
    // Check duplication.
    Fixnum v = value->fixnum;
    Vector *cases = curswitch->switch_.cases;
    for (int i = 0, len = cases->len; i < len; ++i) {
      Stmt *c = cases->data[i];
      if (c->case_.value == NULL)
        continue;
      if (c->case_.value->fixnum == v)
        parse_error(tok, "Case value `%" PRIdPTR "' already defined", v);
    }
    vec_push(cases, stmt);
  }
  return stmt;
}

static Stmt *parse_default(const Token *tok) {
  consume(TK_COLON, "`:' expected");

  Stmt *stmt = new_stmt_default(tok);
  if (curswitch == NULL) {
    parse_error(tok, "`default' cannot use outside of `switch'");
  } else if (curswitch->switch_.default_ != NULL) {
    parse_error(tok, "`default' already defined in `switch'");
  } else {
    curswitch->switch_.default_ = stmt;
    vec_push(curswitch->switch_.cases, stmt);
  }
  return stmt;
}

static Stmt *parse_while(const Token *tok) {
  consume(TK_LPAR, "`(' expected");
  Expr *cond = make_cond(parse_expr());
  consume(TK_RPAR, "`)' expected");

  int save_flag = curloopflag;
  curloopflag |= LF_BREAK | LF_CONTINUE;

  Stmt *body = parse_stmt();

  curloopflag = save_flag;

  return new_stmt_while(tok, cond, body);
}

static Stmt *parse_do_while(void) {
  int save_flag = curloopflag;
  curloopflag |= LF_BREAK | LF_CONTINUE;

  Stmt *body = parse_stmt();

  curloopflag = save_flag;

  const Token *tok = consume(TK_WHILE, "`while' expected");
  consume(TK_LPAR, "`(' expected");
  Expr *cond = make_cond(parse_expr());
  consume(TK_RPAR, "`)' expected");
  consume(TK_SEMICOL, "`;' expected");
  return new_stmt_do_while(body, tok, cond);
}

static Stmt *parse_for(const Token *tok) {
  consume(TK_LPAR, "`(' expected");
  Expr *pre = NULL;
  Vector *decls = NULL;
  Scope *scope = NULL;
  if (!match(TK_SEMICOL)) {
    const Type *rawType = NULL;
    Type *type;
    int storage;
    Token *ident;
    if (parse_var_def(&rawType, (const Type**)&type, &storage, &ident)) {
      if (ident == NULL)
        parse_error(NULL, "Ident expected");
      scope = enter_scope(curfunc, NULL);
      decls = parse_vardecl_cont(rawType, type, storage, ident);
      consume(TK_SEMICOL, "`;' expected");
    } else {
      pre = parse_expr();
      consume(TK_SEMICOL, "`;' expected");
    }
  }

  Expr *cond = NULL;
  Expr *post = NULL;
  Stmt *body = NULL;
  if (!match(TK_SEMICOL)) {
    cond = make_cond(parse_expr());
    consume(TK_SEMICOL, "`;' expected");
  }
  if (!match(TK_RPAR)) {
    post = parse_expr();
    consume(TK_RPAR, "`)' expected");
  }

  int save_flag = curloopflag;
  curloopflag |= LF_BREAK | LF_CONTINUE;

  body = parse_stmt();

  Vector *stmts = new_vector();
  if (decls != NULL) {
    Vector *inits = construct_initializing_stmts(decls);
    vec_push(stmts, new_stmt_vardecl(decls, inits));
  }

  curloopflag = save_flag;

  if (scope != NULL)
    exit_scope();

  Stmt *stmt = new_stmt_for(tok, pre, cond, post, body);
  vec_push(stmts, stmt);
  return new_stmt_block(tok, stmts, scope);
}

static Stmt *parse_break_continue(enum StmtKind kind, const Token *tok) {
  consume(TK_SEMICOL, "`;' expected");
  if ((curloopflag & LF_BREAK) == 0) {
    const char *err;
    if (kind == ST_BREAK)
      err = "`break' cannot be used outside of loop";
    else
      err = "`continue' cannot be used outside of loop";
    parse_error(tok, err);
  }
  return new_stmt(kind, tok);
}

static Stmt *parse_goto(const Token *tok) {
  Token *label = consume(TK_IDENT, "label for goto expected");
  consume(TK_SEMICOL, "`;' expected");

  Stmt *stmt = new_stmt_goto(tok, label);
  add_func_goto(stmt);
  return stmt;
}

static Stmt *parse_label(const Token *label) {
  Stmt *stmt = new_stmt_label(label, parse_stmt());
  add_func_label(label);
  return stmt;
}

static Stmt *parse_return(const Token *tok) {
  Expr *val = NULL;
  if (!match(TK_SEMICOL)) {
    val = parse_expr();
    consume(TK_SEMICOL, "`;' expected");
    val = str_to_char_array_var(val);
  }

  assert(curfunc != NULL);
  const Type *rettype = curfunc->type->func.ret;
  if (val == NULL) {
    if (rettype->kind != TY_VOID)
      parse_error(tok, "`return' required a value");
  } else {
    if (rettype->kind == TY_VOID)
      parse_error(val->token, "void function `return' a value");

    val = make_cast(rettype, val->token, val, false);
  }

  return new_stmt_return(tok, val);
}

static Stmt *parse_asm(const Token *tok) {
  consume(TK_LPAR, "`(' expected");

  Token *token;
  Vector *args = parse_args(&token);

  if (args == NULL || args->len != 1 || ((Expr*)args->data[0])->kind != EX_STR)
    parse_error(token, "`__asm' expected one string");

  return new_stmt_asm(tok, args->data[0]);
}

// Multiple stmt-s, also accept `case` and `default`.
static Vector *parse_stmts(void) {
  Vector *stmts = new_vector();
  for (;;) {
    if (match(TK_RBRACE))
      return stmts;

    Stmt *stmt;
    Token *tok;
    if (parse_vardecl(&stmt))
      ;
    else if ((tok = match(TK_CASE)) != NULL)
      stmt = parse_case(tok);
    else if ((tok = match(TK_DEFAULT)) != NULL)
      stmt = parse_default(tok);
    else
      stmt = parse_stmt();

    if (stmt == NULL)
      continue;
    vec_push(stmts, stmt);
  }
}

static Stmt *parse_block(const Token *tok) {
  Scope *scope = enter_scope(curfunc, NULL);
  Vector *stmts = parse_stmts();
  Stmt *stmt = new_stmt_block(tok, stmts, scope);
  exit_scope();
  return stmt;
}

static Stmt *parse_stmt(void) {
  Token *tok = match(-1);
  switch (tok->kind){
  case TK_IDENT:
    if (match(TK_COLON))
      return parse_label(tok);
    break;
  case TK_SEMICOL:
    return NULL;
  case TK_LBRACE:
    return parse_block(tok);
  case TK_IF:
    return parse_if(tok);
  case TK_SWITCH:
    return parse_switch(tok);
  case TK_WHILE:
    return parse_while(tok);
  case TK_DO:
    return parse_do_while();
  case TK_FOR:
    return parse_for(tok);
  case TK_BREAK: case TK_CONTINUE:
    return parse_break_continue(tok->kind == TK_BREAK ? ST_BREAK : ST_CONTINUE, tok);
  case TK_GOTO:
    return parse_goto(tok);
  case TK_RETURN:
    return parse_return(tok);
  case TK_ASM:
    return parse_asm(tok);
  default:
    break;
  }

  unget_token(tok);

  // expression statement.
  Expr *val = parse_expr();
  consume(TK_SEMICOL, "`;' expected");
  return new_stmt_expr(str_to_char_array_var(val));
}

static Declaration *parse_defun(const Type *functype, int storage, Token *ident) {
  assert(functype->kind == TY_FUNC);
  Function *func = new_func(functype, ident->ident);

  VarInfo *varinfo = scope_find(global_scope, func->name, NULL);
  if (varinfo == NULL) {
    varinfo = scope_add(global_scope, ident, functype, storage);
  } else {
    if (varinfo->type->kind != TY_FUNC)
      parse_error(ident, "Definition conflict: `%s'");
    if (varinfo->global.init != NULL)
      parse_error(ident, "`%.*s' function already defined", func->name->bytes,
                  func->name->chars);
    if (varinfo->type->func.params == NULL &&  // Old-style prototype definition.
        functype->func.params != NULL) {
      varinfo->type = functype;  // Overwrite with actual function type.
    }
  }

  if (match(TK_SEMICOL)) {
    // Prototype declaration.
  } else {
    consume(TK_LBRACE, "`;' or `{' expected");

    assert(curfunc == NULL);
    assert(is_global_scope(curscope));
    curfunc = func;
    Vector *top_vars = NULL;
    const Vector *params = func->type->func.params;
    if (params != NULL) {
      top_vars = new_vector();
      for (int i = 0; i < params->len; ++i)
        vec_push(top_vars, params->data[i]);
    }
    func->scopes = new_vector();
    enter_scope(func, top_vars);  // Scope for parameters.
    func->stmts = parse_stmts();
    exit_scope();
    assert(is_global_scope(curscope));

    // Check goto labels.
    if (func->gotos != NULL) {
      Vector *gotos = func->gotos;
      Table *label_table = func->label_table;
      for (int i = 0; i < gotos->len; ++i) {
        Stmt *stmt = gotos->data[i];
        void *bb;
        if (label_table == NULL || !table_try_get(label_table, stmt->goto_.label->ident, &bb)) {
          const Name *name = stmt->goto_.label->ident;
          parse_error(stmt->goto_.label, "`%.*s' not found", name->bytes, name->chars);
        }
      }
    }

    curfunc = NULL;
  }
  return new_decl_defun(func);
}

static Declaration *parse_global_var_decl(
    const Type *rawtype, int storage, const Type *type, Token *ident
) {
  Vector *decls = NULL;
  for (;;) {
    if (!(type->kind == TY_PTR && type->pa.ptrof->kind == TY_FUNC) &&
        type->kind != TY_VOID)
      type = parse_type_suffix(type);

    if (storage & VS_TYPEDEF) {
      def_type(type, ident);
    } else {
      if (type->kind == TY_VOID)
        parse_error(ident, "`void' not allowed");

      VarInfo *varinfo = scope_add(global_scope, ident, type, storage);

      Initializer *init = NULL;
      if (match(TK_ASSIGN) != NULL)
        init = parse_initializer();
      varinfo->global.init = init;

      init = check_vardecl(type, ident, storage, init);
      VarDecl *decl = new_vardecl(type, ident, init, storage);
      if (decls == NULL)
        decls = new_vector();
      vec_push(decls, decl);
    }

    if (!match(TK_COMMA))
      break;

    // Next declaration.
    type = parse_type_modifier(rawtype);
    ident = consume(TK_IDENT, "`ident' expected");
  }

  consume(TK_SEMICOL, "`;' or `,' expected");

  if (decls == NULL)
    return NULL;
  return new_decl_vardecl(decls);
}

static Declaration *parse_declaration(void) {
  const Type *rawtype = NULL, *type;
  int storage;
  Token *ident;
  if (parse_var_def(&rawtype, &type, &storage, &ident)) {
    if (ident == NULL) {
      if ((type->kind == TY_STRUCT ||
           (type->kind == TY_FIXNUM && type->fixnum.kind == FX_ENUM)) &&
          match(TK_SEMICOL)) {
        // Just struct/union or enum definition.
      } else {
        parse_error(NULL, "Ident expected");
      }
      return NULL;
    }

    if (type->kind == TY_FUNC)
      return parse_defun(type, storage, ident);

    return parse_global_var_decl(rawtype, storage, type, ident);
  }
  parse_error(NULL, "Unexpected token");
  return NULL;
}

void parse(Vector *decls) {
  curscope = global_scope;

  while (!match(TK_EOF)) {
    Declaration *decl = parse_declaration();
    if (decl != NULL)
      vec_push(decls, decl);
  }
}
#include "parser.h"

#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "ast.h"
#include "lexer.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

Scope *curscope;
Vector *toplevel;

static Table builtin_expr_ident_table;

static StructInfo *parse_struct(bool is_union);
static Expr *parse_cast_expr(void);
static Expr *parse_unary(void);

void add_builtin_expr_ident(const char *str, Expr *(*proc)(const Token*)) {
  const Name *name = alloc_name(str, NULL, false);
  table_put(&builtin_expr_ident_table, name, proc);
}

static void define_enum_member(const Type *type, const Token *ident, int value) {
  VarInfo *varinfo = scope_add(curscope, ident, type, VS_ENUM_MEMBER);
  varinfo->enum_member.value = value;
}

void not_void(const Type *type, const Token *token) {
  if (type->kind == TY_VOID)
    parse_error(token, "`void' not allowed");
}

void not_const(const Type *type, const Token *token) {
  if (type->qualifier & TQ_CONST)
    parse_error(token, "Cannot modify `const'");
}

// Returns created global variable info.
VarInfo *str_to_char_array(const Type *type, Initializer *init) {
  assert(type->kind == TY_ARRAY && is_char_type(type->pa.ptrof));
  const Token *ident = alloc_ident(alloc_label(), NULL, NULL);
  VarInfo *varinfo = scope_add(curscope, ident, type, VS_STATIC);
  if (is_global_scope(curscope)) {
    Vector *decls = new_vector();
    vec_push(decls, new_vardecl(varinfo->type, ident, init, varinfo->storage));
    vec_push(toplevel, new_decl_vardecl(decls));
    varinfo->global.init = init;
  } else {
    varinfo->static_.gvar->global.init = init;
  }
  return varinfo;
}

Expr *str_to_char_array_var(Expr *str) {
  if (str->kind != EX_STR)
    return str;
  const Type* type = str->type;
  Initializer *init = malloc(sizeof(*init));
  init->kind = IK_SINGLE;
  init->single = str;
  init->token = str->token;

  VarInfo *varinfo = str_to_char_array(type, init);
  return new_expr_variable(varinfo->name, type, str->token, curscope);
}

bool check_cast(const Type *dst, const Type *src, bool zero, bool is_explicit, const Token *token) {
  bool ok = can_cast(dst, src, zero, is_explicit);
  if (!ok || dst->kind == TY_ARRAY) {
    if (token == NULL)
      token = fetch_token();
    fprintf(stderr, "%s(%d): ", token->line->filename, token->line->lineno);

    fprintf(stderr, "Cannot convert value from type `");
    print_type(stderr, src);
    fprintf(stderr, "' to %s`", dst->kind == TY_ARRAY ? "array type " : "");
    print_type(stderr, dst);
    fprintf(stderr, "'\n");
    parse_error(token, NULL);
    return false;
  }
  return true;
}

Expr *make_cast(const Type *type, const Token *token, Expr *sub, bool is_explicit) {
  if (type->kind == TY_VOID || sub->type->kind == TY_VOID)
    parse_error(NULL, "cannot use `void' as a value");

  if (same_type(type, sub->type))
    return sub;
  if (is_const(sub) && is_number(sub->type) && is_number(type)) {
#ifndef __NO_FLONUM
    switch (sub->type->kind) {
    case TY_FLONUM:
      if (type->kind == TY_FIXNUM) {
        Fixnum fixnum = sub->flonum;
        return new_expr_fixlit(type, sub->token, fixnum);
      }
      sub->type = type;
      return sub;
    case TY_FIXNUM:
      if (type->kind == TY_FLONUM) {
        double flonum = sub->fixnum;
        return new_expr_flolit(type, sub->token, flonum);
      }
      break;
    default:
      break;
    }
#endif

    {
      int bytes = type_size(type);
      int src_bytes = type_size(sub->type);
      if (bytes < (int)type_size(&tySize) &&
          (bytes < src_bytes ||
           (bytes == src_bytes &&
            type->fixnum.is_unsigned != sub->type->fixnum.is_unsigned))) {
        int bits = bytes * CHAR_BIT;
        uintptr_t mask = (-1UL) << bits;
        Fixnum value = sub->fixnum;
        if (!type->fixnum.is_unsigned &&  // signed
            (value & (1UL << (bits - 1))))  // negative
          value |= mask;
        else
          value &= ~mask;
        sub->fixnum = value;
      }
    }
    sub->type = type;
    return sub;
  }

  check_cast(type, sub->type, is_zero(sub), is_explicit, token);

  return new_expr_cast(type, token, sub);
}

static bool cast_numbers(Expr **pLhs, Expr **pRhs, bool keep_left) {
  Expr *lhs = *pLhs;
  Expr *rhs = *pRhs;
  const Type *ltype = lhs->type;
  const Type *rtype = rhs->type;
  assert(ltype != NULL);
  assert(rtype != NULL);
  if (!is_number(ltype)) {
    parse_error(lhs->token, "number type expected");
    return false;
  }
  if (!is_number(rtype)) {
    parse_error(rhs->token, "number type expected");
    return false;
  }

#ifndef __NO_FLONUM
  {
    bool lflo = is_flonum(ltype), rflo = is_flonum(rtype);
    if (lflo || rflo) {
      int dir = !lflo ? 1 : !rflo ? -1 : (int)rtype->flonum.kind - (int)ltype->flonum.kind;
      if (dir < 0 || keep_left)
        *pRhs = make_cast(ltype, rhs->token, rhs, false);
      else if (dir > 0)
        *pLhs = make_cast(rtype, lhs->token, lhs, false);
      return true;
    }
  }
#endif
  enum FixnumKind lkind = ltype->fixnum.kind;
  enum FixnumKind rkind = rtype->fixnum.kind;
  if (ltype->fixnum.kind == FX_ENUM) {
    ltype = &tyInt;
    lkind = FX_INT;
  }
  if (rtype->fixnum.kind == FX_ENUM) {
    rtype = &tyInt;
    rkind = FX_INT;
  }

  int l = (lkind << 1) | (ltype->fixnum.is_unsigned ? 1 : 0);
  int r = (rkind << 1) | (rtype->fixnum.is_unsigned ? 1 : 0);
  if (keep_left || l > r)
    *pRhs = make_cast(ltype, rhs->token, rhs, false);
  else if (l < r)
    *pLhs = make_cast(rtype, lhs->token, lhs, false);
  return true;
}

static void check_lval(const Token *tok, Expr *expr, const char *error) {
  switch (expr->kind) {
  case EX_VAR:
  case EX_DEREF:
  case EX_MEMBER:
    break;
  default:
    parse_error(tok, error);
    break;
  }
}

static void check_referable(const Token *tok, Expr *expr, const char *error) {
  if (expr->kind == EX_COMPLIT)
    return;
  check_lval(tok, expr, error);
}

Expr *make_refer(const Token *tok, Expr *expr) {
  check_referable(tok, expr, "Cannot take reference");
  if (expr->kind == EX_VAR) {
    VarInfo *varinfo = scope_find(expr->var.scope, expr->var.name, NULL);
    assert(varinfo != NULL);
    varinfo->storage |= VS_REF_TAKEN;
  }
  return new_expr_unary(EX_REF, ptrof(expr->type), tok, expr);
}

static Expr *new_expr_num_bop(enum ExprKind kind, const Token *tok, Expr *lhs, Expr *rhs, bool keep_left) {
  if (is_const(lhs) && is_number(lhs->type) &&
      is_const(rhs) && is_number(rhs->type)) {
#ifndef __NO_FLONUM
    if (is_flonum(lhs->type) || is_flonum(rhs->type)) {
      double lval = is_flonum(lhs->type) ? lhs->flonum : lhs->fixnum;
      double rval = is_flonum(rhs->type) ? rhs->flonum : rhs->fixnum;
      double value;
      switch (kind) {
      case EX_MUL:     value = lval * rval; break;
      case EX_DIV:     value = lval / rval; break;
      default:
        assert(!"err");
        value = -1;  // Dummy
        break;
      }
      const Type *type = lhs->type;
      if (!keep_left && is_flonum(rhs->type))
        type = rhs->type;
      if (is_flonum(type)) {
        return new_expr_flolit(type, lhs->token, value);
      } else {
        Fixnum fixnum = value;
        return new_expr_fixlit(type, lhs->token, fixnum);
      }
    }
#endif

#define CALC(kind, l, r, value) \
  switch (kind) { \
  default: assert(false); /* Fallthrough */ \
  case EX_MUL:     value = l * r; break; \
  case EX_DIV:     value = l / r; break; \
  case EX_MOD:     value = l % r; break; \
  case EX_BITAND:  value = l & r; break; \
  case EX_BITOR:   value = l | r; break; \
  case EX_BITXOR:  value = l ^ r; break; \
  }

    intptr_t value;
    if (lhs->type->fixnum.is_unsigned) {
      UFixnum l = lhs->fixnum;
      UFixnum r = rhs->fixnum;
      CALC(kind, l, r, value)
    } else {
      Fixnum l = lhs->fixnum;
      Fixnum r = rhs->fixnum;
      CALC(kind, l, r, value)
    }
#undef CALC
    Fixnum fixnum = value;
    const Type *type = keep_left || lhs->type->fixnum.kind >= rhs->type->fixnum.kind ? lhs->type : rhs->type;
    return new_expr_fixlit(type, lhs->token, fixnum);
  }

  cast_numbers(&lhs, &rhs, keep_left);
  return new_expr_bop(kind, lhs->type, tok, lhs, rhs);
}

static Expr *new_expr_int_bop(enum ExprKind kind, const Token *tok, Expr *lhs, Expr *rhs, bool keep_left) {
  if (!is_fixnum(lhs->type->kind))
    parse_error(lhs->token, "int type expected");
  if (!is_fixnum(rhs->type->kind))
    parse_error(rhs->token, "int type expected");
  return new_expr_num_bop(kind, tok, lhs, rhs, keep_left);
}

static Expr *new_expr_addsub(enum ExprKind kind, const Token *tok, Expr *lhs, Expr *rhs, bool keep_left) {
  const Type *type = NULL;
  const Type *ltype = lhs->type;
  const Type *rtype = rhs->type;
  assert(ltype != NULL);
  assert(rtype != NULL);
  if (is_number(ltype) && is_number(rtype)) {
    if (is_const(lhs) && is_const(rhs)) {
#ifndef __NO_FLONUM
      if (is_flonum(lhs->type) || is_flonum(rhs->type)) {
        double lval = is_flonum(lhs->type) ? lhs->flonum : lhs->fixnum;
        double rval = is_flonum(rhs->type) ? rhs->flonum : rhs->fixnum;
        double value;
        switch (kind) {
        case EX_ADD:     value = lval + rval; break;
        case EX_SUB:     value = lval - rval; break;
        default:
          assert(!"err");
          value = -1;  // Dummy
          break;
        }
        const Type *type = lhs->type;
        if (!keep_left && is_flonum(rhs->type))
          type = rhs->type;
        if (is_flonum(type)) {
          return new_expr_flolit(type, lhs->token, value);
        } else {
          Fixnum fixnum = value;
          return new_expr_fixlit(type, lhs->token, fixnum);
        }
      }
#endif
      enum FixnumKind lnt = ltype->fixnum.kind;
      enum FixnumKind rnt = rtype->fixnum.kind;
      if (lnt == FX_ENUM)
        lnt = FX_INT;
      if (rnt == FX_ENUM)
        rnt = FX_INT;

      intptr_t lval = lhs->fixnum;
      intptr_t rval = rhs->fixnum;
      intptr_t value;
      switch (kind) {
      case EX_ADD: value = lval + rval; break;
      case EX_SUB: value = lval - rval; break;
      default:
        assert(false);
        value = -1;
        break;
      }
      Fixnum fixnum = value;
      const Type *type = lnt >= rnt ? lhs->type : rhs->type;
      return new_expr_fixlit(type, lhs->token, fixnum);
    }

    cast_numbers(&lhs, &rhs, keep_left);
    type = lhs->type;
  } else if (ptr_or_array(ltype)) {
    if (is_fixnum(rtype->kind)) {
      kind = kind == EX_ADD ? EX_PTRADD : EX_PTRSUB;
      type = ltype;
      if (ltype->kind == TY_ARRAY)
        type = array_to_ptr(ltype);
    } else if (kind == EX_SUB && ptr_or_array(rtype)) {
      if (ltype->kind == TY_ARRAY)
        ltype = array_to_ptr(ltype);
      if (rtype->kind == TY_ARRAY)
        rtype = array_to_ptr(rtype);
      if (!same_type(ltype, rtype))
        parse_error(tok, "Different pointer diff");
      const Fixnum elem_size = type_size(ltype->pa.ptrof);
      return new_expr_bop(EX_DIV, &tySSize, tok, new_expr_bop(EX_SUB, &tySSize, tok, lhs, rhs),
                          new_expr_fixlit(&tySSize, tok, elem_size));
    }
  } else if (ptr_or_array(rtype)) {
    if (kind == EX_ADD && is_fixnum(ltype->kind) && !keep_left) {
      kind = EX_PTRADD;
      // Swap lhs and rhs to make lhs as a pointer.
      Expr *tmp = lhs;
      lhs = rhs;
      rhs = tmp;
      type = lhs->type;
      if (type->kind == TY_ARRAY)
        type = array_to_ptr(type);
    }
  }
  if (type == NULL) {
    parse_error(tok, "Cannot apply `%.*s'", (int)(tok->end - tok->begin), tok->begin);
  }
  return new_expr_bop(kind, type, tok, lhs, rhs);
}

static Expr *new_expr_incdec(enum ExprKind kind, const Token *tok, Expr *sub) {
  check_referable(tok, sub, "lvalue expected");
  return new_expr_unary(kind, sub->type, tok, sub);
}

static enum ExprKind swap_cmp(enum ExprKind kind) {
  assert(EX_EQ <= kind && kind <= EX_GT);
  if (kind >= EX_LT)
    kind = EX_GT - (kind - EX_LT);
  return kind;
}

static Expr *new_expr_cmp(enum ExprKind kind, const Token *tok, Expr *lhs, Expr *rhs) {
  const Type *lt = lhs->type, *rt = rhs->type;
  if (ptr_or_array(lt) || ptr_or_array(rt)) {
    if (lt->kind == TY_ARRAY) {
      lt = array_to_ptr(lt);
      lhs = make_cast(lt, lhs->token, lhs, false);
    }
    if (rt->kind == TY_ARRAY) {
      rt = array_to_ptr(rt);
      rhs = make_cast(rt, rhs->token, rhs, false);
    }
    if (lt->kind != TY_PTR) {  // For comparison between pointer and 0.
      Expr *tmp = lhs;
      lhs = rhs;
      rhs = tmp;
      const Type *tt = lt;
      lt = rt;
      rt = tt;
      kind = swap_cmp(kind);
    }
    if (!can_cast(lt, rt, is_zero(rhs), false))
      parse_error(tok, "Cannot compare pointer to other types");
    if (rt->kind != TY_PTR)
      rhs = make_cast(lhs->type, rhs->token, rhs, false);
  } else {
    if (!cast_numbers(&lhs, &rhs, false))
      parse_error(tok, "Cannot compare except numbers");

    if (is_const(lhs) && is_const(rhs)) {
#define JUDGE(kind, tf, l, r)  \
switch (kind) { \
default: assert(false); /* Fallthrough */ \
case EX_EQ:  tf = l == r; break; \
case EX_NE:  tf = l != r; break; \
case EX_LT:  tf = l < r; break; \
case EX_LE:  tf = l <= r; break; \
case EX_GE:  tf = l >= r; break; \
case EX_GT:  tf = l > r; break; \
}
      bool tf;
      switch (lhs->kind) {
      default:
        assert(false);
        // Fallthrough to suppress warning.
      case EX_FIXNUM:
        assert(rhs->kind == EX_FIXNUM);
        if (lhs->type->fixnum.is_unsigned) {
          UFixnum l = lhs->fixnum, r = rhs->fixnum;
          JUDGE(kind, tf, l, r);
        } else {
          Fixnum l = lhs->fixnum, r = rhs->fixnum;
          JUDGE(kind, tf, l, r);
        }
        break;
#ifndef __NO_FLONUM
      case EX_FLONUM:
        {
          assert(rhs->kind == EX_FLONUM);
          double l = lhs->flonum, r = rhs->flonum;
          JUDGE(kind, tf, l, r);
        }
        break;
#endif
      }
      return new_expr_fixlit(&tyBool, tok, tf);
#undef JUDGE
    }
  }
  return new_expr_bop(kind, &tyBool, tok, lhs, rhs);
}

//

Expr *make_cond(Expr *expr) {
  switch (expr->kind) {
  case EX_FIXNUM:
    break;
#ifndef __NO_FLONUM
  case EX_FLONUM:
    expr = new_expr_fixlit(&tyBool, expr->token, expr->flonum != 0);
    break;
#endif
  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_LE:
  case EX_GE:
  case EX_GT:
  case EX_LOGAND:
  case EX_LOGIOR:
    break;
  default:
    switch (expr->type->kind) {
    case TY_ARRAY:
    case TY_FUNC:
      expr = new_expr_fixlit(&tyBool, expr->token, true);
      break;
    default:
      {
        Expr *zero = make_cast(expr->type, expr->token, new_expr_fixlit(&tyInt, expr->token, 0), false);
        expr = new_expr_cmp(EX_NE, expr->token, expr, zero);
      }
      break;
    }
    break;
  }
  return expr;
}

static Expr *make_not_cond(Expr *expr) {
  Expr *cond = make_cond(expr);
  enum ExprKind kind = cond->kind;
  switch (kind) {
  case EX_FIXNUM:
    cond = new_expr_fixlit(&tyBool, expr->token, cond->fixnum == 0);
    break;
#ifndef __NO_FLONUM
  case EX_FLONUM:
    expr = new_expr_fixlit(&tyBool, expr->token, expr->flonum == 0);
    break;
#endif
  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_LE:
  case EX_GE:
  case EX_GT:
    if (kind <= EX_NE)
      kind = (EX_EQ + EX_NE) - kind;
    else
      kind = EX_LT + ((kind - EX_LT) ^ 2);
    cond->kind = kind;
    break;
  case EX_LOGAND:
  case EX_LOGIOR:
    {
      Expr *lhs = make_not_cond(cond->bop.lhs);
      Expr *rhs = make_not_cond(cond->bop.rhs);
      cond = new_expr_bop((EX_LOGAND + EX_LOGIOR) - kind, &tyBool, expr->token, lhs, rhs);
    }
    break;
  default: assert(false); break;
  }
  return cond;
}

Vector *parse_args(Token **ptoken) {
  Vector *args = NULL;
  Token *token;
  if ((token = match(TK_RPAR)) == NULL) {
    args = new_vector();
    for (;;) {
      Expr *arg = parse_assign();
      vec_push(args, arg);
      if ((token = match(TK_RPAR)) != NULL)
        break;
      consume(TK_COMMA, "Comma or `)` expected");
    }
  }

  *ptoken = token;
  return args;
}

const Type *get_callee_type(Expr *func) {
  const Type *type = func->type;
  if (type->kind == TY_PTR)
    type = type->pa.ptrof;
  if (type->kind != TY_FUNC)
    parse_error(func->token, "Cannot call except function");
  return type;
}

void check_funcall_args(Expr *func, Vector *args) {
  const Type *functype = get_callee_type(func);

  const Vector *param_types = functype->func.param_types;  // <const Type*>
  bool vaargs = functype->func.vaargs;
  if (param_types != NULL) {
    int argc = args != NULL ? args->len : 0;
    int paramc = param_types->len;
    if (!(argc == paramc ||
          (vaargs && argc >= paramc)))
      parse_error(func->token, "function `%.*s' expect %d arguments, but %d", func->var.name->bytes, func->var.name->chars, paramc, argc);
  }

  if (args != NULL) {
    int paramc = param_types != NULL ? param_types->len : -1;
    for (int i = 0, len = args->len; i < len; ++i) {
      Expr *arg = args->data[i];
      if (arg->type->kind == TY_ARRAY) {
        arg = str_to_char_array_var(arg);
        arg = make_cast(array_to_ptr(arg->type), arg->token, arg, false);
      }
      if (i < paramc) {
        const Type *type = param_types->data[i];
        arg = make_cast(type, arg->token, arg, false);
      } else if (vaargs && i >= paramc) {
        const Type *type = arg->type;
        switch (type->kind) {
        case TY_FIXNUM:
          if (type->fixnum.kind < FX_INT)  // Promote variadic argument.
            arg = make_cast(&tyInt, arg->token, arg, false);
          break;
#ifndef __NO_FLONUM
        case TY_FLONUM:
          if (type->flonum.kind < FL_DOUBLE)  // Promote variadic argument.
            arg = make_cast(&tyDouble, arg->token, arg, false);
          break;
#endif
        default: break;
        }
      }
      args->data[i] = arg;
    }
  }
}

static Expr *parse_funcall(Expr *func) {
  Token *token;
  Vector *args = parse_args(&token);

  check_funcall_args(func, args);
  return new_expr_funcall(token, func, get_callee_type(func), args);
}

static Expr *parse_array_index(const Token *token, Expr *array) {
  Expr *index = parse_expr();
  consume(TK_RBRACKET, "`]' expected");
  array = str_to_char_array_var(array);
  return new_expr_deref(token, new_expr_addsub(EX_ADD, token, array, index, false));
}

static Expr *parse_member_access(Expr *target, Token *acctok) {
  Token *ident = consume(TK_IDENT, "`ident' expected");
  const Name *name = ident->ident;

  // Find member's type from struct info.
  const Type *type = target->type;
  if (acctok->kind == TK_DOT) {
    if (type->kind != TY_STRUCT)
      parse_error(acctok, "`.' for non struct value");
  } else {  // TK_ARROW
    if (!ptr_or_array(type)) {
      parse_error(acctok, "`->' for non pointer value");
    } else {
      type = type->pa.ptrof;
      if (type->kind != TY_STRUCT)
        parse_error(acctok, "`->' for non struct value");
    }
  }

  ensure_struct((Type*)type, ident, curscope);
  int index = var_find(type->struct_.info->members, name);
  if (index >= 0) {
    const VarInfo *member = type->struct_.info->members->data[index];
    const Type *type = qualified_type(member->type, target->type->qualifier);
    return new_expr_member(acctok, type, target, ident, index);
  } else {
    Vector *stack = new_vector();
    const VarInfo *member = search_from_anonymous(type, ident->ident, ident, stack);
    if (member == NULL)
      parse_error(ident, "`%.*s' doesn't exist in the struct", name->bytes, name->chars);
    Expr *p = target;
    for (int i = 0; i < stack->len; ++i) {
      int index = (int)(long)stack->data[i];
      const VarInfo *member = type->struct_.info->members->data[index];
      type = qualified_type(member->type, type->qualifier);
      p = new_expr_member(acctok, type, p, acctok, index);
    }
    return p;
  }
}

static void parse_enum_members(const Type *type) {
  assert(type != NULL && type->kind == TY_FIXNUM && type->fixnum.kind == FX_ENUM);
  const Type *ctype = qualified_type(type, TQ_CONST);
  int value = 0;
  for (;;) {
    Token *token = consume(TK_IDENT, "ident expected");
    if (match(TK_ASSIGN)) {
      Expr *expr = parse_const();
      value = expr->fixnum;
    }

    if (scope_find(global_scope, token->ident, NULL) != NULL) {
      parse_error(token, "`%.*s' is already defined",
                  token->ident->bytes, token->ident->chars);
    } else {
      define_enum_member(ctype, token, value);
    }
    ++value;

    if (match(TK_COMMA))
      ;
    if (match(TK_RBRACE))
      break;
  }
}

static const Type *parse_enum(void) {
  Token *ident = match(TK_IDENT);
  Type *type = ident != NULL ? find_enum(curscope, ident->ident) : NULL;
  if (match(TK_LBRACE)) {
    if (type != NULL)
      parse_error(ident, "Duplicate enum type");
    type = define_enum(curscope, ident != NULL ? ident->ident : NULL);
    if (!match(TK_RBRACE))
      parse_enum_members(type);
  } else {
    if (type == NULL)
      parse_error(ident, "Unknown enum type");
  }
  return type;
}

const Type *parse_raw_type(int *pstorage) {
  const Type *type = NULL;

  int storage = 0, qualifier = 0;
  bool is_unsigned = false;
  int long_count = 0;
  for (;;) {
    Token *tok;
    if (match(TK_UNSIGNED)) {
      is_unsigned = true;
      continue;
    }
    if (match(TK_SIGNED)) {
      is_unsigned = false;
      continue;
    }
    if (match(TK_STATIC)) {
      storage |= VS_STATIC;
      continue;
    }
    if (match(TK_EXTERN)) {
      storage |= VS_EXTERN;
      continue;
    }
    if (match(TK_TYPEDEF)) {
      storage |= VS_TYPEDEF;
      continue;
    }
    if (match(TK_CONST)) {
      qualifier |= TQ_CONST;
      continue;
    }
    if (match(TK_VOLATILE)) {
      qualifier |= TQ_VOLATILE;
      continue;
    }
    if ((tok = match(TK_LONG)) != NULL) {
      ++long_count;
      if (long_count > 2)
        parse_error(tok, "Too many `long'");
      continue;
    }

    if (type != NULL)
      break;

    Token *ident;
    if (((tok = match(TK_STRUCT)) != NULL) ||
        ((tok = match(TK_UNION)) != NULL)) {
      if (is_unsigned)
        parse_error(tok, "`unsigned' for struct/union");

      bool is_union = tok->kind == TK_UNION;
      const Name *name = NULL;
      Token *ident;
      if ((ident = match(TK_IDENT)) != NULL)
        name = ident->ident;

      StructInfo *sinfo = NULL;
      if (match(TK_LBRACE)) {  // Definition
        sinfo = parse_struct(is_union);
        if (name != NULL) {
          Scope *scope;
          StructInfo *exist = find_struct(curscope, name, &scope);
          if (exist != NULL && scope == curscope)
            parse_error(ident, "`%.*s' already defined", name->bytes, name->chars);
          define_struct(curscope, name, sinfo);
        }
      } else {
        if (name != NULL) {
          sinfo = find_struct(curscope, name, NULL);
          if (sinfo != NULL) {
            if (sinfo->is_union != is_union)
              parse_error(tok, "Wrong tag for `%.*s'", name->bytes, name->chars);
          }
        }
      }

      if (name == NULL && sinfo == NULL)
        parse_error(NULL, "Illegal struct/union usage");

      type = create_struct_type(sinfo, name, qualifier);
    } else if ((tok = match(TK_ENUM)) != NULL) {
      if (is_unsigned)
        parse_error(tok, "`unsigned' for enum");

      type = parse_enum();
    } else if ((ident = match(TK_IDENT)) != NULL) {
      type = find_typedef(curscope, ident->ident, NULL);
      if (type == NULL) {
        unget_token(ident);
      } else {
        if (is_unsigned)
          parse_error(ident, "`unsigned' for typedef");
      }
    } else if ((tok = match(TK_VOID)) != NULL) {
      if (is_unsigned)
        parse_error(tok, "`unsigned' for void");

      type = &tyVoid;
#ifndef __NO_FLONUM
    } else if (match(TK_FLOAT)) {
      type = &tyFloat;
    } else if (match(TK_DOUBLE)) {
      type = &tyDouble;
#endif
    } else {
      static const enum TokenKind kIntTypeTokens[] = {
        TK_CHAR, TK_SHORT, TK_INT,
      };
      static const enum FixnumKind kFixnumKinds[] = {
        FX_CHAR, FX_SHORT, FX_INT,
      };
      const int N = sizeof(kIntTypeTokens) / sizeof(*kIntTypeTokens);
      for (int i = 0; i < N; ++i) {
        if ((tok = match(kIntTypeTokens[i])) != NULL) {
          switch (long_count) {
          default:
            // Fallthrough
          case 0:
            type = get_fixnum_type(kFixnumKinds[i], is_unsigned, qualifier);
            break;
          case 1: case 2:
            if (i != sizeof(kIntTypeTokens) / sizeof(*kIntTypeTokens) - 1)
              parse_error(tok, "`long' can use only with `int' ");
            break;
          }
          break;
        }
      }
    }
    if (type == NULL)
      break;
  }

  if (type == NULL && (storage != 0 || is_unsigned || long_count > 0)) {
    static const enum FixnumKind kLongKinds[] = {
      FX_INT, FX_LONG, FX_LLONG,
    };
    type = get_fixnum_type(kLongKinds[long_count], is_unsigned, qualifier);
  }

  if (pstorage != NULL)
    *pstorage = storage;

  return type;
}

const Type *parse_type_modifier(const Type *type) {
  if (type == NULL)
    return NULL;

  for (;;) {
    if (match(TK_CONST))
      type = qualified_type(type, TQ_CONST);
    if (match(TK_MUL))
      type = ptrof(type);
    else
      break;
  }

  return type;
}

const Type *parse_type_suffix(const Type *type) {
  if (type == NULL)
    return NULL;

  if (!match(TK_LBRACKET))
    return type;
  size_t length = -1;
  if (match(TK_RBRACKET)) {
    // Arbitrary size.
  } else {
    const Token *tok = fetch_token();
    Expr *expr = parse_const();
    if (expr->fixnum <= 0)
      parse_error(tok, "Array size must be greater than 0, but %d", (int)expr->fixnum);
    length = expr->fixnum;
    consume(TK_RBRACKET, "`]' expected");
  }
  return arrayof(parse_type_suffix(type), length);
}

Vector *extract_varinfo_types(const Vector *params) {
  Vector *param_types = NULL;
  if (params != NULL) {
    param_types = new_vector();
    for (int i = 0, len = params->len; i < len; ++i)
      vec_push(param_types, ((VarInfo*)params->data[i])->type);
  }
  return param_types;
}

static const Type *parse_var_def_cont(const Type *type) {
  if (match(TK_LPAR)) {
    const Type *rettype = type;
    bool vaargs;
    Vector *params = parse_funparams(&vaargs);
    Vector *param_types = extract_varinfo_types(params);
    type = new_func_type(rettype, params, param_types, vaargs);
  }
  if (type->kind != TY_VOID)
    type = parse_type_suffix(type);

  return type;
}

bool parse_var_def(const Type **prawType, const Type **ptype, int *pstorage, Token **pident) {
  const Type *rawType = prawType != NULL ? *prawType : NULL;
  if (rawType == NULL) {
    rawType = parse_raw_type(pstorage);
    if (rawType == NULL)
      return false;
    if (prawType != NULL)
      *prawType = rawType;
  }

  const Type *type = parse_type_modifier(rawType);
  Token *ident;
  if (match(TK_LPAR)) {  // Funcion pointer type.
    const Type *base_type = type;
    Type *place_holder = calloc(1, sizeof(*place_holder));
    type = parse_type_modifier(place_holder);
    ident = match(TK_IDENT);
    type = parse_type_suffix(type);
    consume(TK_RPAR, "`)' expected");

    const Type *inner_type = parse_var_def_cont(base_type);
    memcpy(place_holder, inner_type, sizeof(*place_holder));
  } else {
    ident = match(TK_IDENT);
    type = parse_var_def_cont(type);
  }
  *ptype = type;
  if (pident != NULL)
    *pident = ident;
  return true;
}

const Type *parse_full_type(int *pstorage, Token **pident) {
  const Type *type;
  if (!parse_var_def(NULL, &type, pstorage, pident))
    return NULL;
  return type;
}

Vector *parse_funparams(bool *pvaargs) {
  Vector *params = NULL;
  bool vaargs = false;
  if (match(TK_RPAR)) {
    // Arbitrary funparams.
  } else {
    params = new_vector();
    for (;;) {
      if (match(TK_ELLIPSIS)) {
        vaargs = true;
        consume(TK_RPAR, "`)' expected");
        break;
      }

      const Type *type;
      int storage;
      Token *ident;
      if (!parse_var_def(NULL, &type, &storage, &ident))
        parse_error(NULL, "type expected");
      if (storage & VS_STATIC)
        parse_error(ident, "`static' for function parameter");
      if (storage & VS_EXTERN)
        parse_error(ident, "`extern' for function parameter");
      if (storage & VS_TYPEDEF)
        parse_error(ident, "`typedef' for function parameter");

      if (params->len == 0) {
        if (type->kind == TY_VOID) {  // fun(void)
          if (ident != NULL || !match(TK_RPAR))
            parse_error(NULL, "`)' expected");
          break;
        }
      } else {
        not_void(type, NULL);
      }

      // If the type is array, handle it as a pointer.
      if (type->kind == TY_ARRAY)
        type = array_to_ptr(type);

      var_add(params, ident != NULL ? ident->ident : NULL, type, storage, ident);
      if (match(TK_RPAR))
        break;
      consume(TK_COMMA, "Comma or `)' expected");
    }
  }
  *pvaargs = vaargs;
  return params;
}

// Parse struct or union definition `{...}`
static StructInfo *parse_struct(bool is_union) {
  Vector *members = new_vector();
  for (;;) {
    if (match(TK_RBRACE))
      break;

    const Type *rawType = NULL;
    for (;;) {
      const Type *type;
      int storage;
      Token *ident;
      if (!parse_var_def(&rawType, &type, &storage, &ident))
        parse_error(NULL, "type expected");
      not_void(type, NULL);
      if (type->kind == TY_STRUCT) {
        ensure_struct((Type*)type, ident, curscope);
        // Allow ident to be null for anonymous struct member.
      } else {
        if (ident == NULL)
          parse_error(NULL, "`ident' expected");
      }
      const Name *name = ident != NULL ? ident->ident : NULL;
      var_add(members, name, type, storage, ident);

      if (match(TK_COMMA))
        continue;
      consume(TK_SEMICOL, "`;' expected");
      break;
    }
  }
  return create_struct_info(members, is_union);
}

static Expr *parse_compound_literal(const Type *type) {
  Token *token = fetch_token();
  Initializer *init = parse_initializer();
  const Name *name = NULL;
  Vector *inits = NULL;
  Expr *var = NULL;

  if (is_global_scope(curscope)) {
    parse_error(token, "cannot use compound literal in global");
  } else {
    if (type->kind == TY_ARRAY)
      fix_array_size((Type*)type, init);

    name = alloc_label();
    const Token *ident = alloc_ident(name, NULL, NULL);
    scope_add(curscope, ident, type, 0);

    var = new_expr_variable(name, type, token, curscope);
    inits = assign_initial_value(var, init, NULL);
  }

  return new_expr_complit(type, token, var, inits);
}

static Expr *parse_prim(void) {
  Token *tok;
  if ((tok = match(TK_LPAR)) != NULL) {
    int storage;
    const Type *type = parse_full_type(&storage, NULL);
    if (type != NULL) {  // Compound literal
      consume(TK_RPAR, "`)' expected");
      Token *tok2 = consume(TK_LBRACE, "`{' expected");
      unget_token(tok2);
      return parse_compound_literal(type);
    } else {
      Expr *expr = parse_expr();
      consume(TK_RPAR, "No close paren");
      return expr;
    }
  }

  {
    static const struct {
      enum TokenKind tk;
      enum FixnumKind fx;
      bool is_unsigned;
    } TABLE[] = {
      {TK_CHARLIT, FX_CHAR, false},
      {TK_INTLIT, FX_INT, false},
      {TK_LONGLIT, FX_LONG, false},
      {TK_LLONGLIT, FX_LLONG, false},
      {TK_UCHARLIT, FX_CHAR, true},
      {TK_UINTLIT, FX_INT, true},
      {TK_ULONGLIT, FX_LONG, true},
      {TK_ULLONGLIT, FX_LLONG, true},
    };
    for (int i = 0, n = sizeof(TABLE) / sizeof(*TABLE); i < n; ++i) {
      if ((tok = match(TABLE[i].tk)) != NULL) {
        const Type *type = get_fixnum_type(TABLE[i].fx, TABLE[i].is_unsigned, 0);
        Fixnum fixnum = tok->fixnum;
        return new_expr_fixlit(type, tok, fixnum);
      }
    }
  }
#ifndef __NO_FLONUM
  if ((tok = match(TK_FLOATLIT)) != NULL) {
    return new_expr_flolit(&tyFloat, tok, tok->flonum);
  }
  if ((tok = match(TK_DOUBLELIT)) != NULL) {
    return new_expr_flolit(&tyDouble, tok, tok->flonum);
  }
#endif

  if ((tok = match(TK_STR)) != NULL)
    return new_expr_str(tok, tok->str.buf, tok->str.size);

  Token *ident = consume(TK_IDENT, "Number or Ident or open paren expected");
  const Name *name = ident->ident;
  {
    Expr *(*proc)(const Token*) = table_get(&builtin_expr_ident_table, name);
    if (proc != NULL) {
      return proc(ident);
    }
  }
  Scope *scope;
  VarInfo *varinfo = scope_find(curscope, name, &scope);
  const Type *type;
  if (varinfo != NULL) {
    if (varinfo->storage & VS_ENUM_MEMBER)
      return new_expr_fixlit(varinfo->type, ident, varinfo->enum_member.value);
    type = varinfo->type;
  } else {
    parse_error(ident, "undefined indentifier");
    type = &tyInt;
  }
  return new_expr_variable(name, type, ident, scope);
}

static Expr *parse_postfix(void) {
  Expr *expr = parse_prim();

  for (;;) {
    Token *tok;
    if (match(TK_LPAR))
      expr = parse_funcall(expr);
    else if ((tok = match(TK_LBRACKET)) != NULL)
      expr = parse_array_index(tok, expr);
    else if ((tok = match(TK_DOT)) != NULL || (tok = match(TK_ARROW)) != NULL)
      expr = parse_member_access(expr, tok);
    else if ((tok = match(TK_INC)) != NULL) {
      not_const(expr->type, tok);
      expr = new_expr_incdec(EX_POSTINC, tok, expr);
    } else if ((tok = match(TK_DEC)) != NULL) {
      not_const(expr->type, tok);
      expr = new_expr_incdec(EX_POSTDEC, tok, expr);
    } else
      return expr;
  }
}

static Expr *parse_sizeof(const Token *token) {
  const Type *type = NULL;
  Token *tok;
  if ((tok = match(TK_LPAR)) != NULL) {
    type = parse_full_type(NULL, NULL);
    if (type != NULL) {
      consume(TK_RPAR, "`)' expected");
    } else {
      unget_token(tok);
      Expr *expr = parse_prim();
      type = expr->type;
    }
  } else {
    Expr *expr = parse_unary();
    type = expr->type;
  }
  assert(type != NULL);
  if (type->kind == TY_STRUCT)
    ensure_struct((Type*)type, token, curscope);
  if (type->kind == TY_ARRAY) {
    if (type->pa.length == (size_t)-1) {
      // TODO: assert `export` modifier.
      parse_error(token, "size unknown");
    }
  }

  const Fixnum size = type_size(type);
  return new_expr_fixlit(&tySize, token, size);
}

static Expr *parse_unary(void) {
  Token *tok;
  if ((tok = match(TK_ADD)) != NULL) {
    Expr *expr = parse_cast_expr();
    if (!is_number(expr->type))
      parse_error(tok, "Cannot apply `+' except number types");
    if (is_const(expr))
      return expr;
    return new_expr_unary(EX_POS, expr->type, tok, expr);
  }

  if ((tok = match(TK_SUB)) != NULL) {
    Expr *expr = parse_cast_expr();
    if (!is_number(expr->type))
      parse_error(tok, "Cannot apply `-' except number types");
    if (is_const(expr)) {
#ifndef __NO_FLONUM
      if (is_flonum(expr->type)) {
        expr->flonum = -expr->flonum;
        return expr;
      }
#endif
      expr->fixnum = -expr->fixnum;
      if (expr->type->fixnum.is_unsigned) {
        size_t size = type_size(expr->type);
        if (size < sizeof(UFixnum))
          expr->fixnum &= (((UFixnum)1) << (size * CHAR_BIT)) - 1;
      }
      return expr;
    }
    return new_expr_unary(EX_NEG, expr->type, tok, expr);
  }

  if ((tok = match(TK_NOT)) != NULL) {
    Expr *expr = parse_cast_expr();
    if (!is_number(expr->type) && !ptr_or_array(expr->type))
      parse_error(tok, "Cannot apply `!' except number or pointer types");
    if (is_const(expr)) {
      switch (expr->kind) {
      case EX_FIXNUM:
        expr->fixnum = !expr->fixnum;
        break;
#ifndef __NO_FLONUM
      case EX_FLONUM:
        {
          Fixnum value = expr->fixnum == 0;
          expr = new_expr_fixlit(&tyBool, tok, value);
        }
        break;
#endif
      case EX_STR:
        {
          Fixnum value = 0;
          expr = new_expr_fixlit(&tyBool, tok, value);
        }
        break;
      default:
        assert(false);
        break;
      }
      return expr;
    }
    return make_not_cond(expr);
  }

  if ((tok = match(TK_TILDA)) != NULL) {
    Expr *expr = parse_cast_expr();
    if (!is_fixnum(expr->type->kind))
      parse_error(tok, "Cannot apply `~' except number type");
    if (is_const(expr)) {
      expr->fixnum = ~expr->fixnum;
      return expr;
    }
    return new_expr_unary(EX_BITNOT, expr->type, tok, expr);
  }

  if ((tok = match(TK_AND)) != NULL) {
    Expr *expr = parse_cast_expr();
    assert(expr->type != NULL);
    return make_refer(tok, expr);
  }

  if ((tok = match(TK_MUL)) != NULL) {
    Expr *expr = parse_cast_expr();
    const Type *type = expr->type;
    assert(type != NULL);
    switch (type->kind) {
    case TY_PTR: case TY_ARRAY:
      type = type->pa.ptrof;
      break;
    case TY_FUNC:
      break;
    default:
      parse_error(tok, "Cannot dereference raw type");
      break;
    }
    expr = str_to_char_array_var(expr);
    return new_expr_unary(EX_DEREF, type, tok, expr);
  }

  if ((tok = match(TK_INC)) != NULL) {
    Expr *expr = parse_unary();
    not_const(expr->type, tok);
    return new_expr_incdec(EX_PREINC, tok, expr);
  }

  if ((tok = match(TK_DEC)) != NULL) {
    Expr *expr = parse_unary();
    not_const(expr->type, tok);
    return new_expr_incdec(EX_PREDEC, tok, expr);
  }

  if ((tok = match(TK_SIZEOF)) != NULL) {
    return parse_sizeof(tok);
  }

  return parse_postfix();
}

static Expr *parse_cast_expr(void) {
  Token *lpar;
  if ((lpar = match(TK_LPAR)) != NULL) {
    int storage;
    const Token *token = fetch_token();
    const Type *type = parse_full_type(&storage, NULL);
    if (type != NULL) {  // Cast
      consume(TK_RPAR, "`)' expected");

      Token *token2 = fetch_token();
      if (token2 != NULL && token2->kind == TK_LBRACE) {
        return parse_compound_literal(type);
      } else {
        Expr *sub = parse_cast_expr();
        check_cast(type, sub->type, is_zero(sub), true, token);
        if (is_const(sub) && type->kind != TY_VOID)
          return make_cast(type, token, sub, true);
        else
          return sub->type->kind != TY_VOID ? new_expr_cast(type, token, sub) : sub;
      }
    }
    unget_token(lpar);
  }
  return parse_unary();
}

static Expr *parse_mul(void) {
  Expr *expr = parse_cast_expr();

  for (;;) {
    enum ExprKind kind;
    Token *tok;
    if ((tok = match(TK_MUL)) != NULL)
      kind = EX_MUL;
    else if ((tok = match(TK_DIV)) != NULL)
      kind = EX_DIV;
    else if ((tok = match(TK_MOD)) != NULL)
      kind = EX_MOD;
    else
      return expr;

    Expr *lhs = expr, *rhs = parse_cast_expr();
    expr = new_expr_num_bop(kind, tok, lhs, rhs, false);
  }
}

static Expr *parse_add(void) {
  Expr *expr = parse_mul();

  for (;;) {
    enum ExprKind kind;
    Token *tok;
    if ((tok = match(TK_ADD)) != NULL)
      kind = EX_ADD;
    else if ((tok = match(TK_SUB)) != NULL)
      kind = EX_SUB;
    else
      return expr;

    Expr *lhs = expr, *rhs = parse_mul();
    expr = new_expr_addsub(kind, tok, lhs, rhs, false);
  }
}

static Expr *parse_shift(void) {
  Expr *expr = parse_add();

  for (;;) {
    enum ExprKind kind;
    Token *tok;
    if ((tok = match(TK_LSHIFT)) != NULL)
      kind = EX_LSHIFT;
    else if ((tok = match(TK_RSHIFT)) != NULL)
      kind = EX_RSHIFT;
    else
      return expr;

    Expr *lhs = expr, *rhs = parse_add();
    if (!is_fixnum(lhs->type->kind) ||
        !is_fixnum(rhs->type->kind))
      parse_error(tok, "Cannot use `%.*s' except numbers.", (int)(tok->end - tok->begin), tok->begin);

    if (is_const(lhs) && is_const(rhs)) {
      Fixnum value;
      if (lhs->type->fixnum.is_unsigned) {
        UFixnum lval = lhs->fixnum;
        UFixnum rval = rhs->fixnum;
        value = kind == EX_LSHIFT ? lval << rval : lval >> rval;
      } else {
        Fixnum lval = lhs->fixnum;
        Fixnum rval = rhs->fixnum;
        value = kind == EX_LSHIFT ? lval << rval : lval >> rval;
      }
      expr = new_expr_fixlit(lhs->type, tok, value);
    } else {
      expr = new_expr_bop(kind, lhs->type, tok, lhs, rhs);
    }
  }
}

static Expr *parse_cmp(void) {
  Expr *expr = parse_shift();

  for (;;) {
    enum ExprKind kind;
    Token *tok;
    if ((tok = match(TK_LT)) != NULL)
      kind = EX_LT;
    else if ((tok = match(TK_GT)) != NULL)
      kind = EX_GT;
    else if ((tok = match(TK_LE)) != NULL)
      kind = EX_LE;
    else if ((tok = match(TK_GE)) != NULL)
      kind = EX_GE;
    else
      return expr;

    Expr *lhs = expr, *rhs = parse_shift();
    expr = new_expr_cmp(kind, tok, lhs, rhs);
  }
}

static Expr *parse_eq(void) {
  Expr *expr = parse_cmp();

  for (;;) {
    enum ExprKind kind;
    Token *tok;
    if ((tok = match(TK_EQ)) != NULL)
      kind = EX_EQ;
    else if ((tok = match(TK_NE)) != NULL)
      kind = EX_NE;
    else
      return expr;

    Expr *lhs = expr, *rhs = parse_cmp();
    expr = new_expr_cmp(kind, tok, lhs, rhs);
  }
}

static Expr *parse_and(void) {
  Expr *expr = parse_eq();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_AND)) == NULL)
      return expr;

    Expr *lhs = expr, *rhs = parse_eq();
    expr = new_expr_int_bop(EX_BITAND, tok, lhs, rhs, false);
  }
}

static Expr *parse_xor(void) {
  Expr *expr = parse_and();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_HAT)) == NULL)
      return expr;

    Expr *lhs = expr, *rhs = parse_and();
    expr = new_expr_int_bop(EX_BITXOR, tok, lhs, rhs, false);
  }
}

static Expr *parse_or(void) {
  Expr *expr = parse_xor();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_OR)) == NULL)
      return expr;

    Expr *lhs = expr, *rhs = parse_xor();
    expr = new_expr_int_bop(EX_BITOR, tok, lhs, rhs, false);
  }
}

static Expr *parse_logand(void) {
  Expr *expr = parse_or();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_LOGAND)) != NULL)
      expr = new_expr_bop(EX_LOGAND, &tyBool, tok, make_cond(expr), make_cond(parse_or()));
    else
      return expr;
  }
}

static Expr *parse_logior(void) {
  Expr *expr = parse_logand();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_LOGIOR)) != NULL)
      expr = new_expr_bop(EX_LOGIOR, &tyBool, tok, make_cond(expr), make_cond(parse_logand()));
    else
      return expr;
  }
}

static const Type *to_ptr_type(const Type *type) {
  switch (type->kind) {
  case TY_ARRAY: return array_to_ptr(type);
  case TY_FUNC:  return ptrof(type);
  default:  return type;
  }
}

static const Type *choose_type(Expr *tval, Expr *fval) {
  const Type *ttype = tval->type;
  const Type *ftype = fval->type;
  ttype = to_ptr_type(ttype);
  ftype = to_ptr_type(ftype);

  if (ftype->kind == TY_ARRAY)
    ftype = array_to_ptr(ftype);

  if (same_type(ttype, ftype))
    return ttype;
  if (ttype->kind == TY_PTR) {
    if (ftype->kind == TY_PTR) {  // Both pointer type
      if (is_void_ptr(ttype))
        return ftype;
      if (is_void_ptr(ftype))
        return ttype;
    } else {
      if (can_cast(ttype, ftype, is_zero(fval), false))
        return ttype;
    }
  } else if (ftype->kind == TY_PTR) {
    return choose_type(fval, tval);  // Make ttype to pointer, and check again.
  } else if (is_number(ttype) && is_number(ftype)) {
#ifndef __NO_FLONUM
    if (is_flonum(ttype)) {
      // TODO: Choose lager one.
      //if (is_flonum(ftype)) {
      //  return ttype;
      //}
      return ttype;
    } else if (is_flonum(ftype)) {
      return ftype;
    }
#endif
    assert(is_fixnum(ttype->kind));
    assert(is_fixnum(ftype->kind));
    if (ttype->fixnum.kind > ftype->fixnum.kind)
      return ttype;
    else
      return ftype;
  }
  return NULL;
}

static Expr *parse_conditional(void) {
  Expr *expr = parse_logior();
  for (;;) {
    const Token *tok;
    if ((tok = match(TK_QUESTION)) == NULL)
      return expr;
    Expr *tval = parse_expr();
    consume(TK_COLON, "`:' expected");
    Expr *fval = parse_conditional();

    tval = str_to_char_array_var(tval);
    fval = str_to_char_array_var(fval);

    const Type *type;
    if (tval->type->kind == TY_VOID || fval->type->kind == TY_VOID) {
      type = &tyVoid;
    } else {
      type = choose_type(tval, fval);
      if (type == NULL)
        parse_error(tok, "lhs and rhs must be same type");
      assert(type->kind != TY_VOID);
      tval = make_cast(type, tval->token, tval, false);
      fval = make_cast(type, fval->token, fval, false);
    }
    if (is_const(expr)) {
      bool tf;
      switch (expr->kind) {
      case EX_FIXNUM:  tf = expr->fixnum != 0; break;
#ifndef __NO_FLONUM
      case EX_FLONUM:  tf = expr->flonum != 0; break;
#endif
      default:
        assert(false);
        // Fallthrough to avoid warning.
      case EX_STR:     tf = true; break;
      }
      expr = tf ? tval : fval;
    } else {
      expr = new_expr_ternary(tok, make_cond(expr), tval, fval, type);
    }
  }
}

Expr *parse_assign(void) {
  enum {
    ASSIGN,
    ADDSUB,
    MULDIV,
    FIXNUM_BOP,
    SHIFT,
  };

  static const struct {
    enum TokenKind tk;
    enum ExprKind ex;
    int mode;
  } kAssignWithOps[] = {
    { TK_ASSIGN, EX_ASSIGN, ASSIGN },
    { TK_ADD_ASSIGN, EX_ADD, ADDSUB },
    { TK_SUB_ASSIGN, EX_SUB, ADDSUB },
    { TK_MUL_ASSIGN, EX_MUL, MULDIV },
    { TK_DIV_ASSIGN, EX_DIV, MULDIV },
    { TK_MOD_ASSIGN, EX_MOD, FIXNUM_BOP },
    { TK_AND_ASSIGN, EX_BITAND, FIXNUM_BOP },
    { TK_OR_ASSIGN, EX_BITOR, FIXNUM_BOP },
    { TK_HAT_ASSIGN, EX_BITXOR, FIXNUM_BOP },
    { TK_LSHIFT_ASSIGN, EX_LSHIFT, SHIFT },
    { TK_RSHIFT_ASSIGN, EX_RSHIFT, SHIFT },
  };

  Expr *expr = parse_conditional();

  Token *tok = match(-1);
  if (tok != NULL) {
    for (int i = 0; i < (int)(sizeof(kAssignWithOps) / sizeof(*kAssignWithOps)); ++i) {
      if (tok->kind == kAssignWithOps[i].tk) {
        enum ExprKind kind = kAssignWithOps[i].ex;
        Expr *lhs = expr, *rhs = parse_assign();

        check_lval(tok, lhs, "Cannot assign");

        switch (lhs->type->kind) {
        case TY_ARRAY:
          parse_error(tok, "Cannot assign to array");
          break;
        case TY_FUNC:
          parse_error(tok, "Cannot assign to function");
          break;
        default: break;
        }

        not_const(lhs->type, tok);
        Expr *bop;
        switch (kAssignWithOps[i].mode) {
        case ASSIGN:
          rhs = str_to_char_array_var(rhs);
          return new_expr_bop(EX_ASSIGN, lhs->type, tok, lhs, make_cast(lhs->type, tok, rhs, false));
        case ADDSUB:  bop = new_expr_addsub(kind, tok, lhs, rhs, true); break;
        case MULDIV:  bop = new_expr_num_bop(kind, tok, lhs, rhs, true); break;
        case FIXNUM_BOP:  bop = new_expr_int_bop(kind, tok, lhs, rhs, true); break;
        case SHIFT:
          {
            const Type *ltype = lhs->type;
            const Type *rtype = rhs->type;
            assert(ltype != NULL);
            assert(rtype != NULL);
            if (!is_fixnum(ltype->kind) || !is_fixnum(rtype->kind))
              parse_error(tok, "Cannot use `%.*s' except numbers.", (int)(tok->end - tok->begin), tok->begin);
            bop = new_expr_bop(kind, lhs->type, tok, lhs, rhs);
          }
          break;
        default:  assert(false); bop = NULL; break;
        }
        assert(bop->type != NULL);
        return new_expr_unary(EX_MODIFY, lhs->type, tok, bop);
      }
    }
    unget_token(tok);
  }
  return expr;
}

Expr *parse_const(void) {
  Expr *expr = parse_conditional();
  if (!(is_const(expr) && is_fixnum(expr->type->kind)))
    parse_error(expr->token, "constant value expected");
  return expr;
}

Expr *parse_expr(void) {
  Expr *expr = parse_assign();
  Expr *last = expr;
  const Token *tok;
  while ((tok = match(TK_COMMA)) != NULL) {
    Expr *next_expr = parse_assign();
    if (is_const(expr))
      expr = last = next_expr;
    else if (is_const(next_expr))
      last = next_expr;
    else
      expr = last = new_expr_bop(EX_COMMA, next_expr->type, tok, expr, next_expr);
  }
  if (expr != last)
    expr = new_expr_bop(EX_COMMA, last->type, tok, expr, last);
  return expr;
}
#include "regalloc.h"

#include <assert.h>
#include <limits.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "ast.h"
#include "codegen.h"  // WORD_SIZE
#include "ir.h"
#include "type.h"
#include "util.h"
#include "var.h"

#if (defined(__linux__) || defined(__APPLE__)) && !defined(__XCC) && !defined(__XV6)
#define USE_ALLOCA
#endif

#if defined(USE_ALLOCA)
#include <alloca.h>
#define ALLOCA(size)  alloca(size)
#else
#define ALLOCA(size)  malloc(size)
#endif

#define SPILLED_REG_NO(ra)  (ra->phys_max)
#ifndef __NO_FLONUM
#define SPILLED_FREG_NO(ra)  (ra->fphys_max)
#endif

static void spill_vreg(RegAlloc *ra, VReg *vreg) {
  vreg->phys = SPILLED_REG_NO(ra);
}

// Register allocator

RegAlloc *new_reg_alloc(int phys_max) {
  RegAlloc *ra = malloc(sizeof(*ra));
  ra->vregs = new_vector();
  //ra->regno = 0;
  vec_clear(ra->vregs);
  ra->frame_size = 0;
  ra->phys_max = phys_max;
#ifndef __NO_FLONUM
  ra->fphys_max = 0;
#else
  assert(phys_max < (int)(sizeof(ra->used_reg_bits) * CHAR_BIT));
#endif
  ra->used_reg_bits = 0;
  return ra;
}

VReg *reg_alloc_spawn(RegAlloc *ra, const VRegType *vtype, int flag) {
  VReg *vreg = new_vreg(ra->vregs->len, vtype, flag);
  vec_push(ra->vregs, vreg);
  return vreg;
}

static int insert_active(LiveInterval **active, int active_count, LiveInterval *li) {
  int j;
  for (j = 0; j < active_count; ++j) {
    LiveInterval *p = active[j];
    if (li->end < p->end)
      break;
  }
  if (j < active_count)
    memmove(&active[j + 1], &active[j], sizeof(LiveInterval*) * (active_count - j));
  active[j] = li;
  return j;
}

static void remove_active(LiveInterval **active, int active_count, int start, int n) {
  if (n <= 0)
    return;
  int tail = active_count - (start + n);
  assert(tail >= 0);

  if (tail > 0)
    memmove(&active[start], &active[start + n], sizeof(LiveInterval*) * tail);
}

static int sort_live_interval(const void *pa, const void *pb) {
  LiveInterval *a = *(LiveInterval**)pa, *b = *(LiveInterval**)pb;
  int d = a->start - b->start;
  if (d == 0)
    d = b->end - a->start;
  return d;
}

static void split_at_interval(RegAlloc *ra, LiveInterval **active, int active_count,
                              LiveInterval *li) {
  assert(active_count > 0);
  LiveInterval *spill = active[active_count - 1];
  if (spill->end > li->end) {
    li->phys = spill->phys;
    spill->phys = ra->phys_max;
    spill->state = LI_SPILL;
    insert_active(active, active_count - 1, li);
  } else {
    li->phys = ra->phys_max;
    li->state = LI_SPILL;
  }
}

static void expire_old_intervals(
  LiveInterval **active, int *pactive_count, unsigned short *pusing_bits, int start
) {
  int active_count = *pactive_count;
  int j;
  short using_bits = *pusing_bits;
  for (j = 0; j < active_count; ++j) {
    LiveInterval *li = active[j];
    if (li->end > start)
      break;
    using_bits &= ~((short)1 << li->phys);
  }
  remove_active(active, active_count, 0, j);
  *pactive_count = active_count - j;
  *pusing_bits = using_bits;
}

static void set_inout_interval(Vector *regs, LiveInterval *intervals, int nip) {
  for (int j = 0; j < regs->len; ++j) {
    VReg *reg = regs->data[j];
    LiveInterval *li = &intervals[reg->virt];
    if (li->start < 0 || li->start > nip)
      li->start = nip;
    if (li->end < nip)
      li->end = nip;
  }
}

static LiveInterval **check_live_interval(BBContainer *bbcon, int vreg_count,
                                          LiveInterval **pintervals) {
  LiveInterval *intervals = malloc(sizeof(LiveInterval) * vreg_count);
  for (int i = 0; i < vreg_count; ++i) {
    LiveInterval *li = &intervals[i];
    li->virt = i;
    li->phys = -1;
    li->start = li->end = -1;
    li->state = LI_NORMAL;
  }

  int nip = 0;
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];

    set_inout_interval(bb->in_regs, intervals, nip);

    for (int j = 0; j < bb->irs->len; ++j, ++nip) {
      IR *ir = bb->irs->data[j];
      VReg *regs[] = {ir->dst, ir->opr1, ir->opr2};
      for (int k = 0; k < 3; ++k) {
        VReg *reg = regs[k];
        if (reg == NULL)
          continue;
        LiveInterval *li = &intervals[reg->virt];
        if (li->start < 0)
          li->start = nip;
        if (li->end < nip)
          li->end = nip;
      }
    }

    set_inout_interval(bb->out_regs, intervals, nip);
  }

  // Sort by start, end
  LiveInterval **sorted_intervals = malloc(sizeof(LiveInterval*) * vreg_count);
  for (int i = 0; i < vreg_count; ++i)
    sorted_intervals[i] = &intervals[i];
  QSORT(sorted_intervals, vreg_count, sizeof(LiveInterval*), sort_live_interval);

  *pintervals = intervals;
  return sorted_intervals;
}

static void linear_scan_register_allocation(RegAlloc *ra, LiveInterval **sorted_intervals,
                                            int vreg_count) {
  typedef struct {
    LiveInterval **active;
    int phys_max;
    int active_count;
    unsigned short using_bits;
    unsigned short used_bits;
  } Info;

  Info ireg_info = {
    .active = ALLOCA(sizeof(LiveInterval*) * ra->phys_max),
    .phys_max = ra->phys_max,
    .active_count = 0,
    .using_bits = 0,
    .used_bits = 0,
  };
#ifndef __NO_FLONUM
  Info freg_info = {
    .active = ALLOCA(sizeof(LiveInterval*) * ra->fphys_max),
    .phys_max = ra->fphys_max,
    .active_count = 0,
    .using_bits = 0,
    .used_bits = 0,
  };
#endif
  Info *info = &ireg_info;

  for (int i = 0; i < vreg_count; ++i) {
    LiveInterval *li = sorted_intervals[i];
    if (li->state != LI_NORMAL)
      continue;
    expire_old_intervals(ireg_info.active, &ireg_info.active_count, &ireg_info.using_bits,
                         li->start);
#ifndef __NO_FLONUM
    expire_old_intervals(freg_info.active, &freg_info.active_count, &freg_info.using_bits,
                         li->start);
    if (((VReg*)ra->vregs->data[li->virt])->vtype->flag & VRTF_FLONUM)
      info = &freg_info;
    else
      info = &ireg_info;
#endif
    if (info->active_count >= info->phys_max) {
      split_at_interval(ra, info->active, info->active_count, li);
    } else {
      int regno = -1;
      for (int j = 0; j < info->phys_max; ++j) {
        if (!(info->using_bits & (1 << j))) {
          regno = j;
          break;
        }
      }
      assert(regno >= 0);
      li->phys = regno;
      info->using_bits |= 1 << regno;

      insert_active(info->active, info->active_count, li);
      ++info->active_count;
    }
    info->used_bits |= info->using_bits;
  }
  ra->used_reg_bits = ireg_info.used_bits;
#ifndef __NO_FLONUM
  ra->used_freg_bits = freg_info.used_bits;
#endif
}

static int insert_load_store_spilled(BBContainer *bbcon, Vector *vregs, const int spilled) {
  int inserted = 0;
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    Vector *irs = bb->irs;
    for (int j = 0; j < irs->len; ++j) {
      IR *ir = irs->data[j];

      int flag = 0;
      int load_size = 0;
      switch (ir->kind) {
      case IR_MOV:
      case IR_ADD:  // binops
      case IR_SUB:
      case IR_MUL:
      case IR_DIV:
      case IR_DIVU:
      case IR_MOD:
      case IR_MODU:
      case IR_PTRADD:
      case IR_BITAND:
      case IR_BITOR:
      case IR_BITXOR:
      case IR_LSHIFT:
      case IR_RSHIFT:
      case IR_CMP:
      case IR_NEG:  // unary ops
      case IR_BITNOT:
      case IR_COND:
      case IR_TEST:
      case IR_PUSHARG:
      case IR_RESULT:
        flag = 7;
        load_size = ir->size;
        break;

      case IR_CAST:
        flag = 5;
        load_size = ir->opr1->vtype->size;
        break;

      case IR_CALL:
        flag = 7;
        load_size = WORD_SIZE;
        break;

      case IR_LOAD:
      case IR_STORE:
      case IR_MEMCPY:
        flag = 7;
        load_size = WORD_SIZE;
        break;

      case IR_BOFS:
      case IR_IOFS:
      case IR_SOFS:
        flag = 4;
        break;

      default:
        continue;
      }

      assert(!((ir->opr1 != NULL && (flag & 1) != 0 && !(ir->opr1->flag & VRF_CONST) && ir->opr1->phys == spilled) &&
               (ir->opr2 != NULL && (flag & 2) != 0 && !(ir->opr2->flag & VRF_CONST) && ir->opr2->phys == spilled)));

      if (ir->opr1 != NULL && (flag & 1) != 0 &&
          !(ir->opr1->flag & VRF_CONST) && ir->opr1->phys == spilled) {
        int flag = 0;
#ifndef __NO_FLONUM
        if (ir->opr1->vtype->flag & VRTF_FLONUM)
          flag |= VRTF_FLONUM;
#endif
        vec_insert(irs, j++,
                   new_ir_load_spilled(ir->opr1, ((VReg*)vregs->data[ir->opr1->virt])->offset, load_size, flag));
        inserted |= 1;
      }

      if (ir->opr2 != NULL && (flag & 2) != 0 &&
          !(ir->opr2->flag & VRF_CONST) && ir->opr2->phys == spilled) {
        int flag = 0;
#ifndef __NO_FLONUM
        if (ir->opr2->vtype->flag & VRTF_FLONUM)
          flag |= VRTF_FLONUM;
#endif
        vec_insert(irs, j++,
                   new_ir_load_spilled(ir->opr2, ((VReg*)vregs->data[ir->opr2->virt])->offset, load_size, flag));
        inserted |= 2;
      }

      if (ir->dst != NULL && (flag & 4) != 0 &&
          !(ir->dst->flag & VRF_CONST) && ir->dst->phys == spilled) {
        assert(!(ir->dst->flag & VRF_CONST));
        int flag = 0;
#ifndef __NO_FLONUM
        if (ir->dst->vtype->flag & VRTF_FLONUM)
          flag |= VRTF_FLONUM;
#endif
        vec_insert(irs, ++j,
                   new_ir_store_spilled(ir->dst, ((VReg*)vregs->data[ir->dst->virt])->offset, ir->size, flag));
        inserted |= 4;
      }
    }
  }
  return inserted;
}

static void analyze_reg_flow(BBContainer *bbcon) {
  // Enumerate in and defined regsiters for each BB.
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    Vector *in_regs = new_vector();
    Vector *assigned_regs = new_vector();
    Vector *irs = bb->irs;
    for (int j = 0; j < irs->len; ++j) {
      IR *ir = irs->data[j];
      VReg *regs[] = {ir->opr1, ir->opr2};
      for (int k = 0; k < 2; ++k) {
        VReg *reg = regs[k];
        if (reg == NULL || reg->flag & VRF_CONST)
          continue;
        if (!vec_contains(in_regs, reg) &&
            !vec_contains(assigned_regs, reg))
          vec_push(in_regs, reg);
      }
      if (ir->dst != NULL && !vec_contains(assigned_regs, ir->dst))
        vec_push(assigned_regs, ir->dst);
    }

    bb->in_regs = in_regs;
    bb->out_regs = new_vector();
    bb->assigned_regs = assigned_regs;
  }

  // Propagate in regs to previous BB.
  bool cont;
  do {
    cont = false;
    for (int i = 0; i < bbcon->bbs->len; ++i) {
      BB *bb = bbcon->bbs->data[i];
      Vector *irs = bb->irs;

      BB *next_bbs[2];
      next_bbs[0] = bb->next;
      next_bbs[1] = NULL;

      if (irs->len > 0) {
        IR *ir = irs->data[irs->len - 1];
        if (ir->kind == IR_JMP) {
          next_bbs[1] = ir->jmp.bb;
          if (ir->jmp.cond == COND_ANY)
            next_bbs[0] = NULL;
        }
      }
      for (int j = 0; j < 2; ++j) {
        BB *next = next_bbs[j];
        if (next == NULL)
          continue;
        Vector *in_regs = next->in_regs;
        for (int k = 0; k < in_regs->len; ++k) {
          VReg *reg = in_regs->data[k];
          if (!vec_contains(bb->out_regs, reg))
            vec_push(bb->out_regs, reg);
          if (vec_contains(bb->assigned_regs, reg) ||
              vec_contains(bb->in_regs, reg))
            continue;
          vec_push(bb->in_regs, reg);
          cont = true;
        }
      }
    }
  } while (cont);
}

// Detect living registers for each instruction.
static void detect_living_registers(
  RegAlloc *ra, BBContainer *bbcon, LiveInterval **sorted_intervals, int vreg_count
) {
  UNUSED(ra);
  unsigned int living_pregs = 0;
  int nip = 0;
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    for (int j = 0; j < bb->irs->len; ++j, ++nip) {
      for (int k = 0; k < vreg_count; ++k) {
        LiveInterval *li = sorted_intervals[k];
        if (li->state != LI_NORMAL)
          continue;
        if (nip < li->start)
          break;
        int phys = li->phys;
#ifndef __NO_FLONUM
        if (((VReg*)ra->vregs->data[li->virt])->vtype->flag & VRTF_FLONUM)
          phys += ra->phys_max;
#endif
        if (nip == li->start)
          living_pregs |= 1U << phys;
        if (nip == li->end)
          living_pregs &= ~(1U << phys);
      }

      // Store living regs to IR.
      IR *ir = bb->irs->data[j];
      if (ir->kind == IR_CALL) {
        ir->call.precall->precall.living_pregs = living_pregs;
        // Store it into corresponding precall, too.
        IR *ir_precall = ir->call.precall;
        ir_precall->precall.living_pregs = living_pregs;
      }
    }
  }
}

void prepare_register_allocation(Function *func) {
  // Handle function parameters first.
  if (func->type->func.params != NULL) {
    const int DEFAULT_OFFSET = WORD_SIZE * 2;  // Return address, saved base pointer.
    assert((Scope*)func->scopes->data[0] != NULL);
    int ireg_index = is_stack_param(func->type->func.ret) ? 1 : 0;
#ifndef __NO_FLONUM
    int freg_index = 0;
#endif
    int reg_param_index = ireg_index;
    int offset = DEFAULT_OFFSET;
    for (int j = 0; j < func->type->func.params->len; ++j) {
      VarInfo *varinfo = func->type->func.params->data[j];
      VReg *vreg = varinfo->local.reg;
      // Currently, all parameters are force spilled.
      spill_vreg(func->ra, vreg);
      // stack parameters
      if (is_stack_param(varinfo->type)) {
        vreg->offset = offset = ALIGN(offset, align_size(varinfo->type));
        offset += type_size(varinfo->type);
        continue;
      }

      if (func->type->func.vaargs) {  // Variadic function parameters.
        vreg->offset = (reg_param_index - MAX_REG_ARGS) * WORD_SIZE;
      }
      ++reg_param_index;
      bool through_stack;
#ifndef __NO_FLONUM
      if (is_flonum(varinfo->type)) {
        through_stack = freg_index >= MAX_FREG_ARGS;
        ++freg_index;
      } else
#endif
      {
        through_stack = ireg_index >= MAX_REG_ARGS;
        ++ireg_index;
      }

      if (through_stack) {
        // Function argument passed through the stack.
        vreg->offset = offset;
        offset += WORD_SIZE;
      }
    }
  }

  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = (Scope*)func->scopes->data[i];
    if (scope->vars == NULL)
      continue;

    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      if (varinfo->storage & (VS_STATIC | VS_EXTERN | VS_ENUM_MEMBER))
        continue;
      VReg *vreg = varinfo->local.reg;
      if (vreg == NULL || vreg->flag & VRF_PARAM)
        continue;

      bool spill = false;
      if (vreg->flag & VRF_REF)
        spill = true;

      switch (varinfo->type->kind) {
      case TY_ARRAY:
      case TY_STRUCT:
        // Make non-primitive variable spilled.
        spill = true;
        break;
      default:
        break;
      }

      if (spill)
        spill_vreg(func->ra, vreg);
    }
  }
}

void alloc_physical_registers(RegAlloc *ra, BBContainer *bbcon) {
#ifndef __NO_FLONUM
  assert(ra->phys_max + ra->fphys_max < (int)(sizeof(ra->used_reg_bits) * CHAR_BIT));
#endif
  analyze_reg_flow(bbcon);

  int vreg_count = ra->vregs->len;
  LiveInterval *intervals;
  LiveInterval **sorted_intervals = check_live_interval(bbcon, vreg_count, &intervals);

  for (int i = 0; i < vreg_count; ++i) {
    LiveInterval *li = &intervals[i];
    VReg *vreg = ra->vregs->data[i];

    if (vreg->flag & VRF_CONST) {
      li->state = LI_CONST;
      continue;
    }

    // Force function parameter spilled.
    if (vreg->param_index >= 0) {
      spill_vreg(ra, vreg);
      li->start = 0;
      li->state = LI_SPILL;
    }
    if (vreg->phys >= ra->phys_max) {
      li->state = LI_SPILL;
      li->phys = vreg->phys;
    }
  }

  linear_scan_register_allocation(ra, sorted_intervals, vreg_count);

  // Map vreg to preg.
  for (int i = 0; i < vreg_count; ++i) {
    VReg *vreg = ra->vregs->data[i];
    LiveInterval *li = &intervals[i];
    if (li->state != LI_CONST)
      vreg->phys = intervals[vreg->virt].phys;
  }

  detect_living_registers(ra, bbcon, sorted_intervals, vreg_count);

  // Allocated spilled virtual registers onto stack.
  int frame_size = 0;
  for (int i = 0; i < vreg_count; ++i) {
    LiveInterval *li = sorted_intervals[i];
    if (li->state != LI_SPILL)
      continue;
    VReg *vreg = ra->vregs->data[li->virt];
    if (vreg->offset != 0) {  // Variadic function parameter or stack parameter.
      if (-vreg->offset > frame_size)
        frame_size = -vreg->offset;
      continue;
    }

    int size, align;
    const VRegType *vtype = vreg->vtype;
    assert(vtype != NULL);
    size = vtype->size;
    align = vtype->align;
    if (size < 1)
      size = 1;

    frame_size = ALIGN(frame_size + size, align);
    vreg->offset = -frame_size;
  }

  int spilled = SPILLED_REG_NO(ra);
  int inserted = insert_load_store_spilled(bbcon, ra->vregs, spilled);
  if (inserted != 0)
    ra->used_reg_bits |= 1 << spilled;

  ra->sorted_intervals = sorted_intervals;

  ra->frame_size = ALIGN(frame_size, 8);
}
#include "type.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "table.h"
#include "util.h"
#include "var.h"  // VarInfo

const Type tyChar =          {.kind=TY_FIXNUM, .fixnum={.kind=FX_CHAR,  .is_unsigned=false}};
const Type tyInt =           {.kind=TY_FIXNUM, .fixnum={.kind=FX_INT,   .is_unsigned=false}};
const Type tyUnsignedChar =  {.kind=TY_FIXNUM, .fixnum={.kind=FX_CHAR,  .is_unsigned=true}};
const Type tyUnsignedInt =   {.kind=TY_FIXNUM, .fixnum={.kind=FX_INT,   .is_unsigned=true}};
const Type tyEnum =          {.kind=TY_FIXNUM, .fixnum={.kind=FX_ENUM}};
const Type tyVoid =          {.kind=TY_VOID};
const Type tyVoidPtr =       {.kind=TY_PTR, .pa={.ptrof=&tyVoid}};
const Type tyBool =          {.kind=TY_FIXNUM, .fixnum={.kind=FX_INT,   .is_unsigned=false}};
const Type tySize =          {.kind=TY_FIXNUM, .fixnum={.kind=FX_LONG,  .is_unsigned=true}};
const Type tySSize =         {.kind=TY_FIXNUM, .fixnum={.kind=FX_LONG,  .is_unsigned=false}};
#ifndef __NO_FLONUM
const Type tyFloat =         {.kind=TY_FLONUM, .flonum={.kind=FL_FLOAT}};
const Type tyDouble =        {.kind=TY_FLONUM, .flonum={.kind=FL_DOUBLE}};
#endif

#define FIXNUM_TABLE(uns, qual) \
    { \
      {.kind=TY_FIXNUM, .fixnum={.kind=FX_CHAR,  .is_unsigned=uns}, .qualifier=qual}, \
      {.kind=TY_FIXNUM, .fixnum={.kind=FX_SHORT, .is_unsigned=uns}, .qualifier=qual}, \
      {.kind=TY_FIXNUM, .fixnum={.kind=FX_INT,   .is_unsigned=uns}, .qualifier=qual}, \
      {.kind=TY_FIXNUM, .fixnum={.kind=FX_LONG,  .is_unsigned=uns}, .qualifier=qual}, \
      {.kind=TY_FIXNUM, .fixnum={.kind=FX_LLONG, .is_unsigned=uns}, .qualifier=qual}, \
    }

static const Type kFixnumTypeTable[2][4][FX_LLONG + 1] = {
  {
    FIXNUM_TABLE(false, 0), FIXNUM_TABLE(false, 1), FIXNUM_TABLE(false, 2), FIXNUM_TABLE(false, 3),
  },
  {
    FIXNUM_TABLE(true, 0), FIXNUM_TABLE(true, 1), FIXNUM_TABLE(true, 2), FIXNUM_TABLE(true, 3),
  },
};
#undef FIXNUM_TABLE

size_t fixnum_size_table[]  = {1, 2, 4, 8, 8, 4};
int    fixnum_align_table[] = {1, 2, 4, 8, 8, 4};

#ifndef __NO_FLONUM
size_t flonum_size_table[]  = {4, 8};
int    flonum_align_table[] = {4, 8};
#endif

void set_fixnum_size(enum FixnumKind kind, size_t size, int align) {
  fixnum_size_table[kind] = size;
  fixnum_align_table[kind] = align;
}

static void calc_struct_size(StructInfo *sinfo) {
  assert(sinfo != NULL);
  if (sinfo->size >= 0)
    return;

  size_t size = 0;
  size_t maxsize = 0;
  int max_align = 1;

  for (int i = 0, len = sinfo->members->len; i < len; ++i) {
    VarInfo *member = sinfo->members->data[i];
    size_t sz = type_size(member->type);
    int align = align_size(member->type);
    size = ALIGN(size, align);
    member->struct_member.offset = size;
    if (!sinfo->is_union) {
      size += sz;
    } else {
      if (maxsize < sz)
        maxsize = sz;
    }
    if (max_align < align)
      max_align = align;
  }

  if (sinfo->is_union)
    size = maxsize;
  size = ALIGN(size, max_align);
  sinfo->size = size;
  sinfo->align = max_align;
}

size_t type_size(const Type *type) {
  switch (type->kind) {
  case TY_VOID:
    return 0;
  case TY_FIXNUM:
    return fixnum_size_table[type->fixnum.kind];
#ifndef __NO_FLONUM
  case TY_FLONUM:
    return flonum_size_table[type->flonum.kind];
#endif
  case TY_PTR:
    return fixnum_size_table[FX_LONG];
  case TY_ARRAY:
    assert(type->pa.length != (size_t)-1);
    return type_size(type->pa.ptrof) * type->pa.length;
  case TY_FUNC:
    return 1;
  case TY_STRUCT:
    calc_struct_size(type->struct_.info);
    return type->struct_.info->size;
  default:
    assert(false);
    return 1;
  }
}

int align_size(const Type *type) {
  switch (type->kind) {
  case TY_VOID:
    return 1;  // Just in case.
  case TY_FIXNUM:
    return fixnum_align_table[type->fixnum.kind];
#ifndef __NO_FLONUM
  case TY_FLONUM:
    return flonum_align_table[type->fixnum.kind];
#endif
  case TY_PTR:
    return fixnum_align_table[FX_LONG];
  case TY_FUNC:
    return 1;
  case TY_ARRAY:
    return align_size(type->pa.ptrof);
  case TY_STRUCT:
    calc_struct_size(type->struct_.info);
    return type->struct_.info->align;
  default:
    assert(false);
    return 1;
  }
}

bool is_fixnum(enum TypeKind kind) {
  return kind == TY_FIXNUM;
}

bool is_number(const Type *type) {
#ifndef __NO_FLONUM
  if (is_flonum(type))
    return true;
#endif
  return is_fixnum(type->kind);
}

#ifndef __NO_FLONUM
bool is_flonum(const Type *type) {
  return type->kind == TY_FLONUM;
}
#endif

bool is_char_type(const Type *type) {
  return type->kind == TY_FIXNUM && type->fixnum.kind == FX_CHAR;
}

bool is_void_ptr(const Type *type) {
  return type->kind == TY_PTR && type->pa.ptrof->kind == TY_VOID;
}

bool ptr_or_array(const Type *type) {
  return type->kind == TY_PTR || type->kind == TY_ARRAY;
}

const Type *get_fixnum_type(enum FixnumKind kind, bool is_unsigned, int qualifier) {
  assert(kind != FX_ENUM);
  return &kFixnumTypeTable[is_unsigned][qualifier & 3][kind];
}

Type *ptrof(const Type *type) {
  Type *ptr = malloc(sizeof(*ptr));
  ptr->kind = TY_PTR;
  ptr->qualifier = 0;
  ptr->pa.ptrof = type;
  return ptr;
}

const Type *array_to_ptr(const Type *type) {
  assert(type->kind == TY_ARRAY);
  return ptrof(type->pa.ptrof);
}

Type *arrayof(const Type *type, size_t length) {
  Type *arr = malloc(sizeof(*arr));
  arr->kind = TY_ARRAY;
  arr->qualifier = 0;
  arr->pa.ptrof = type;
  arr->pa.length = length;
  return arr;
}

Type *new_func_type(const Type *ret, const Vector *params, const Vector *param_types, bool vaargs) {
  Type *f = malloc(sizeof(*f));
  f->kind = TY_FUNC;
  f->qualifier = 0;
  f->func.ret = ret;
  f->func.vaargs = vaargs;
  f->func.params = params;
  f->func.param_types = param_types;
  return f;
}

const Type *qualified_type(const Type *type, int additional) {
  int modified = type->qualifier | additional;
  if (modified == type->qualifier)
    return type;
  Type *ctype = malloc(sizeof(*ctype));
  memcpy(ctype, type, sizeof(*ctype));
  ctype->qualifier = modified;
  return ctype;
}

// Struct

StructInfo *create_struct_info(Vector *members, bool is_union) {
  StructInfo *sinfo = malloc(sizeof(*sinfo));
  sinfo->members = members;
  sinfo->is_union = is_union;
  sinfo->size = -1;
  sinfo->align = 0;
  calc_struct_size(sinfo);
  return sinfo;
}

Type *create_struct_type(StructInfo *sinfo, const Name *name, int qualifier) {
  Type *type = malloc(sizeof(*type));
  type->kind = TY_STRUCT;
  type->qualifier = qualifier;
  type->struct_.name = name;
  type->struct_.info = sinfo;
  return type;
}

// Enum

Type *create_enum_type(const Name *name) {
  Type *type = malloc(sizeof(*type));
  type->kind = TY_FIXNUM;
  type->qualifier = 0;
  type->fixnum.kind = FX_ENUM;
  type->fixnum.is_unsigned = false;
  type->fixnum.enum_.ident = name;
  return type;
}

#if 0
void dump_type(FILE *fp, const Type *type) {
  switch (type->kind) {
  case TY_VOID: fprintf(fp, "void"); break;
  case TY_FIXNUM:
    switch (type->fixnum.kind) {
    case FX_CHAR:  fprintf(fp, "char"); break;
    case FX_SHORT: fprintf(fp, "short"); break;
    case FX_INT:   fprintf(fp, "int"); break;
    case FX_LONG:  fprintf(fp, "long"); break;
    case FX_ENUM:  fprintf(fp, "enum"); break;
    default: assert(false); break;
    }
    break;
  case TY_PTR: dump_type(fp, type->pa.ptrof); fprintf(fp, "*"); break;
  case TY_ARRAY: dump_type(fp, type->pa.ptrof); fprintf(fp, "[%d]", (int)type->pa.length); break;
  default: assert(false); break;
  }
}
#endif

bool same_type(const Type *type1, const Type *type2) {
  for (;;) {
    if (type1->kind != type2->kind)
      return false;

    switch (type1->kind) {
    case TY_VOID:
      return true;
    case TY_FIXNUM:
      return type1->fixnum.kind == type2->fixnum.kind &&
          type1->fixnum.is_unsigned == type2->fixnum.is_unsigned;
#ifndef __NO_FLONUM
    case TY_FLONUM:
      return type1->flonum.kind == type2->flonum.kind;
#endif
    case TY_ARRAY:
      if (type1->pa.length != type2->pa.length)
        return false;
      // Fallthrough
    case TY_PTR:
      type1 = type1->pa.ptrof;
      type2 = type2->pa.ptrof;
      continue;
    case TY_FUNC:
      if (!same_type(type1->func.ret, type2->func.ret) || type1->func.vaargs != type2->func.vaargs)
        return false;
      if (type1->func.param_types == NULL && type2->func.param_types == NULL)
        return true;
      if (type1->func.param_types == NULL || type2->func.param_types == NULL ||
          type1->func.param_types->len != type2->func.param_types->len)
        return false;
      for (int i = 0, len = type1->func.param_types->len; i < len; ++i) {
        const Type *t1 = (const Type*)type1->func.param_types->data[i];
        const Type *t2 = (const Type*)type2->func.param_types->data[i];
        if (!same_type(t1, t2))
          return false;
      }
      return true;
    case TY_STRUCT:
      {
        if (type1->struct_.info != NULL) {
          if (type2->struct_.info != NULL)
            return type1->struct_.info == type2->struct_.info;
        }
        if (type1->struct_.name == NULL || type2->struct_.name == NULL)
          return false;
        return equal_name(type1->struct_.name, type2->struct_.name);
      }
    }
  }
}

bool can_cast(const Type *dst, const Type *src, bool zero, bool is_explicit) {
  if (same_type(dst, src))
    return true;

  if (dst->kind == TY_VOID)
    return src->kind == TY_VOID || is_explicit;
  if (src->kind == TY_VOID)
    return false;

  switch (dst->kind) {
  case TY_FIXNUM:
    switch (src->kind) {
    case TY_FIXNUM:
#ifndef __NO_FLONUM
    case TY_FLONUM:
#endif
      return true;
    case TY_PTR:
    case TY_ARRAY:
    case TY_FUNC:
      if (is_explicit) {
        // TODO: Check sizeof(long) is same as sizeof(ptr)
        return true;
      }
      break;
    default:
      break;
    }
    break;
#ifndef __NO_FLONUM
  case TY_FLONUM:
    switch (src->kind) {
    case TY_FIXNUM:
      return true;
    case TY_FLONUM:
      return true;
    default:
      break;
    }
    break;
#endif
  case TY_PTR:
    switch (src->kind) {
    case TY_FIXNUM:
      if (zero)  // Special handling for 0 to pointer.
        return true;
      if (is_explicit)
        return true;
      break;
    case TY_PTR:
      if (is_explicit)
        return true;
      // void* is interchangable with any pointer type.
      if (dst->pa.ptrof->kind == TY_VOID || src->pa.ptrof->kind == TY_VOID)
        return true;
      if (src->pa.ptrof->kind == TY_FUNC)
        return can_cast(dst, src->pa.ptrof, zero, is_explicit);
      break;
    case TY_ARRAY:
      if (is_explicit)
        return true;
      if (same_type(dst->pa.ptrof, src->pa.ptrof) ||
          can_cast(dst, ptrof(src->pa.ptrof), zero, is_explicit))
        return true;
      break;
    case TY_FUNC:
      if (is_explicit)
        return true;
      switch (dst->pa.ptrof->kind) {
      case TY_FUNC:
        {
          const Type *ftype = dst->pa.ptrof;
          return (same_type(ftype, src) ||
                  (ftype->func.param_types == NULL || src->func.param_types == NULL));
        }
      case TY_VOID:
        return true;
      default:
        break;
      }
      break;
    default:  break;
    }
    break;
  case TY_ARRAY:
    switch (src->kind) {
    case TY_PTR:
      if (is_explicit && same_type(dst->pa.ptrof, src->pa.ptrof))
        return true;
      // Fallthrough
    case TY_ARRAY:
      if (is_explicit)
        return true;
      break;
    default:  break;
    }
    break;
  default:
    break;
  }
  return false;
}

//

typedef struct PrintTypeChain PrintTypeChain;
struct PrintTypeChain {
  struct PrintTypeChain *parent;
  void (*callback)(FILE *fp, const Type *type);
  const Type *type;
};

static void call_print_type_chain(const PrintTypeChain *chain, FILE *fp) {
  for (; chain != NULL; chain = chain->parent)
    (*chain->callback)(fp, chain->type);
}

static void print_func_params(FILE *fp, const Type *type) {
  assert(type->kind == TY_FUNC);
  fprintf(fp, "(");
  if (type->func.param_types != NULL) {
    int param_count = type->func.param_types->len;
    if (param_count == 0 && !type->func.vaargs) {
      fprintf(fp, "void");
    } else {
      for (int i = 0; i < param_count; ++i) {
        if (i > 0)
          fprintf(fp, ", ");
        print_type(fp, type->func.param_types->data[i]);
      }
      if (type->func.vaargs) {
        if (param_count > 0)
          fprintf(fp, ", ");
        fprintf(fp, "...");
      }
    }
  }
  fprintf(fp, ")");
}

static void print_ptr_type(FILE *fp, const Type *_type) {
  UNUSED(_type);
  fprintf(fp, "*");
}

static void print_nested_ptr_type(FILE *fp, const Type *type) {
  fprintf(fp, "(");
  for (const Type *p = type; p->kind == TY_PTR; p = p->pa.ptrof)
    fprintf(fp, "*");
}

static void print_nested_ptr_type2(FILE *fp, const Type *_type) {
  UNUSED(_type);
  fprintf(fp, ")");
}

static void print_array_type(FILE *fp, const Type *type) {
  for (; type->kind == TY_ARRAY; type = type->pa.ptrof) {
    if (type->pa.length != (size_t)-1)
      fprintf(fp, "[%zu]", type->pa.length);
    else
      fprintf(fp, "[]");
  }
}

void print_type_recur(FILE *fp, const Type *type, PrintTypeChain *parent) {
  switch (type->kind) {
  case TY_VOID:
    fprintf(fp, "void");
    call_print_type_chain(parent, fp);
    break;
  case TY_FIXNUM:
    switch (type->fixnum.kind) {
    case FX_CHAR:  fprintf(fp, "char"); break;
    case FX_SHORT: fprintf(fp, "short"); break;
    case FX_INT:   fprintf(fp, "int"); break;
    case FX_LONG:  fprintf(fp, "long"); break;
    case FX_ENUM:  fprintf(fp, "enum"); break;
    default: assert(false); break;
    }
    call_print_type_chain(parent, fp);
    break;
#ifndef __NO_FLONUM
  case TY_FLONUM:
    switch (type->flonum.kind) {
    case FL_FLOAT:  fprintf(fp, "float"); break;
    case FL_DOUBLE: fprintf(fp, "double"); break;
    default: assert(false); break;
    }
    call_print_type_chain(parent, fp);
    break;
#endif
  case TY_PTR:
    {
      const Type *nestedtype = NULL;
      for (const Type *p = type; p->kind == TY_PTR; p = p->pa.ptrof) {
        const Type *ptrof = p->pa.ptrof;
        if (ptrof->kind == TY_FUNC || ptrof->kind == TY_ARRAY) {
          nestedtype = ptrof;
          break;
        }
      }
      if (nestedtype != NULL) {
        PrintTypeChain last = {
          NULL,
          print_nested_ptr_type2,
          NULL,
        };
        if (parent != NULL) {
          for (PrintTypeChain *p = parent;; p = p->parent) {
            if (p->parent == NULL) {
              p->parent = &last;
              break;
            }
          }
        } else {
          parent = &last;
        }

        PrintTypeChain chain = {
          parent,
          print_nested_ptr_type,
          type,
        };
        switch (nestedtype->kind) {
        case TY_FUNC:
          print_type_recur(fp, nestedtype->func.ret, &chain);
          print_func_params(fp, nestedtype);
          break;
        case TY_ARRAY:
          print_type_recur(fp, nestedtype->pa.ptrof, &chain);
          print_array_type(fp, nestedtype);
          break;
        default: assert(false); break;
        }
      } else {
        PrintTypeChain chain = {
          parent,
          print_ptr_type,
          NULL,
        };
        print_type_recur(fp, type->pa.ptrof, &chain);
      }
    }
    break;
  case TY_ARRAY:
    {
      PrintTypeChain chain = {
        parent,
        print_array_type,
        type,
      };
      const Type *nonarray;
      for (nonarray = type; nonarray->kind == TY_ARRAY; nonarray = nonarray->pa.ptrof)
        ;
      print_type_recur(fp, nonarray, &chain);
    }
    break;
  case TY_FUNC:
    {
      // No parenthesis.
      PrintTypeChain chain = {
        parent,
        print_func_params,
        type,
      };
      print_type_recur(fp, type->func.ret, &chain);
    }
    break;
  case TY_STRUCT:
    if (type->struct_.name != NULL) {
      fprintf(fp, "struct %.*s", type->struct_.name->bytes, type->struct_.name->chars);
    } else {
      fprintf(fp, "struct (anonymous)");
    }
    call_print_type_chain(parent, fp);
    break;
  }
}

void print_type(FILE *fp, const Type *type) {
  print_type_recur(fp, type, NULL);
}
#include "var.h"

#include <assert.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "lexer.h"
#include "table.h"
#include "type.h"
#include "util.h"

static VarInfo *define_global(const Name *name, const Type *type, int storage, const Token *ident);

int var_find(const Vector *vars, const Name *name) {
  for (int i = 0, len = vars->len; i < len; ++i) {
    VarInfo *info = vars->data[i];
    if (info->name != NULL && equal_name(info->name, name))
      return i;
  }
  return -1;
}

VarInfo *var_add(Vector *vars, const Name *name, const Type *type, int storage,
                 const Token *ident) {
  if (name != NULL) {
    int idx = var_find(vars, name);
    if (idx >= 0)
      parse_error(ident, "`%.*s' already defined", name->bytes, name->chars);
  }

  VarInfo *varinfo = calloc(1, sizeof(*varinfo));
  varinfo->name = name;
  varinfo->type = type;
  varinfo->storage = storage;
  if (storage & VS_STATIC)
    varinfo->static_.gvar = define_global(alloc_label(), type, storage, NULL);
  vec_push(vars, varinfo);
  return varinfo;
}

// Global

Scope *global_scope;

void init_global(void) {
  global_scope = calloc(1, sizeof(*global_scope));
  global_scope->parent = NULL;
  global_scope->vars = new_vector();
}

static VarInfo *define_global(const Name *name, const Type *type, int storage, const Token *ident) {
  assert(name != NULL);
  VarInfo *varinfo = scope_find(global_scope, name, NULL);
  if (varinfo != NULL) {
    if (!(varinfo->storage & VS_EXTERN)) {
      if (!(storage & VS_EXTERN))
        parse_error(ident, "`%.*s' already defined", name->bytes, name->chars);
      return varinfo;
    }
    varinfo->name = name;
    varinfo->type = type;
    varinfo->storage = storage;
    varinfo->global.init = NULL;
  } else {
    // `static' is different meaning for global and local variable.
    varinfo = var_add(global_scope->vars, name, type, storage & ~VS_STATIC, ident);
    varinfo->storage = storage;
  }
  return varinfo;
}

// Scope

Scope *new_scope(Scope *parent, Vector *vars) {
  Scope *scope = malloc(sizeof(*scope));
  scope->parent = parent;
  scope->vars = vars;
  scope->struct_table = NULL;
  scope->typedef_table = NULL;
  scope->enum_table = NULL;
  return scope;
}

bool is_global_scope(Scope *scope) {
  assert(scope->parent != NULL || scope == global_scope);  // Global scope is only one.
  return scope->parent == NULL;
}

VarInfo *scope_find(Scope *scope, const Name *name, Scope **pscope) {
  VarInfo *varinfo = NULL;
  for (;; scope = scope->parent) {
    if (scope == NULL)
      break;
    if (scope->vars != NULL) {
      int idx = var_find(scope->vars, name);
      if (idx >= 0) {
        varinfo = scope->vars->data[idx];
        break;
      }
    }
  }
  if (pscope != NULL)
    *pscope = scope;
  return varinfo;
}

VarInfo *scope_add(Scope *scope, const Token *ident, const Type *type, int storage) {
  assert(ident != NULL);
  if (is_global_scope(scope))
    return define_global(ident->ident, type, storage, ident);

  if (scope->vars == NULL)
    scope->vars = new_vector();
  return var_add(scope->vars, ident->ident, type, storage, ident);
}

StructInfo *find_struct(Scope *scope, const Name *name, Scope **pscope) {
  for (; scope != NULL; scope = scope->parent) {
    if (scope->struct_table == NULL)
      continue;
    StructInfo *sinfo = table_get(scope->struct_table, name);
    if (sinfo != NULL) {
      if (pscope != NULL)
        *pscope = scope;
      return sinfo;
    }
  }
  return NULL;
}

void define_struct(Scope *scope, const Name *name, StructInfo *sinfo) {
  if (scope->struct_table == NULL) {
    scope->struct_table = malloc(sizeof(*scope->struct_table));
    table_init(scope->struct_table);
  }
  table_put(scope->struct_table, name, sinfo);
}

const Type *find_typedef(Scope *scope, const Name *name, Scope **pscope) {
  for (; scope != NULL; scope = scope->parent) {
    if (scope->typedef_table == NULL)
      continue;
    const Type *type = table_get(scope->typedef_table, name);
    if (type != NULL) {
      if (pscope != NULL)
        *pscope = scope;
      return type;
    }
  }
  return NULL;
}

bool add_typedef(Scope *scope, const Name *name, const Type *type) {
  if (scope->typedef_table != NULL) {
    if (table_get(scope->typedef_table, name) != NULL)
      return false;
  } else {
    scope->typedef_table = malloc(sizeof(*scope->typedef_table));
    table_init(scope->typedef_table);
  }
  table_put(scope->typedef_table, name, (void*)type);
  return true;
}

Type *find_enum(Scope *scope, const Name *name) {
  for (; scope != NULL; scope = scope->parent) {
    if (scope->enum_table == NULL)
      continue;
    Type *type = table_get(scope->enum_table, name);
    if (type != NULL)
      return type;
  }
  return NULL;
}

Type *define_enum(Scope *scope, const Name *name) {
  Type *type = create_enum_type(name);
  if (name != NULL) {
    if (scope->enum_table == NULL) {
      scope->enum_table = malloc(sizeof(*scope->enum_table));
      table_init(scope->enum_table);
    }
    table_put(scope->enum_table, name, type);
  }
  return type;
}

// Misc.

void ensure_struct(Type *type, const Token *token, Scope *scope) {
  assert(type->kind == TY_STRUCT);
  if (type->struct_.info == NULL) {
    StructInfo *sinfo = find_struct(scope, type->struct_.name, NULL);
    if (sinfo == NULL)
      parse_error(token, "Accessing unknown struct(%.*s)'s member", type->struct_.name->bytes,
                  type->struct_.name->chars);
    type->struct_.info = sinfo;
  }

  // Recursively.
  StructInfo *sinfo = type->struct_.info;
  for (int i = 0; i < sinfo->members->len; ++i) {
    VarInfo *varinfo = sinfo->members->data[i];
    if (varinfo->type->kind == TY_STRUCT)
      ensure_struct((Type*)varinfo->type, token, scope);
  }
}

const VarInfo *search_from_anonymous(const Type *type, const Name *name, const Token *ident,
                                     Vector *stack) {
  assert(type->kind == TY_STRUCT);
  const Vector *members = type->struct_.info->members;
  for (int i = 0, len = members->len; i < len; ++i) {
    const VarInfo *member = members->data[i];
    if (member->name != NULL) {
      if (equal_name(member->name, name)) {
        vec_push(stack, (void*)(long)i);
        return member;
      }
    } else if (member->type->kind == TY_STRUCT) {
      vec_push(stack, (void*)(intptr_t)i);
      const VarInfo *submember = search_from_anonymous(member->type, name, ident, stack);
      if (submember != NULL)
        return submember;
      vec_pop(stack);
    }
  }
  return NULL;
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
