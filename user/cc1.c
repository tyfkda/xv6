#include "lexer.h"

#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdlib.h>  // malloc
#include <string.h>
#include <sys/types.h>  // ssize_t

#include "util.h"

#define MAX_LOOKAHEAD  (2)

static const struct {
  const char *str;
  enum TokenType type;
} kReservedWords[] = {
  { "if", TK_IF },
  { "else", TK_ELSE },
  { "switch", TK_SWITCH },
  { "case", TK_CASE },
  { "default", TK_DEFAULT },
  { "do", TK_DO },
  { "while", TK_WHILE },
  { "for", TK_FOR },
  { "break", TK_BREAK },
  { "continue", TK_CONTINUE },
  { "goto", TK_GOTO },
  { "return", TK_RETURN },
  { "void", TK_KWVOID },
  { "char", TK_KWCHAR },
  { "short", TK_KWSHORT },
  { "int", TK_KWINT },
  { "long", TK_KWLONG },
  { "const", TK_KWCONST },
  { "unsigned", TK_UNSIGNED },
  { "static", TK_STATIC },
  { "extern", TK_EXTERN },
  { "struct", TK_STRUCT },
  { "union", TK_UNION },
  { "enum", TK_ENUM },
  { "sizeof", TK_SIZEOF },
  { "typedef", TK_TYPEDEF },
};

static const struct {
  const char ident[4];
  enum TokenType type;
} kMultiOperators[] = {
  // Must align from long to short keyword.
  {"<<=", TK_LSHIFT_ASSIGN},
  {">>=", TK_RSHIFT_ASSIGN},
  {"...", TK_DOTDOTDOT},
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
};

static const char kSingleOperators[] = "+-*/%&!(){}[]<>=^|:;,.?";

typedef struct {
  FILE *fp;
  const char *filename;
  Line* line;
  const char *p;
  Token *fetched[MAX_LOOKAHEAD];
  int idx;
  int lineno;
} Lexer;

static Lexer lexer;

void show_error_line(const char *line, const char *p) {
  fprintf(stderr, "%s\n", line);
  size_t pos = p - line;
  if (pos <= strlen(line)) {
    for (size_t i = 0; i < pos; ++i)
      fputc(line[i] == '\t' ? '\t' : ' ', stderr);
    fprintf(stderr, "^\n");
  }
}

void lex_error(const char *p, const char* fmt, ...) {
  fprintf(stderr, "%s(%d): ", lexer.filename, lexer.lineno);

  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fprintf(stderr, "\n");

  show_error_line(lexer.line->buf, p);

  exit(1);
}

void parse_error(const Token *token, const char* fmt, ...) {
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

  if (token != NULL && token->line != NULL && token->begin != NULL)
    show_error_line(token->line->buf, token->begin);

  exit(1);
}

static Token *alloc_token(enum TokenType type, const char *begin, const char *end) {
  Token *token = malloc(sizeof(*token));
  token->type = type;
  token->line = lexer.line;
  token->begin = begin;
  token->end = end;
  return token;
}

Token *alloc_ident(const char *ident, const char *begin, const char *end) {
  Token *tok = alloc_token(TK_IDENT, begin, end);
  tok->u.ident = ident;
  return tok;
}

static enum TokenType reserved_word(const char *word) {
  for (int i = 0; i < (int)(sizeof(kReservedWords) / sizeof(*kReservedWords)); ++i) {
    if (strcmp(kReservedWords[i].str, word) == 0)
      return kReservedWords[i].type;
  }
  return -1;
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

void init_lexer(FILE *fp, const char *filename) {
  lexer.fp = fp;
  lexer.filename = filename;
  lexer.line = NULL;
  lexer.p = "";
  lexer.idx = -1;
  lexer.lineno = 0;
}

void init_lexer_string(const char *line, const char *filename, int lineno) {
  Line *p = malloc(sizeof(*line));
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
  return lexer.p;
}

static int scan_linemarker(const char *line, long *pnum, char **pfn, int *pflag) {
  const char *p = line;
  if (p[0] != '#' || p[1] != ' ')
    return 0;
  p += 2;

  int n = 0;
  const char *next = p;
  long num = strtol(next, (char**)&next, 10);
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
  ssize_t len;
  for (;;) {
    len = getline_(&line, &capa, lexer.fp, 0);
    if (len == EOF) {
      lexer.p = NULL;
      lexer.line = NULL;
      return;
    }
    while (len > 0 && line[len - 1] == '\\') {  // Continue line.
      len = getline_(&line, &capa, lexer.fp, len - 1);  // -1 for overwrite on '\'
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

  Line *p = malloc(sizeof(*line));
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
    char c = *p++;
    if (c == '\0') {
      read_next_line();
      p = lexer.p;
      if (p == NULL)
        return NULL;
      continue;
    } else if (isspace(c)) {
      continue;
    } else if (c == '/') {
      if (*p == '*') {
        p = skip_block_comment(p + 1);
        if (p == NULL)
          return NULL;
        continue;
      } else if (*p == '/') {
        p = skip_line_comment();
        if (p == NULL)
          return NULL;
        continue;
      }
    }
    break;
  }
  return p - 1;
}

static Token *read_num(const char **pp) {
  const char *start = *pp, *p = start;
  int base = 10;
  if (*p == '0') {
    base = 8;
    ++p;
    if (*p == 'x') {
      base = 16;
      ++p;
    }
  }
  long val = strtol(p, (char**)pp, base);
  if (*pp == p && base == 16)
    lex_error(p, "Illegal literal");
  Token *tok;
  enum TokenType tt = TK_INTLIT;
  if (**pp == 'L') {
    tt = TK_LONGLIT;
    ++(*pp);
  }
  tok = alloc_token(tt, start, *pp);
  tok->u.value = val;
  return tok;
}

char *read_ident(const char **pp) {
  const char *p = *pp;
  if (!isalpha(*p) && *p != '_')
    return NULL;

  const char *q;
  for (q = p + 1; ; ++q) {
    if (!(isalnum(*q) || *q == '_'))
      break;
  }
  *pp = q;
  return strndup_(p, q - p);
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
  tok->u.value = c;
  *pp = p;
  return tok;
}

static Token *read_string(const char **pp) {
  const int ADD = 16;
  const char *p = *pp;
  const char *begin = p++;  // Skip first '"'
  size_t capa = 16, size = 0;
  char *str = malloc(capa);
  for (;;) {
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

    // Continue string literal when next character is '"'
    p = skip_whitespace_or_comment(p);
    if (p == NULL || *p != '"')
      break;
    ++p;
  }
  assert(size < capa);
  str[size++] = '\0';
  Token *tok = alloc_token(TK_STR, begin, p);
  tok->u.str.buf = str;
  tok->u.str.size = size;
  *pp = p;
  return tok;
}

static Token *get_op_token(const char **pp) {
  const char *p = *pp;
  if (isalnum(*p))
    return NULL;

  Token *tok = NULL;
  for (int i = 0, n = sizeof(kMultiOperators) / sizeof(*kMultiOperators); i < n; ++i) {
    const char *ident = kMultiOperators[i].ident;
    size_t len = strlen(ident);
    if (strncmp(p, ident, len) == 0) {
      const char *q = p + len;
      tok = alloc_token(kMultiOperators[i].type, p, q);
      p = q;
      break;
    }
  }

  if (tok == NULL) {
    if (strchr(kSingleOperators, *p) != NULL) {
      tok = alloc_token((enum TokenType)*p, p, p + 1);
      ++p;
    }
  }

  if (tok != NULL)
    *pp = p;

  return tok;
}

static Token *get_token(void) {
  static Token kEofToken = {.type = TK_EOF};

  const char *p = lexer.p;
  if (p == NULL)
    return &kEofToken;

  p = skip_whitespace_or_comment(p);
  if (p == NULL)
    return &kEofToken;

  Token *tok = NULL;
  const char *begin = p;
  char *ident = read_ident(&p);
  if (ident != NULL) {
    enum TokenType word = reserved_word(ident);
    if ((int)word != -1) {
      free(ident);
      tok = alloc_token(word, begin, p);
    } else {
      tok = alloc_ident(ident, begin, p);
    }
  } else if ((tok = get_op_token(&p)) != NULL) {
    // Ok.
  } else if (isdigit(*p)) {
    tok = read_num(&p);
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

Token *consume(enum TokenType type) {
  Token *tok = fetch_token();
  if (tok->type != type && (int)type != -1)
    return NULL;
  if (tok->type != TK_EOF)
    --lexer.idx;
  return tok;
}

void unget_token(Token *token) {
  ++lexer.idx;
  assert(lexer.idx < MAX_LOOKAHEAD);
  lexer.fetched[lexer.idx] = token;
}
#include "type.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "util.h"
#include "lexer.h"

const Type tyChar =  {.type=TY_NUM, .u={.num={.type=NUM_CHAR}}};
const Type tyShort = {.type=TY_NUM, .u={.num={.type=NUM_SHORT}}};
const Type tyInt =   {.type=TY_NUM, .u={.num={.type=NUM_INT}}};
const Type tyLong =  {.type=TY_NUM, .u={.num={.type=NUM_LONG}}};
const Type tyEnum =  {.type=TY_NUM, .u={.num={.type=NUM_ENUM}}};
const Type tyVoid =  {.type=TY_VOID};

bool is_number(enum eType type) {
  return type == TY_NUM;
}

bool is_char_type(const Type *type) {
  return type->type == TY_NUM && type->u.num.type == NUM_CHAR;
}

bool is_void_ptr(const Type *type) {
  return type->type == TY_PTR && type->u.pa.ptrof->type == TY_VOID;
}

bool same_type(const Type *type1, const Type *type2) {
  for (;;) {
    if (type1->type != type2->type)
      return false;

    switch (type1->type) {
    case TY_VOID:
      return true;
    case TY_NUM:
      return type1->u.num.type == type2->u.num.type;
    case TY_ARRAY:
    case TY_PTR:
      type1 = type1->u.pa.ptrof;
      type2 = type2->u.pa.ptrof;
      continue;
    case TY_FUNC:
      if (!same_type(type1->u.func.ret, type2->u.func.ret) ||
          type1->u.func.param_types->len != type2->u.func.param_types->len)
        return false;
      for (int i = 0, len = type1->u.func.param_types->len; i < len; ++i) {
        const Type *t1 = (const Type*)type1->u.func.param_types->data[i];
        const Type *t2 = (const Type*)type2->u.func.param_types->data[i];
        if (!same_type(t1, t2))
          return false;
      }
      return true;
    case TY_STRUCT:
      {
        if (type1->u.struct_.info != NULL) {
          if (type2->u.struct_.info != NULL)
            return type1->u.struct_.info == type2->u.struct_.info;
          const Type *tmp = type1;
          type1 = type2;
          type2 = tmp;
        } else if (type2->u.struct_.info == NULL) {
          return strcmp(type1->u.struct_.name, type2->u.struct_.name) == 0;
        }
        // Find type1 from name.
        StructInfo *sinfo = find_struct(type1->u.struct_.name);
        if (sinfo == NULL)
          return false;
        return sinfo == type2->u.struct_.info;
      }
    }
  }
}

Type* ptrof(const Type *type) {
  Type *ptr = malloc(sizeof(*ptr));
  ptr->type = TY_PTR;
  ptr->u.pa.ptrof = type;
  return ptr;
}

const Type *array_to_ptr(const Type *type) {
  if (type->type != TY_ARRAY)
    return type;
  return ptrof(type->u.pa.ptrof);
}

Type* arrayof(const Type *type, size_t length) {
  Type *arr = malloc(sizeof(*arr));
  arr->type = TY_ARRAY;
  arr->u.pa.ptrof = type;
  arr->u.pa.length = length;
  return arr;
}

Type* new_func_type(const Type *ret, Vector *param_types, bool vaargs) {
  Type *f = malloc(sizeof(*f));
  f->type = TY_FUNC;
  f->u.func.ret = ret;
  f->u.func.vaargs = vaargs;
  f->u.func.param_types = param_types;
  return f;
}

// Struct

Map *struct_map;

StructInfo *find_struct(const char *name) {
  return (StructInfo*)map_get(struct_map, name);
}

void define_struct(const char *name, StructInfo *sinfo) {
  map_put(struct_map, name, sinfo);
}

// Enum

Map *enum_map;
Map *enum_value_map;

Type *find_enum(const char *name) {
  return map_get(enum_map, name);
}

Type *define_enum(const Token *ident) {
  Type *type = malloc(sizeof(*type));
  type->type = TY_NUM;
  type->u.num.type = NUM_ENUM;
  type->u.num.enum_.ident = ident;
  type->u.num.enum_.members = new_vector();

  if (ident != NULL) {
    map_put(enum_map, ident->u.ident, type);
  }

  return type;
}

void add_enum_member(Type *type, const Token *ident, int value) {
  assert(type->type == TY_NUM && type->u.num.type == NUM_ENUM);
  EnumMember *member = malloc(sizeof(*member));
  member->ident = ident;
  member->value = value;
  vec_push(type->u.num.enum_.members, member);

  map_put(enum_value_map, ident->u.ident, (void*)(intptr_t)value);
}

bool find_enum_value(const char *name, intptr_t *output) {
  return map_try_get(enum_value_map, name, (void**)output);
}

#if 0
void dump_type(FILE *fp, const Type *type) {
  switch (type->type) {
  case TY_VOID: fprintf(fp, "void"); break;
  case TY_NUM:
    switch (type->u.num.type) {
    case NUM_CHAR:  fprintf(fp, "char"); break;
    case NUM_SHORT: fprintf(fp, "short"); break;
    case NUM_INT:   fprintf(fp, "int"); break;
    case NUM_LONG:  fprintf(fp, "long"); break;
    case NUM_ENUM:  fprintf(fp, "enum"); break;
    default: assert(false); break;
    }
    break;
  case TY_PTR: dump_type(fp, type->u.pa.ptrof); fprintf(fp, "*"); break;
  case TY_ARRAY: dump_type(fp, type->u.pa.ptrof); fprintf(fp, "[%d]", (int)type->u.pa.length); break;
  default: assert(false); break;
  }
}
#endif
#include "var.h"

#include <stdlib.h>  // malloc
#include <string.h>

#include "lexer.h"
#include "util.h"

int var_find(Vector *lvars, const char *name) {
  for (int i = 0, len = lvars->len; i < len; ++i) {
    VarInfo *info = (VarInfo*)lvars->data[i];
    if (info->name != NULL && strcmp(info->name, name) == 0)
      return i;
  }
  return -1;
}

VarInfo *var_add(Vector *lvars, const Token *ident, const Type *type, int flag) {
  // init is only for static local variable.
  const char *name = NULL;
  const char *label = NULL;
  VarInfo *ginfo = NULL;
  if (ident != NULL) {
    name = ident->u.ident;
    int idx = var_find(lvars, name);
    if (idx >= 0)
      parse_error(ident, "`%s' already defined", name);
    if (flag & VF_STATIC) {
      label = alloc_label();
      ginfo = define_global(type, flag, NULL, label);
    }
  }

  VarInfo *info = malloc(sizeof(*info));
  info->name = name;
  info->type = type;
  info->flag = flag;
  info->u.l.label = label;
  info->offset = -1;
  vec_push(lvars, info);
  return ginfo != NULL ? ginfo : info;
}

// Global

Map *gvar_map;

VarInfo *find_global(const char *name) {
  return (VarInfo*)map_get(gvar_map, name);
}

VarInfo *define_global(const Type *type, int flag, const Token *ident, const char *name) {
  if (name == NULL)
    name = ident->u.ident;
  VarInfo *varinfo = find_global(name);
  if (varinfo != NULL && !(varinfo->flag & VF_EXTERN)) {
    if (flag & VF_EXTERN)
      return varinfo;
    parse_error(ident, "`%s' already defined", name);
  }
  varinfo = malloc(sizeof(*varinfo));
  varinfo->name = name;
  varinfo->type = type;
  varinfo->flag = flag;
  varinfo->u.g.init = NULL;
  varinfo->offset = 0;
  map_put(gvar_map, name, varinfo);
  return varinfo;
}

// Scope

Scope *new_scope(Scope *parent, Vector *vars) {
  Scope *scope = malloc(sizeof(*scope));
  scope->parent = parent;
  scope->vars = vars;
  return scope;
}

VarInfo *scope_find(Scope *scope, const char *name) {
  for (;; scope = scope->parent) {
    if (scope == NULL)
      return NULL;
    if (scope->vars != NULL) {
      int idx = var_find(scope->vars, name);
      if (idx >= 0)
        return (VarInfo*)scope->vars->data[idx];
    }
  }
}
#include "expr.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "lexer.h"
#include "type.h"
#include "util.h"
#include "var.h"

static StructInfo *parse_struct(bool is_union);
static Expr *cast_expr(void);
static Expr *unary(void);

// Typedef

Map *typedef_map;

//

Expr *new_expr(enum ExprType type, const Type *valType, const Token *token) {
  Expr *expr = malloc(sizeof(*expr));
  expr->type = type;
  expr->valType = valType;
  expr->token = token;
  return expr;
}

bool is_const(Expr *expr) {
  // TODO: Handle constant variable.

  switch (expr->type) {
  case EX_NUM:
  case EX_STR:
    return true;
  default:
    return false;
  }
}

Expr *new_expr_numlit(const Type *type, const Token *token, const Num *num) {
  assert(type->type == TY_NUM);
  Expr *expr = new_expr(EX_NUM, type, token);
#if 0
  // TODO: Accept this
  expr->u.num = *num;
#else
  expr->u.num.ival = num->ival;
#endif
  return expr;
}

static Expr *new_expr_str(const Token *token, const char *str, size_t size) {
  Type *type = malloc(sizeof(*type));
  type->type = TY_ARRAY;
  type->u.pa.ptrof = &tyChar;
  type->u.pa.length = size;

  Expr *expr = new_expr(EX_STR, type, token);
  expr->u.str.buf = str;
  expr->u.str.size = size;
  return expr;
}

Expr *new_expr_varref(const char *name, const Type *type, bool global, const Token *token) {
  Expr *expr = new_expr(EX_VARREF, type, token);
  expr->u.varref.ident = name;
  expr->u.varref.global = global;
  return expr;
}

Expr *new_expr_bop(enum ExprType type, const Type *valType, const Token *token, Expr *lhs, Expr *rhs) {
  Expr *expr = new_expr(type, valType, token);
  expr->u.bop.lhs = lhs;
  expr->u.bop.rhs = rhs;
  return expr;
}

static Expr *new_expr_unary(enum ExprType type, const Type *valType, const Token *token, Expr *sub) {
  Expr *expr = new_expr(type, valType, token);
  expr->u.unary.sub = sub;
  return expr;
}

Expr *new_expr_deref(const Token *token, Expr *sub) {
  if (sub->valType->type != TY_PTR && sub->valType->type != TY_ARRAY)
    parse_error(token, "Cannot dereference raw type");
  return new_expr_unary(EX_DEREF, sub->valType->u.pa.ptrof, token, sub);
}

static Expr *new_expr_ternary(const Token *token, Expr *cond, Expr *tval, Expr *fval, const Type *type) {
  Expr *expr = new_expr(EX_TERNARY, type, token);
  expr->u.ternary.cond = cond;
  expr->u.ternary.tval = tval;
  expr->u.ternary.fval = fval;
  return expr;
}

Expr *new_expr_member(const Token *token, const Type *valType, Expr *target, const Token *acctok, const Token *ident, int index) {
  Expr *expr = new_expr(EX_MEMBER, valType, token);
  expr->u.member.target = target;
  expr->u.member.acctok = acctok;
  expr->u.member.ident = ident;
  expr->u.member.index = index;
  return expr;
}

static Expr *new_expr_funcall(const Token *token, Expr *func, Vector *args) {
  Expr *expr = new_expr(EX_FUNCALL, NULL, token);
  expr->u.funcall.func = func;
  expr->u.funcall.args = args;
  return expr;
}

static Expr *new_expr_comma(Vector *list) {
  Expr *expr = new_expr(EX_COMMA, NULL, NULL);
  expr->u.comma.list = list;
  return expr;
}

static Expr *funcall(Expr *func) {
  Vector *args = NULL;
  Token *token;
  if ((token = consume(TK_RPAR)) == NULL) {
    args = new_vector();
    for (;;) {
      Expr *arg = parse_assign();
      vec_push(args, arg);
      if ((token = consume(TK_RPAR)) != NULL)
        break;
      if (consume(TK_COMMA))
        continue;
      parse_error(NULL, "Comma or `)` expected");
    }
  }

  return new_expr_funcall(token, func, args);
}

Expr *array_index(const Token *token, Expr *array) {
  Expr *index = parse_expr();
  Token *tok;
  if ((tok = consume(TK_RBRACKET)) == NULL)
    parse_error(NULL, "`]' expected");
  //return new_expr_deref(add_expr(tok, array, index));
  return new_expr_unary(EX_DEREF, NULL, token, new_expr_bop(EX_ADD, NULL, token, array, index));
}

Expr *member_access(Expr *target, Token *acctok) {
  Token *ident;
  if (!(ident = consume(TK_IDENT)))
    parse_error(NULL, "`ident' expected");

  return new_expr_member(acctok, NULL, target, acctok, ident, -1);
}

static const Type *parse_enum(void) {
  Token *typeIdent = consume(TK_IDENT);
  Type *type = typeIdent != NULL ? find_enum(typeIdent->u.ident) : NULL;
  if (consume(TK_LBRACE)) {
    // TODO: Duplicate check.
    if (type != NULL)
      parse_error(typeIdent, "Duplicate enum type");
    type = define_enum(typeIdent);
    if (!consume(TK_RBRACE)) {
      int value = 0;
      for (;;) {
        Token *numtok;
        Token *ident = numtok = consume(TK_IDENT);
        if (ident == NULL)
          parse_error(NULL, "ident expected");
        if (consume(TK_ASSIGN)) {
          numtok = fetch_token();
          Expr *expr = analyze_expr(parse_const(), false);
          if (!(is_const(expr) && is_number(expr->valType->type))) {
            parse_error(numtok, "const expected for enum");
          }
          value = expr->u.num.ival;
        }

        // TODO: Check whether symbol is not defined.
        add_enum_member(type, ident, value);
        ++value;

        if (consume(TK_COMMA))
          ;
        if (consume(TK_RBRACE))
          break;
      }
    }
  } else {
    if (type == NULL)
      parse_error(typeIdent, "Unknown enum type");
  }
  return type;
}

const Type *parse_raw_type(int *pflag) {
  const Type *type = NULL;

  int flag = 0;
  for (;;) {
    if (consume(TK_UNSIGNED)) {
      flag |= VF_UNSIGNED;
      continue;
    }
    if (consume(TK_KWCONST)) {
      flag |= VF_CONST;
      continue;
    }
    if (consume(TK_STATIC)) {
      flag |= VF_STATIC;
      continue;
    }
    if (consume(TK_EXTERN)) {
      flag |= VF_EXTERN;
      continue;
    }

    if (type != NULL)
      break;

    Token *structtok;
    Token *ident;
    if (((structtok = consume(TK_STRUCT)) != NULL) ||
        ((structtok = consume(TK_UNION)) != NULL)) {
      bool is_union = structtok->type == TK_UNION;
      const char *name = NULL;
      Token *ident;
      if ((ident = consume(TK_IDENT)) != NULL)
        name = ident->u.ident;

      StructInfo *sinfo = NULL;
      if (consume(TK_LBRACE)) {  // Definition
        sinfo = parse_struct(is_union);
        if (name != NULL) {
          StructInfo *exist = find_struct(name);
          if (exist != NULL)
            parse_error(ident, "`%s' already defined", name);
          define_struct(name, sinfo);
        }
      } else {
        if (name != NULL) {
          sinfo = (StructInfo*)map_get(struct_map, name);
          if (sinfo != NULL) {
            if (sinfo->is_union != is_union)
              parse_error(structtok, "Wrong tag for `%s'", name);
          }
        }
      }

      if (name == NULL && sinfo == NULL)
        parse_error(NULL, "Illegal struct/union usage");

      Type *stype = malloc(sizeof(*type));
      stype->type = TY_STRUCT;
      stype->u.struct_.name = name;
      stype->u.struct_.info = sinfo;
      type = stype;
    } else if (consume(TK_ENUM)) {
      type = parse_enum();
    } else if ((ident = consume(TK_IDENT)) != NULL) {
      type = map_get(typedef_map, ident->u.ident);
      if (type == NULL)
        unget_token(ident);
    } else {
      static const enum TokenType kKeywords[] = {
        TK_KWVOID, TK_KWCHAR, TK_KWSHORT, TK_KWINT, TK_KWLONG,
      };
      static const Type *kTypes[] = {
        &tyVoid, &tyChar, &tyShort, &tyInt, &tyLong,
      };
      const int N = sizeof(kTypes) / sizeof(*kTypes);
      for (int i = 0; i < N; ++i) {
        if (consume(kKeywords[i])) {
          type = kTypes[i];
          break;
        }
      }
    }
    if (type == NULL)
      break;
  }

  if (pflag != NULL)
    *pflag = flag;

  return type;
}

void not_void(const Type *type) {
  if (type->type == TY_VOID)
    parse_error(NULL, "`void' not allowed");
}

const Type *parse_type_modifier(const Type* type) {
  if (type == NULL)
    return NULL;

  for (;;) {
    if (consume(TK_KWCONST)) {
      // TODO: Reflect to the type.
      ;
    }
    if (consume(TK_MUL))
      type = ptrof(type);
    else
      break;
  }

  return type;
}

const Type *parse_type_suffix(const Type *type) {
  if (type == NULL)
    return NULL;

  if (!consume(TK_LBRACKET))
    return type;
  size_t length = -1;
  if (consume(TK_RBRACKET)) {
    // Arbitrary size.
  } else {
    const Token *tok = fetch_token();
    Expr *expr = analyze_expr(parse_const(), false);
    if (!(is_const(expr) && is_number(expr->valType->type)))
      parse_error(NULL, "syntax error");
    if (expr->u.num.ival <= 0)
      parse_error(tok, "Array size must be greater than 0, but %d", (int)expr->u.num.ival);
    length = expr->u.num.ival;
    if (!consume(TK_RBRACKET))
      parse_error(NULL, "`]' expected");
  }
  return arrayof(parse_type_suffix(type), length);
}

static Vector *parse_funparam_types(bool *pvaargs) {  // Vector<Type*>
  Vector *params = parse_funparams(pvaargs);
  Vector *param_types = NULL;
  if (params != NULL) {
    param_types = new_vector();
    for (int i = 0, len = params->len; i < len; ++i)
      vec_push(param_types, ((VarInfo*)params->data[i])->type);
  }
  return param_types;
}

bool parse_var_def(const Type **prawType, const Type** ptype, int *pflag, Token **pident) {
  const Type *rawType = prawType != NULL ? *prawType : NULL;
  if (rawType == NULL) {
    rawType = parse_raw_type(pflag);
    if (rawType == NULL)
      return false;
    if (prawType != NULL)
      *prawType = rawType;
  }

  const Type *type = parse_type_modifier(rawType);

  Token *ident = NULL;
  if (consume(TK_LPAR)) {  // Funcion type.
    consume(TK_MUL);  // Skip `*' if exists.
    ident = consume(TK_IDENT);
    //if (ident == NULL && !allow_noname)
    //  parse_error(NULL, "Ident expected");
    if (!consume(TK_RPAR))
      parse_error(NULL, "`)' expected");
    if (!consume(TK_LPAR))
      parse_error(NULL, "`(' expected");

    bool vaargs;
    Vector *param_types = parse_funparam_types(&vaargs);
    type = ptrof(new_func_type(type, param_types, vaargs));
  } else {
    if (type->type != TY_VOID) {
      ident = consume(TK_IDENT);
      //if (ident == NULL && !allow_noname)
      //  parse_error(NULL, "Ident expected");
    }
  }
  if (type->type != TY_VOID)
    type = parse_type_suffix(type);

  *ptype = type;
  if (pident != NULL)
    *pident = ident;

  return true;
}

const Type *parse_full_type(int *pflag, Token **pident) {
  const Type *type;
  if (!parse_var_def(NULL, &type, pflag, pident))
    return NULL;
  return type;
}

Vector *parse_funparams(bool *pvaargs) {  // Vector<VarInfo*>, NULL=>old style.
  Vector *params = NULL;
  bool vaargs = false;
  if (consume(TK_RPAR)) {
    // Arbitrary funparams.
  } else {
    params = new_vector();
    for (;;) {
      if (consume(TK_DOTDOTDOT)) {
        vaargs = true;
        if (!consume(TK_RPAR))
          parse_error(NULL, "`)' expected");
        break;
      }

      const Type *type;
      int flag;
      Token *ident;
      if (!parse_var_def(NULL, &type, &flag, &ident))
        parse_error(NULL, "type expected");
      if (flag & VF_STATIC)
        parse_error(ident, "`static' for function parameter");
      if (flag & VF_EXTERN)
        parse_error(ident, "`extern' for function parameter");

      if (params->len == 0) {
        if (type->type == TY_VOID) {  // fun(void)
          if (!consume(TK_RPAR))
            parse_error(NULL, "`)' expected");
          break;
        }
      } else {
        not_void(type);
      }

      // If the type is array, handle it as a pointer.
      type = array_to_ptr(type);

      var_add(params, ident, type, flag);
      if (consume(TK_RPAR))
        break;
      if (consume(TK_COMMA))
        continue;
      parse_error(NULL, "Comma or `)' expected");
    }
  }
  *pvaargs = vaargs;
  return params;
}

// Parse struct or union definition `{...}`
static StructInfo *parse_struct(bool is_union) {
  Vector *members = new_vector();
  for (;;) {
    if (consume(TK_RBRACE))
      break;

    const Type *rawType = NULL;
    for (;;) {
      const Type *type;
      int flag;
      Token *ident;
      if (!parse_var_def(&rawType, &type, &flag, &ident))
        parse_error(NULL, "type expected");
      not_void(type);
      var_add(members, ident, type, flag);

      if (consume(TK_COMMA))
        continue;
      if (!consume(TK_SEMICOL))
        parse_error(NULL, "`;' expected");
      break;
    }
  }

  StructInfo *sinfo = malloc(sizeof(*sinfo));
  sinfo->members = members;
  sinfo->is_union = is_union;
  sinfo->size = -1;
  sinfo->align = 0;
  return sinfo;
}

static Expr *prim(void) {
  if (consume(TK_LPAR)) {
    Expr *expr = parse_expr();
    if (!consume(TK_RPAR))
      parse_error(NULL, "No close paren");
    return expr;
  }

  Token *tok;
  {
    const Type *type;
    if (((tok = consume(TK_CHARLIT)) != NULL && (type = &tyChar, true)) ||
        ((tok = consume(TK_INTLIT)) != NULL && (type = &tyInt, true)) ||
        ((tok = consume(TK_LONGLIT)) != NULL && (type = &tyLong, true))) {
      Num num = {tok->u.value};
      return new_expr_numlit(type, tok, &num);
    }
  }
  if ((tok = consume(TK_STR)))
    return new_expr_str(tok, tok->u.str.buf, tok->u.str.size);

  Token *ident;
  if ((ident = consume(TK_IDENT)) != NULL) {
    const char *name = ident->u.ident;
    return new_expr_varref(name, /*type*/NULL, /*global*/false, ident);
  }
  parse_error(NULL, "Number or Ident or open paren expected");
  return NULL;
}

static Expr *postfix(void) {
  Expr *expr = prim();

  for (;;) {
    Token *tok;
    if (consume(TK_LPAR))
      expr = funcall(expr);
    else if ((tok = consume(TK_LBRACKET)) != NULL)
      expr = array_index(tok, expr);
    else if ((tok = consume(TK_DOT)) != NULL)
      expr = member_access(expr, tok);
    else if ((tok = consume(TK_ARROW)) != NULL)
      expr = member_access(expr, tok);
    else if ((tok = consume(TK_INC)) != NULL)
      expr = new_expr_unary(EX_POSTINC, /*expr->valType*/NULL, tok, expr);
    else if ((tok = consume(TK_DEC)) != NULL)
      expr = new_expr_unary(EX_POSTDEC, /*expr->valType*/NULL, tok, expr);
    else
      return expr;
  }
}

static Expr *parse_sizeof(const Token *token) {
  const Type *type = NULL;
  Expr *expr = NULL;
  Token *tok;
  if ((tok = consume(TK_LPAR)) != NULL) {
    type = parse_full_type(NULL, NULL);
    if (type != NULL) {
      if (!consume(TK_RPAR))
        parse_error(NULL, "`)' expected");
    } else {
      unget_token(tok);
      expr = prim();
    }
  } else {
    expr = unary();
  }
  return new_expr_sizeof(token, type, expr);
}

static Expr *unary(void) {
  Token *tok;
  if ((tok = consume(TK_ADD)) != NULL) {
    Expr *expr = cast_expr();
    switch (expr->type) {
    case EX_NUM:
      return expr;
    default:
      return new_expr_unary(EX_POS, /*expr->valType*/NULL, tok, expr);
    }

    return expr;
  }

  if ((tok = consume(TK_SUB)) != NULL) {
    Expr *expr = cast_expr();
    switch (expr->type) {
    case EX_NUM:
      expr->u.num.ival = -expr->u.num.ival;
      return expr;
    default:
      return new_expr_unary(EX_NEG, /*expr->valType*/NULL, tok, expr);
    }
  }

  if ((tok = consume(TK_NOT)) != NULL) {
    Expr *expr = cast_expr();
    return new_expr_unary(EX_NOT, &tyBool, tok, expr);
  }

  if ((tok = consume(TK_AND)) != NULL) {
    Expr *expr = cast_expr();
    return new_expr_unary(EX_REF, /*ptrof(expr->valType)*/NULL, tok, expr);
  }

  if ((tok = consume(TK_MUL)) != NULL) {
    Expr *expr = cast_expr();
    return new_expr_unary(EX_DEREF, /*expr->valType->u.pa.ptrof*/NULL, tok, expr);
  }

  if ((tok = consume(TK_INC)) != NULL) {
    Expr *expr = unary();
    return new_expr_unary(EX_PREINC, /*expr->valType*/NULL, tok, expr);
  }

  if ((tok = consume(TK_DEC)) != NULL) {
    Expr *expr = unary();
    return new_expr_unary(EX_PREDEC, /*expr->valType*/NULL, tok, expr);
  }

  if ((tok = consume(TK_SIZEOF)) != NULL) {
    return parse_sizeof(tok);
  }

  return postfix();
}

static Expr *cast_expr(void) {
  Token *lpar;
  if ((lpar = consume(TK_LPAR)) != NULL) {
    int flag;
    const Token *token = fetch_token();
    const Type *type = parse_full_type(&flag, NULL);
    if (type != NULL) {  // Cast
      if (!consume(TK_RPAR))
        parse_error(NULL, "`)' expected");
      Expr *sub = cast_expr();
      Expr *expr = new_expr(EX_CAST, type, token);
      expr->u.cast.sub = sub;
      return expr;
    }
    unget_token(lpar);
  }
  return unary();
}

static Expr *mul(void) {
  Expr *expr = cast_expr();

  for (;;) {
    enum ExprType t;
    Token *tok;
    if ((tok = consume(TK_MUL)) != NULL)
      t = EX_MUL;
    else if ((tok = consume(TK_DIV)) != NULL)
      t = EX_DIV;
    else if ((tok = consume(TK_MOD)) != NULL)
      t = EX_MOD;
    else
      return expr;

    expr = new_expr_bop(t, NULL, tok, expr, cast_expr());
  }
}

static Expr *add(void) {
  Expr *expr = mul();

  for (;;) {
    enum ExprType t;
    Token *tok;
    if ((tok = consume(TK_ADD)) != NULL)
      t = EX_ADD;
    else if ((tok = consume(TK_SUB)) != NULL)
      t = EX_SUB;
    else
      return expr;

    expr = new_expr_bop(t, NULL, tok, expr, mul());
  }
}

static Expr *shift(void) {
  Expr *expr = add();

  for (;;) {
    enum ExprType t;
    Token *tok;
    if ((tok = consume(TK_LSHIFT)) != NULL)
      t = EX_LSHIFT;
    else if ((tok = consume(TK_RSHIFT)) != NULL)
      t = EX_RSHIFT;
    else
      return expr;

    Expr *lhs = expr, *rhs = add();
    expr = new_expr_bop(t, NULL, tok, lhs, rhs);
  }
}

static Expr *cmp(void) {
  Expr *expr = shift();

  for (;;) {
    enum ExprType t;
    Token *tok;
    if ((tok = consume(TK_LT)) != NULL)
      t = EX_LT;
    else if ((tok = consume(TK_GT)) != NULL)
      t = EX_GT;
    else if ((tok = consume(TK_LE)) != NULL)
      t = EX_LE;
    else if ((tok = consume(TK_GE)) != NULL)
      t = EX_GE;
    else
      return expr;

    Expr *lhs = expr, *rhs= shift();
    expr = new_expr_bop(t, &tyBool, tok, lhs, rhs);
  }
}

static Expr *eq(void) {
  Expr *expr = cmp();

  for (;;) {
    enum ExprType t;
    Token *tok;
    if ((tok = consume(TK_EQ)) != NULL)
      t = EX_EQ;
    else if ((tok = consume(TK_NE)) != NULL)
      t = EX_NE;
    else
      return expr;

    Expr *lhs = expr, *rhs= cmp();
    expr = new_expr_bop(t, &tyBool, tok, lhs, rhs);
  }
}

static Expr *and(void) {
  Expr *expr = eq();
  for (;;) {
    Token *tok;
    if ((tok = consume(TK_AND)) != NULL) {
      Expr *lhs = expr, *rhs= eq();
      expr = new_expr_bop(EX_BITAND, /*lhs->valType*/NULL, tok, lhs, rhs);
    } else
      return expr;
  }
}

static Expr *xor(void) {
  Expr *expr = and();
  for (;;) {
    Token *tok;
    if ((tok = consume(TK_HAT)) != NULL) {
      Expr *lhs = expr, *rhs= and();
      expr = new_expr_bop(EX_BITXOR, /*lhs->valType*/NULL, tok, lhs, rhs);
    } else
      return expr;
  }
}

static Expr *or(void) {
  Expr *expr = xor();
  for (;;) {
    Token *tok;
    if ((tok = consume(TK_OR)) != NULL) {
      Expr *lhs = expr, *rhs= xor();
      expr = new_expr_bop(EX_BITOR, /*lhs->valType*/NULL, tok, lhs, rhs);
    } else
      return expr;
  }
}

static Expr *logand(void) {
  Expr *expr = or();
  for (;;) {
    Token *tok;
    if ((tok = consume(TK_LOGAND)) != NULL)
      expr = new_expr_bop(EX_LOGAND, &tyBool, tok, expr, or());
    else
      return expr;
  }
}

static Expr *logior(void) {
  Expr *expr = logand();
  for (;;) {
    Token *tok;
    if ((tok = consume(TK_LOGIOR)) != NULL)
      expr = new_expr_bop(EX_LOGIOR, &tyBool, tok, expr, logand());
    else
      return expr;
  }
}

static Expr *conditional(void) {
  Expr *expr = logior();
  for (;;) {
    const Token *tok;
    if ((tok = consume(TK_QUESTION)) == NULL)
      return expr;
    Expr *t = parse_expr();
    if (!consume(TK_COLON))
      parse_error(NULL, "`:' expected");
    Expr *f = conditional();
    expr = new_expr_ternary(tok, expr, t, f, /*t->valType*/NULL);
  }
}

Expr *parse_assign(void) {
  Expr *expr = conditional();

  Token *tok;
  if ((tok = consume(TK_ASSIGN)) != NULL)
    return new_expr_bop(EX_ASSIGN, /*expr->valType*/NULL, tok, expr, parse_assign());
  enum ExprType t;
  if ((tok = consume(TK_ADD_ASSIGN)) != NULL)
    t = EX_ADD;
  else if ((tok = consume(TK_SUB_ASSIGN)) != NULL)
    t = EX_SUB;
  else if ((tok = consume(TK_MUL_ASSIGN)) != NULL)
    t = EX_MUL;
  else if ((tok = consume(TK_DIV_ASSIGN)) != NULL)
    t = EX_DIV;
  else if ((tok = consume(TK_MOD_ASSIGN)) != NULL)
    t = EX_MOD;
  else if ((tok = consume(TK_AND_ASSIGN)) != NULL)
    t = EX_BITAND;
  else if ((tok = consume(TK_OR_ASSIGN)) != NULL)
    t = EX_BITOR;
  else if ((tok = consume(TK_HAT_ASSIGN)) != NULL)
    t = EX_BITXOR;
  else if ((tok = consume(TK_LSHIFT_ASSIGN)) != NULL)
    t = EX_LSHIFT;
  else if ((tok = consume(TK_RSHIFT_ASSIGN)) != NULL)
    t = EX_RSHIFT;
  else
    return expr;

  return new_expr_unary(EX_ASSIGN_WITH, /*expr->valType*/NULL, tok,
                        new_expr_bop(t, NULL, tok, expr, parse_assign()));
}

Expr *parse_const(void) {
  return conditional();
}

Expr *parse_expr(void) {
  Expr *expr;
  Vector *list = NULL;
  for (;;) {
    expr = parse_assign();
    if (!consume(TK_COMMA))
      break;
    if (list == NULL)
      list = new_vector();
    vec_push(list, expr);
  }

  if (list == NULL)
    return expr;
  vec_push(list, expr);
  return new_expr_comma(list);
}
#include "expr.h"

#include <assert.h>
#include <string.h>

#include "lexer.h"
#include "type.h"
#include "util.h"
#include "var.h"

static const Type *tyNumTable[] = { &tyChar, &tyShort, &tyInt, &tyLong, &tyEnum };

Scope *curscope;

// Call before accessing struct member to ensure that struct is declared.
void ensure_struct(Type *type, const Token *token) {
  assert(type->type == TY_STRUCT);
  if (type->u.struct_.info == NULL) {
    // TODO: Search from name.
    StructInfo *sinfo = (StructInfo*)map_get(struct_map, type->u.struct_.name);
    if (sinfo == NULL)
      parse_error(token, "Accessing unknown struct(%s)'s member", type->u.struct_.name);
    type->u.struct_.info = sinfo;
  }
}

bool can_cast(const Type *dst, const Type *src, Expr *src_expr, bool is_explicit) {
  if (same_type(dst, src))
    return true;

  if (dst->type == TY_VOID)
    return src->type == TY_VOID || is_explicit;
  if (src->type == TY_VOID)
    return false;

  switch (dst->type) {
  case TY_NUM:
    switch (src->type) {
    case TY_NUM:
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
  case TY_PTR:
    switch (src->type) {
    case TY_NUM:
      if (src_expr->type == EX_NUM && src_expr->u.num.ival == 0)  // Special handling for 0 to pointer.
        return true;
      if (is_explicit)
        return true;
      break;
    case TY_PTR:
      if (is_explicit)
        return true;
      // void* is interchangable with any pointer type.
      if (dst->u.pa.ptrof->type == TY_VOID || src->u.pa.ptrof->type == TY_VOID)
        return true;
      break;
    case TY_ARRAY:
      if (is_explicit)
        return true;
      if (same_type(dst->u.pa.ptrof, src->u.pa.ptrof) ||
          can_cast(dst, ptrof(src->u.pa.ptrof), src_expr, is_explicit))
        return true;
      break;
    case TY_FUNC:
      if (is_explicit)
        return true;
      if (dst->u.pa.ptrof->type == TY_FUNC && same_type(dst->u.pa.ptrof, src))
        return true;
      break;
    default:  break;
    }
    break;
  case TY_ARRAY:
    switch (src->type) {
    case TY_PTR:
      if (is_explicit && same_type(dst->u.pa.ptrof, src->u.pa.ptrof))
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

bool check_cast(const Type *dst, const Type *src, Expr *src_expr, bool is_explicit) {
  if (can_cast(dst, src, src_expr, is_explicit))
    return true;
  parse_error(NULL, "Cannot convert value from type %d to %d", src->type, dst->type);
  return false;
}

Expr *new_expr_cast(const Type *type, const Token *token, Expr *sub, bool is_explicit) {
  if (type->type == TY_VOID || sub->valType->type == TY_VOID)
    parse_error(NULL, "cannot use `void' as a value");

  if (same_type(type, sub->valType))
    return sub;
  //if (is_const(sub)) {
  //  // Casting number types needs its value range info,
  //  // so handlded in codegen.
  //  sub->valType = type;
  //  return sub;
  //}

  check_cast(type, sub->valType, sub, is_explicit);

  Expr *expr = new_expr(EX_CAST, type, token);
  expr->u.cast.sub = sub;
  return expr;
}

Expr *new_expr_sizeof(const Token *token, const Type *type, Expr *sub) {
  Expr *expr = new_expr(EX_SIZEOF, &tySize, token);
  expr->u.sizeof_.type = type;
  expr->u.sizeof_.sub = sub;
  return expr;
}

// num +|- num
static Expr *add_num(enum ExprType exprType, const Token *tok, Expr *lhs, Expr *rhs, bool keep_left) {
  const Type *ltype = lhs->valType;
  const Type *rtype = rhs->valType;
  assert(ltype->type == TY_NUM && rtype->type == TY_NUM);
  enum NumType lnt = ltype->u.num.type;
  enum NumType rnt = rtype->u.num.type;
  if (lnt == NUM_ENUM)
    lnt = NUM_INT;
  if (rnt == NUM_ENUM)
    rnt = NUM_INT;

  if (is_const(lhs) && is_const(rhs)) {
    intptr_t lval = lhs->u.num.ival;
    intptr_t rval = rhs->u.num.ival;
    intptr_t value;
    switch (exprType) {
    case EX_ADD:
      value = lval + rval;
      break;
    case EX_SUB:
      value = lval - rval;
      break;
    default:
      assert(false);
      value = -1;
      break;
    }
    Num num = {value};
    const Type *type = lnt >= rnt ? lhs->valType : rhs->valType;
    return new_expr_numlit(type, lhs->token, &num);
  }

  const Type *type;
  if (lnt >= rnt || keep_left) {
    type = tyNumTable[lnt];
    rhs = new_expr_cast(type, rhs->token, rhs, false);
  } else {
    type = tyNumTable[rnt];
    lhs = new_expr_cast(type, lhs->token, lhs, false);
  }
  return new_expr_bop(exprType, type, tok, lhs, rhs);
}

// pointer +|- num
static Expr *add_ptr_num(enum ExprType exprType, const Token *token, Expr *ptr, Expr *num) {
  const Type *ptr_type = ptr->valType;
  if (ptr_type->type == TY_ARRAY)
    ptr_type = array_to_ptr(ptr_type);
  return new_expr_bop(exprType, ptr_type, token, ptr,
                      new_expr_bop(EX_MUL, &tySize, token,
                                   new_expr_cast(&tySize, token, num, false),
                                   new_expr_sizeof(token, ptr_type->u.pa.ptrof, NULL)));
}

Expr *add_expr(const Token *tok, Expr *lhs, Expr *rhs, bool keep_left) {
  const Type *ltype = lhs->valType;
  const Type *rtype = rhs->valType;
  //if (ltype->type == TY_ENUM)
  //  ltype = &tyInt;
  //if (rtype->type == TY_ENUM)
  //  rtype = &tyInt;

  if (is_number(ltype->type)) {
    if (is_number(rtype->type))
      return add_num(EX_ADD, tok, lhs, rhs, keep_left);
    if (same_type(ltype, rtype))
      return new_expr_bop(EX_ADD, ltype, tok, lhs, rhs);
  }

  switch (ltype->type) {
  case TY_NUM:
    switch (rtype->type) {
    case TY_PTR: case TY_ARRAY:
      if (!keep_left)
        return add_ptr_num(EX_ADD, tok, rhs, lhs);
      break;
    default:
      break;
    }
    break;

  case TY_PTR: case TY_ARRAY:
    switch (rtype->type) {
    case TY_NUM:
      return add_ptr_num(EX_ADD, tok, lhs, rhs);
    default:
      break;
    }
    break;

  default:
    break;
  }

  parse_error(tok, "Illegal `+'");
  return NULL;
}

static Expr *diff_ptr(const Token *tok, Expr *lhs, Expr *rhs) {
  if (!same_type(lhs->valType, rhs->valType))
    parse_error(tok, "Different pointer diff");
  const Type *elem_type = lhs->valType;
  if (elem_type->type == TY_PTR)
    elem_type = elem_type->u.pa.ptrof;
  return new_expr_bop(EX_DIV, &tySize, tok,
                      new_expr_bop(EX_SUB, &tySize, tok, lhs, rhs),
                      new_expr_sizeof(tok, elem_type, NULL));
}

static Expr *sub_expr(const Token *tok, Expr *lhs, Expr *rhs, bool keep_left) {
  if (is_number(lhs->valType->type)) {
    if (is_number(rhs->valType->type))
      return add_num(EX_SUB, tok, lhs, rhs, keep_left);
    if (same_type(lhs->valType, rhs->valType))
      return new_expr_bop(EX_SUB, lhs->valType, tok, lhs, rhs);
  }

  switch (lhs->valType->type) {
  case TY_PTR:
    switch (rhs->valType->type) {
    case TY_NUM:
      return add_ptr_num(EX_SUB, tok, lhs, rhs);
    case TY_PTR: case TY_ARRAY:
      return diff_ptr(tok, lhs, rhs);
    default:
      break;
    }
    break;

  case TY_ARRAY:
    switch (rhs->valType->type) {
    case TY_PTR: case TY_ARRAY:
      return diff_ptr(tok, lhs, rhs);
    default:
      break;
    }
    break;

  default:
    break;
  }

  parse_error(tok, "Illegal `-'");
  return NULL;
}

static bool cast_numbers(Expr **pLhs, Expr **pRhs, bool keep_left) {
  if (!is_number((*pLhs)->valType->type) ||
      !is_number((*pRhs)->valType->type))
    return false;

  enum NumType ltype = (*pLhs)->valType->u.num.type;
  enum NumType rtype = (*pRhs)->valType->u.num.type;
  if (ltype == NUM_ENUM)
    ltype = NUM_INT;
  if (rtype == NUM_ENUM)
    rtype = NUM_INT;
  if (ltype != rtype) {
    if (ltype > rtype || keep_left)
      *pRhs = new_expr_cast((*pLhs)->valType, (*pRhs)->token, *pRhs, false);
    else if (ltype < rtype)
      *pLhs = new_expr_cast((*pRhs)->valType, (*pLhs)->token, *pLhs, false);
  }
  return true;
}

static bool member_access_recur(const Type *type, const Token *ident, Vector *stack) {
  assert(type->type == TY_STRUCT);
  ensure_struct((Type*)type, ident);
  const char *name = ident->u.ident;

  Vector *lvars = type->u.struct_.info->members;
  for (int i = 0, len = lvars->len; i < len; ++i) {
    VarInfo *info = (VarInfo*)lvars->data[i];
    if (info->name != NULL) {
      if (strcmp(info->name, name) == 0) {
        vec_push(stack, (void*)(long)i);
        return true;
      }
    } else if (info->type->type == TY_STRUCT) {
      vec_push(stack, (void*)(long)i);
      bool res = member_access_recur(info->type, ident, stack);
      if (res)
        return true;
      //vec_pop(stack);
      --stack->len;
    }
  }
  return false;
}

static void analyze_cmp(Expr *expr) {
  Expr *lhs = expr->u.bop.lhs, *rhs = expr->u.bop.rhs;
  if (lhs->valType->type == TY_PTR || rhs->valType->type == TY_PTR) {
    const Type *lt = lhs->valType, *rt = rhs->valType;
    if (lt->type != TY_PTR) {
      const Type *tmp = lt;
      lt = rt;
      rt = tmp;
    }
    if (!can_cast(lt, rt, rhs, false))
      parse_error(expr->token, "Cannot compare pointer to other types");
    if (rt->type != TY_PTR) {
      if (lt == lhs->valType)
        expr->u.bop.rhs = new_expr_cast(lhs->valType, expr->token, rhs, false);
      else
        expr->u.bop.lhs = new_expr_cast(rhs->valType, expr->token, lhs, false);
    }
  } else {
    if (!cast_numbers(&expr->u.bop.lhs, &expr->u.bop.rhs, false))
      parse_error(expr->token, "Cannot compare except numbers");
  }
}

// Traverse expr to check semantics and determine value type.
Expr *analyze_expr(Expr *expr, bool keep_left) {
  if (expr == NULL)
    return NULL;

  switch (expr->type) {
  // Literals
  case EX_NUM:
  case EX_STR:
    assert(expr->valType != NULL);
    break;

  case EX_VARREF:
    {
      const char *name = expr->u.varref.ident;
      const Type *type = NULL;
      bool global = false;
      if (curscope != NULL) {
        VarInfo *varinfo = scope_find(curscope, name);
        if (varinfo != NULL) {
          if (varinfo->flag & VF_STATIC) {
            // Replace local variable reference to global.
            name = varinfo->u.l.label;
            expr = new_expr_varref(name, varinfo->type, true, expr->token);
            global = true;
          } else {
            type = varinfo->type;
          }
        }
      }
      if (type == NULL) {
        VarInfo *varinfo = find_global(name);
        if (varinfo != NULL) {
          global = true;
          type = varinfo->type;
        }
      }
      if (type == NULL) {
        intptr_t value;
        if (find_enum_value(name, &value)) {
          Num num = {.ival = value};
          return new_expr_numlit(&tyInt, NULL, &num);
        }
      }
      if (type == NULL)
        parse_error(expr->token, "Undefined `%s'", name);
      expr->valType = type;
      expr->u.varref.global = global;
    }
    break;

  // Binary operators
  case EX_ADD:
  case EX_SUB:
  case EX_MUL:
  case EX_DIV:
  case EX_MOD:
  case EX_BITAND:
  case EX_BITOR:
  case EX_BITXOR:
  case EX_LSHIFT:
  case EX_RSHIFT:
  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_GT:
  case EX_LE:
  case EX_GE:
  case EX_LOGAND:
  case EX_LOGIOR:
  case EX_ASSIGN:
    expr->u.bop.lhs = analyze_expr(expr->u.bop.lhs, false);
    expr->u.bop.rhs = analyze_expr(expr->u.bop.rhs, false);
    assert(expr->u.bop.lhs->valType != NULL);
    assert(expr->u.bop.rhs->valType != NULL);

    switch (expr->type) {
    case EX_ADD:
      return add_expr(expr->token, expr->u.bop.lhs, expr->u.bop.rhs, keep_left);
    case EX_SUB:
      return sub_expr(expr->token, expr->u.bop.lhs, expr->u.bop.rhs, keep_left);
    case EX_MUL:
    case EX_DIV:
    case EX_MOD:
    case EX_BITAND:
    case EX_BITOR:
    case EX_BITXOR:
      if (!cast_numbers(&expr->u.bop.lhs, &expr->u.bop.rhs, keep_left))
        parse_error(expr->token, "Cannot use `%d' except numbers.", expr->type);

      if (is_const(expr->u.bop.lhs) && is_const(expr->u.bop.rhs)) {
        Expr *lhs = expr->u.bop.lhs, *rhs = expr->u.bop.rhs;
        intptr_t lval = lhs->u.num.ival;
        intptr_t rval = rhs->u.num.ival;
        intptr_t value;
        switch (expr->type) {
        case EX_MUL:
          value = lval * rval;
          break;
        case EX_DIV:
          value = lval / rval;
          break;
        case EX_MOD:
          value = lval % rval;
          break;
        case EX_BITAND:
          value = lval & rval;
          break;
        case EX_BITOR:
          value = lval | rval;
          break;
        case EX_BITXOR:
          value = lval ^ rval;
          break;
        default:
          assert(!"err");
          value = -1;  // Dummy
          break;
        }
        Num num = {value};
        const Type *type = lhs->valType->u.num.type >= rhs->valType->u.num.type ? lhs->valType : rhs->valType;
        return new_expr_numlit(type, lhs->token, &num);
      }

      expr->valType = expr->u.bop.lhs->valType;
      break;

    case EX_LSHIFT:
    case EX_RSHIFT:
      {
        enum eType t;
        if (!is_number(t = expr->u.bop.lhs->valType->type) ||
            !is_number(t = expr->u.bop.rhs->valType->type))
          parse_error(expr->token, "Cannot use `%d' except numbers.", t);

        if (is_const(expr->u.bop.lhs) && is_const(expr->u.bop.rhs)) {
          intptr_t lval = expr->u.bop.lhs->u.num.ival;
          intptr_t rval = expr->u.bop.rhs->u.num.ival;
          intptr_t value = expr->type == EX_LSHIFT ? lval << rval : lval >> rval;
          Num num = {value};
          return new_expr_numlit(expr->u.bop.lhs->valType, expr->u.bop.lhs->token, &num);
        }

        expr->valType = expr->u.bop.lhs->valType;
      }
      break;

    case EX_EQ:
    case EX_NE:
    case EX_LT:
    case EX_GT:
    case EX_LE:
    case EX_GE:
      analyze_cmp(expr);
      break;

    case EX_LOGAND:
    case EX_LOGIOR:
      break;

    case EX_ASSIGN:
      expr->valType = expr->u.bop.lhs->valType;
      expr->u.bop.rhs = new_expr_cast(expr->valType, expr->token, expr->u.bop.rhs, false);
      break;

    default:
      fprintf(stderr, "expr type=%d\n", expr->type);
      assert(!"analyze not handled!");
      break;
    }
    break;

  // Unary operators
  case EX_POS:
  case EX_NEG:
  case EX_NOT:
  case EX_PREINC:
  case EX_PREDEC:
  case EX_POSTINC:
  case EX_POSTDEC:
  case EX_REF:
  case EX_DEREF:
  case EX_CAST:
  case EX_ASSIGN_WITH:
    expr->u.unary.sub = analyze_expr(expr->u.unary.sub, expr->type == EX_ASSIGN_WITH);
    assert(expr->u.unary.sub->valType != NULL);

    switch (expr->type) {
    case EX_POS:
      if (!is_number(expr->u.unary.sub->valType->type))
        parse_error(expr->token, "Cannot apply `+' except number types");
      return expr->u.unary.sub;

    case EX_NEG:
      if (!is_number(expr->u.unary.sub->valType->type))
        parse_error(expr->token, "Cannot apply `-' except number types");
      expr->valType = expr->u.unary.sub->valType;
      break;

    case EX_NOT:
      switch (expr->u.unary.sub->valType->type) {
      case TY_NUM:
      case TY_PTR:
      case TY_ARRAY:
        break;
      default:
        parse_error(expr->token, "Cannot apply `!' except number or pointer types");
        break;
      }
      break;

    case EX_PREINC:
    case EX_PREDEC:
    case EX_POSTINC:
    case EX_POSTDEC:
      expr->valType = expr->u.unary.sub->valType;
      break;

    case EX_REF:
      expr->valType = ptrof(expr->u.unary.sub->valType);
      break;

    case EX_DEREF:
      {
        Expr *sub = expr->u.unary.sub;
        if (sub->valType->type != TY_PTR && sub->valType->type != TY_ARRAY)
          parse_error(expr->token, "Cannot dereference raw type");
        expr->valType = sub->valType->u.pa.ptrof;
      }
      break;

    case EX_ASSIGN_WITH:
      expr->valType = expr->u.unary.sub->u.bop.lhs->valType;
      break;

    case EX_CAST:
      {
        Expr *sub = expr->u.unary.sub;
        if (same_type(expr->valType, sub->valType))
          return sub;
        check_cast(expr->valType, sub->valType, sub, true);
      }
      break;

    default:
      fprintf(stderr, "expr type=%d\n", expr->type);
      assert(!"analyze not handled!");
      break;
    }
    break;

  case EX_TERNARY:
    expr->u.ternary.cond = analyze_expr(expr->u.ternary.cond, false);
    expr->u.ternary.tval = analyze_expr(expr->u.ternary.tval, false);
    expr->u.ternary.fval = analyze_expr(expr->u.ternary.fval, false);
    {
      const Type *ttype = expr->u.ternary.tval->valType;
      const Type *ftype = expr->u.ternary.fval->valType;
      if (same_type(ttype, ftype)) {
        expr->valType = ttype;
      } else if (is_void_ptr(ttype) && ftype->type == TY_PTR) {
        expr->valType = ftype;
      } else if (is_void_ptr(ftype) && ttype->type == TY_PTR) {
        expr->valType = ttype;
      } else {
        parse_error(NULL, "lhs and rhs must be same type");
      }
    }
    break;

  case EX_MEMBER:  // x.member or x->member
    {
      Expr *target = expr->u.member.target;
      expr->u.member.target = target = analyze_expr(target, false);
      assert(target->valType != NULL);

      const Token *acctok = expr->u.member.acctok;
      const Token *ident = expr->u.member.ident;
      const char *name = ident->u.ident;

      // Find member's type from struct info.
      const Type *targetType = target->valType;
      if (acctok->type == TK_DOT) {
        if (targetType->type != TY_STRUCT)
          parse_error(acctok, "`.' for non struct value");
      } else {  // TK_ARROW
        if (targetType->type == TY_PTR)
          targetType = targetType->u.pa.ptrof;
        else if (targetType->type == TY_ARRAY)
          targetType = targetType->u.pa.ptrof;
        else
          parse_error(acctok, "`->' for non pointer value");
        if (targetType->type != TY_STRUCT)
          parse_error(acctok, "`->' for non struct value");
      }

      ensure_struct((Type*)targetType, ident);
      int index = var_find(targetType->u.struct_.info->members, name);
      if (index >= 0) {
        VarInfo *varinfo = (VarInfo*)targetType->u.struct_.info->members->data[index];
        expr->valType = varinfo->type;
        expr->u.member.index = index;
      } else {
        Vector *stack = new_vector();
        bool res = member_access_recur(targetType, ident, stack);
        if (!res)
          parse_error(ident, "`%s' doesn't exist in the struct", name);
        Expr *p = target;
        const Type *type = targetType;
        VarInfo *varinfo;
        for (int i = 0; i < stack->len; ++i) {
          int index = (int)(long)stack->data[i];
          varinfo = type->u.struct_.info->members->data[index];
          type = varinfo->type;
          p = new_expr_member(acctok, type, p, NULL, NULL, index);
        }
        expr = p;
      }
    }
    break;

  case EX_SIZEOF:
    {
      Expr *sub = expr->u.sizeof_.sub;
      if (sub != NULL) {
        sub = analyze_expr(sub, false);
        assert(sub->valType != NULL);
        expr->u.sizeof_.type = sub->valType;
      }
    }
    break;

  case EX_FUNCALL:
    {
      Expr *func = expr->u.funcall.func;
      Vector *args = expr->u.funcall.args;  // <Expr*>
      expr->u.funcall.func = func = analyze_expr(func, false);
      if (args != NULL) {
        for (int i = 0, len = args->len; i < len; ++i)
          args->data[i] = analyze_expr(args->data[i], false);
      }

      const Type *functype;
      if (!((functype = func->valType)->type == TY_FUNC ||
            (func->valType->type == TY_PTR && (functype = func->valType->u.pa.ptrof)->type == TY_FUNC)))
        parse_error(NULL, "Cannot call except funtion");
      expr->valType = functype->u.func.ret;

      Vector *param_types = functype->u.func.param_types;  // <const Type*>
      bool vaargs = functype->u.func.vaargs;
      if (param_types != NULL) {
        int argc = args != NULL ? args->len : 0;
        int paramc = param_types->len;
        if (!(argc == paramc ||
              (vaargs && argc >= paramc)))
          parse_error(func->token, "function `%s' expect %d arguments, but %d", func->u.varref.ident, paramc, argc);
      }

      if (args != NULL && param_types != NULL) {
        int paramc = param_types->len;
        for (int i = 0, len = args->len; i < len; ++i) {
          if (i < param_types->len) {
            Expr *arg = args->data[i];
            const Type *type = (const Type*)param_types->data[i];
            args->data[i] = new_expr_cast(type, arg->token, arg, false);
          } else if (vaargs && i >= paramc) {
            Expr *arg = args->data[i];
            const Type *type = arg->valType;
            if (type->type == TY_NUM && type->u.num.type < NUM_INT)  // Promote variadic argument.
              args->data[i] = new_expr_cast(&tyInt, arg->token, arg, false);
          }
        }
      }
    }
    break;

  case EX_COMMA:
    {
      Vector *list = expr->u.comma.list;
      int len = list->len;
      for (int i = 0; i < len; ++i)
        list->data[i] = analyze_expr(list->data[i], false);
      expr->valType = ((Expr*)list->data[len - 1])->valType;
    }
    break;

  default:
    fprintf(stderr, "expr type=%d\n", expr->type);
    assert(!"analyze not handled!");
    break;
  }

if (expr->valType == NULL) { fprintf(stderr, "expr->type=%d, ", expr->type); }
  assert(expr->valType != NULL);
  return expr;
}
#include "parser.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc

#include "expr.h"
#include "lexer.h"
#include "type.h"
#include "util.h"

static Node *stmt(void);

static VarDecl *new_vardecl(const Type *type, const Token *ident, Initializer *init, int flag) {
  VarDecl *decl = malloc(sizeof(*decl));
  decl->type = type;
  decl->ident = ident;
  decl->init = init;
  decl->flag = flag;
  return decl;
}

static Defun *new_defun(const Type *rettype, const char *name, Vector *params, int flag, bool vaargs) {
  Defun *defun = malloc(sizeof(*defun));
  defun->rettype = rettype;
  defun->name = name;
  defun->params = params;
  defun->flag = flag;
  defun->vaargs = vaargs;

  defun->type = NULL;
  defun->stmts = NULL;
  defun->top_scope = NULL;
  defun->all_scopes = new_vector();
  defun->labels = NULL;
  defun->gotos = NULL;
  defun->ret_label = NULL;
  return defun;
}

static Node *new_node(enum NodeType type) {
  Node *node = malloc(sizeof(Node));
  node->type = type;
  return node;
}

Node *new_node_expr(Expr *e) {
  Node *node = new_node(ND_EXPR);
  node->u.expr = e;
  return node;
}

static Node *new_node_block(Vector *nodes) {
  Node *node = new_node(ND_BLOCK);
  node->u.block.scope = NULL;
  node->u.block.nodes = nodes;
  return node;
}

static Node *new_node_if(Expr *cond, Node *tblock, Node *fblock) {
  Node *node = new_node(ND_IF);
  node->u.if_.cond = cond;
  node->u.if_.tblock = tblock;
  node->u.if_.fblock = fblock;
  return node;
}

static Node *new_node_switch(Expr *value) {
  Node *node = new_node(ND_SWITCH);
  node->u.switch_.value = value;
  node->u.switch_.body = NULL;
  node->u.switch_.case_values = new_vector();
  node->u.switch_.has_default = false;
  return node;
}

static Node *new_node_case(int value) {
  Node *node = new_node(ND_CASE);
  node->u.case_.value = value;
  return node;
}

static Node *new_node_default(void) {
  Node *node = new_node(ND_DEFAULT);
  return node;
}

static Node *new_node_while(Expr *cond, Node *body) {
  Node *node = new_node(ND_WHILE);
  node->u.while_.cond = cond;
  node->u.while_.body = body;
  return node;
}

static Node *new_node_do_while(Node *body, Expr *cond) {
  Node *node = new_node(ND_DO_WHILE);
  node->u.while_.body = body;
  node->u.while_.cond = cond;
  return node;
}

static Node *new_node_for(Expr *pre, Expr *cond, Expr *post, Node *body) {
  Node *node = new_node(ND_FOR);
  node->u.for_.pre = pre;
  node->u.for_.cond = cond;
  node->u.for_.post = post;
  node->u.for_.body = body;
  return node;
}

static Node *new_node_return(Expr *val) {
  Node *node = new_node(ND_RETURN);
  node->u.return_.val = val;
  return node;
}

static Node *new_node_goto(const Token *label) {
  Node *node = new_node(ND_GOTO);
  node->u.goto_.tok = label;
  node->u.goto_.ident = label->u.ident;
  return node;
}

static Node *new_node_label(const char *name, Node *stmt) {
  Node *node = new_node(ND_LABEL);
  node->u.label.name = name;
  node->u.label.stmt = stmt;
  return node;
}

static Node *new_node_vardecl(Vector *decls) {
  Node *node = new_node(ND_VARDECL);
  node->u.vardecl.decls = decls;
  node->u.vardecl.inits = NULL;
  return node;
}

static Node *new_node_defun(Defun *defun) {
  Node *node = new_node(ND_DEFUN);
  node->u.defun = defun;
  return node;
}

// Initializer

static Initializer *parse_initializer(void) {
  Initializer *result = malloc(sizeof(*result));
  if (consume(TK_LBRACE)) {
    Vector *multi = new_vector();
    if (!consume(TK_RBRACE)) {
      for (;;) {
        Initializer *init;
        if (consume(TK_DOT)) {  // .member=value
          Token *ident = consume(TK_IDENT);
          if (ident == NULL)
            parse_error(NULL, "`ident' expected for dotted initializer");
          if (!consume(TK_ASSIGN))
            parse_error(NULL, "`=' expected for dotted initializer");
          Initializer *value = parse_initializer();
          init = malloc(sizeof(*init));
          init->type = vDot;
          init->u.dot.name = ident->u.ident;
          init->u.dot.value = value;
        } else if (consume(TK_LBRACKET)) {
          Expr *index = parse_const();
          if (!consume(TK_RBRACKET))
            parse_error(NULL, "`]' expected");
          consume(TK_ASSIGN);  // both accepted: `[1] = 2`, and `[1] 2`
          Initializer *value = parse_initializer();
          init = malloc(sizeof(*init));
          init->type = vArr;
          init->u.arr.index = index;
          init->u.arr.value = value;
        } else {
          init = parse_initializer();
        }
        vec_push(multi, init);

        if (consume(TK_COMMA)) {
          if (consume(TK_RBRACE))
            break;
        } else {
          if (!consume(TK_RBRACE))
            parse_error(NULL, "`}' or `,' expected");
          break;
        }
      }
    }
    result->type = vMulti;
    result->u.multi = multi;
  } else {
    result->type = vSingle;
    result->u.single = parse_assign();
  }
  return result;
}

static Vector *parse_vardecl_cont(const Type *rawType, Type *type, int flag, Token *ident) {
  Vector *decls = NULL;
  bool first = true;
  do {
    if (!first) {
      if (!parse_var_def(&rawType, (const Type**)&type, &flag, &ident) || ident == NULL) {
        parse_error(NULL, "`ident' expected");
        return NULL;
      }
    }
    first = false;
    not_void(type);

    Initializer *init = NULL;
    if (consume(TK_ASSIGN)) {
      init = parse_initializer();
    }

    VarDecl *decl = new_vardecl(type, ident, init, flag);
    if (decls == NULL)
      decls = new_vector();
    vec_push(decls, decl);
  } while (consume(TK_COMMA));
  return decls;
}

static Node *parse_vardecl(void) {
  const Type *rawType = NULL;
  Type *type;
  int flag;
  Token *ident;
  if (!parse_var_def(&rawType, (const Type**)&type, &flag, &ident))
    return NULL;
  if (ident == NULL)
    parse_error(NULL, "Ident expected");

  Vector *decls = parse_vardecl_cont(rawType, type, flag, ident);

  if (!consume(TK_SEMICOL))
    parse_error(NULL, "`;' expected");

  return decls != NULL ? new_node_vardecl(decls) : NULL;
}

static Node *parse_if(void) {
  if (consume(TK_LPAR)) {
    Expr *cond = parse_expr();
    if (consume(TK_RPAR)) {
      Node *tblock = stmt();
      Node *fblock = NULL;
      if (consume(TK_ELSE)) {
        fblock = stmt();
      }
      return new_node_if(cond, tblock, fblock);
    }
  }
  parse_error(NULL, "Illegal syntax in `if'");
  return NULL;
}

static Node *parse_switch(void) {
  if (consume(TK_LPAR)) {
    Expr *value = parse_expr();
    if (consume(TK_RPAR)) {
      Node *swtch = new_node_switch(value);
      swtch->u.switch_.body = stmt();
      return swtch;
    }
  }
  parse_error(NULL, "Illegal syntax in `switch'");
  return NULL;
}

static Node *parse_case(void) {
  Token *tok = fetch_token();
  Expr *valnode = analyze_expr(parse_const(), false);
  if (!is_const(valnode))
    parse_error(tok, "Cannot use expression");
  intptr_t value = valnode->u.num.ival;

  if (!consume(TK_COLON))
    parse_error(NULL, "`:' expected");

  return new_node_case(value);
}

static Node *parse_default(void) {
  if (!consume(TK_COLON))
    parse_error(NULL, "`:' expected");
  return new_node_default();
}

static Node *parse_while(void) {
  if (consume(TK_LPAR)) {
    Expr *cond = parse_expr();
    if (consume(TK_RPAR)) {
      Node *body = stmt();

      return new_node_while(cond, body);
    }
  }
  parse_error(NULL, "Illegal syntax in `while'");
  return NULL;
}

static Node *parse_do_while(void) {
  Node *body = stmt();

  if (consume(TK_WHILE) && consume(TK_LPAR)) {
    Expr *cond = parse_expr();
    if (consume(TK_RPAR) && consume(TK_SEMICOL)) {
      return new_node_do_while(body, cond);
    }
  }
  parse_error(NULL, "Illegal syntax in `do-while'");
  return NULL;
}

static Node *parse_for(void) {
  if (consume(TK_LPAR)) {
    Expr *pre = NULL;
    bool nopre = false;
    Vector *decls = NULL;
    if (consume(TK_SEMICOL)) {
      nopre = true;
    } else {
      const Type *rawType = NULL;
      Type *type;
      int flag;
      Token *ident;
      if (parse_var_def(&rawType, (const Type**)&type, &flag, &ident)) {
        if (ident == NULL)
          parse_error(NULL, "Ident expected");
        decls = parse_vardecl_cont(rawType, type, flag, ident);
        if (!consume(TK_SEMICOL))
          decls = NULL;  // Error
      } else {
        pre = parse_expr();
        if (!consume(TK_SEMICOL))
          pre = NULL;  // Error
      }
    }
    if (nopre || pre != NULL || decls != NULL) {
      Expr *cond = NULL;
      Expr *post = NULL;
      Node *body = NULL;
      if ((consume(TK_SEMICOL) || (cond = parse_expr(), consume(TK_SEMICOL))) &&
          (consume(TK_RPAR) || (post = parse_expr(), consume(TK_RPAR)))) {
        body = stmt();

        Node *node = new_node_for(pre, cond, post, body);
        if (decls != NULL) {
          Vector *stmts = new_vector();
          vec_push(stmts, new_node_vardecl(decls));
          vec_push(stmts, node);
          return new_node_block(stmts);
        } else {
          return node;
        }
      }
    }
  }
  parse_error(NULL, "Illegal syntax in `for'");
  return NULL;
}

static Node *parse_break_continue(enum NodeType type) {
  if (!consume(TK_SEMICOL))
    parse_error(NULL, "`;' expected");
  return new_node(type);
}

static Node *parse_goto(void) {
  Token *label = consume(TK_IDENT);
  if (label == NULL)
    parse_error(NULL, "label for goto expected");
  if (!consume(TK_SEMICOL))
    parse_error(NULL, "`;' expected");
  return new_node_goto(label);
}

static Node *parse_return(void) {
  Expr *val = NULL;
  Token *tok;
  if ((tok = consume(TK_SEMICOL)) != NULL) {
  } else {
    tok = fetch_token();
    val = parse_expr();
    if (!consume(TK_SEMICOL))
      parse_error(NULL, "`;' expected");
  }
  return new_node_return(val);
}

// Multiple stmt-s, also accept `case` and `default`.
static Vector *read_stmts(void) {
  Vector *nodes = NULL;
  for (;;) {
    if (consume(TK_RBRACE))
      return nodes;

    Node *node;
    Token *tok;
    if ((node = parse_vardecl()) != NULL)
      ;
    else if ((tok = consume(TK_CASE)) != NULL)
      node = parse_case();
    else if ((tok = consume(TK_DEFAULT)) != NULL)
      node = parse_default();
    else
      node = stmt();

    if (node == NULL)
      continue;
    if (nodes == NULL)
      nodes = new_vector();
    vec_push(nodes, node);
  }
}

static Node *parse_block(void) {
  Vector *nodes = read_stmts();
  return new_node_block(nodes);
}

static Node *stmt(void) {
  Token *label = consume(TK_IDENT);
  if (label != NULL) {
    if (consume(TK_COLON)) {
      return new_node_label(label->u.ident, stmt());
    }
    unget_token(label);
  }

  if (consume(TK_SEMICOL))
    return NULL;

  if (consume(TK_LBRACE))
    return parse_block();

  if (consume(TK_IF))
    return parse_if();

  if (consume(TK_SWITCH))
    return parse_switch();

  if (consume(TK_WHILE))
    return parse_while();

  if (consume(TK_DO))
    return parse_do_while();

  if (consume(TK_FOR))
    return parse_for();

  Token *tok;
  if ((tok = consume(TK_BREAK)) != NULL) {
    return parse_break_continue(ND_BREAK);
  }
  if ((tok = consume(TK_CONTINUE)) != NULL) {
    return parse_break_continue(ND_CONTINUE);
  }
  if ((tok = consume(TK_GOTO)) != NULL) {
    return parse_goto();
  }

  if (consume(TK_RETURN))
    return parse_return();

  // expression statement.
  Expr *val = parse_expr();
  if (!consume(TK_SEMICOL))
    parse_error(NULL, "Semicolon required");
  return new_node_expr(val);
}

static Node *parse_defun(const Type *rettype, int flag, Token *ident) {
  const char *name = ident->u.ident;
  bool vaargs;
  Vector *params = parse_funparams(&vaargs);

  // Definition.
  Defun *defun = new_defun(rettype, name, params, flag, vaargs);
  if (consume(TK_SEMICOL)) {  // Prototype declaration.
  } else {
    if (!consume(TK_LBRACE)) {
      parse_error(NULL, "`;' or `{' expected");
      return NULL;
    }

    defun->stmts = read_stmts();
    // Ensure stmts to be non-null to indicate this is not prototype definition.
    if (defun->stmts == NULL)
      defun->stmts = new_vector();
  }
  return new_node_defun(defun);
}

static void parse_typedef(void) {
  int flag;
  Token *ident;
  const Type *type = parse_full_type(&flag, &ident);
  if (type == NULL)
    parse_error(NULL, "type expected");
  not_void(type);

  if (ident == NULL) {
    ident = consume(TK_IDENT);
    if (ident == NULL)
      parse_error(NULL, "ident expected");
  }
  const char *name = ident->u.ident;

  map_put(typedef_map, name, type);

  if (!consume(TK_SEMICOL))
    parse_error(NULL, "`;' expected");
}

static Node *parse_global_var_decl(const Type *rawtype, int flag, const Type *type, Token *ident) {
  bool first = true;
  Vector *decls = NULL;
  do {
    if (!first) {
      type = parse_type_modifier(rawtype);
      if ((ident = consume(TK_IDENT)) == NULL)
        parse_error(NULL, "`ident' expected");
    }
    first = false;

    if (type->type == TY_VOID)
      parse_error(ident, "`void' not allowed");

    type = parse_type_suffix(type);
    Initializer *init = NULL;
    const Token *tok;
    if ((tok = consume(TK_ASSIGN)) != NULL) {
      init = parse_initializer();
    }

    VarDecl *decl = new_vardecl(type, ident, init, flag);
    if (decls == NULL)
      decls = new_vector();
    vec_push(decls, decl);
  } while (consume(TK_COMMA));

  if (!consume(TK_SEMICOL))
    parse_error(NULL, "`;' or `,' expected");

  return decls != NULL ? new_node_vardecl(decls) : NULL;
}

static Node *toplevel(void) {
  int flag;
  const Type *rawtype = parse_raw_type(&flag);
  if (rawtype != NULL) {
    const Type *type = parse_type_modifier(rawtype);
    if ((type->type == TY_STRUCT ||
         (type->type == TY_NUM && type->u.num.type == NUM_ENUM)) &&
        consume(TK_SEMICOL))  // Just struct/union definition.
      return NULL;

    Token *ident;
    if ((ident = consume(TK_IDENT)) != NULL) {
      if (consume(TK_LPAR))  // Function.
        return parse_defun(type, flag, ident);

      return parse_global_var_decl(rawtype, flag, type, ident);
    }
    parse_error(NULL, "ident expected");
    return NULL;
  }
  if (consume(TK_TYPEDEF)) {
    parse_typedef();
    return NULL;
  }
  parse_error(NULL, "Unexpected token");
  return NULL;
}

Node *parse_program(void) {
  Vector *nodes = new_vector();
  while (!consume(TK_EOF)) {
    Node *node = toplevel();
    if (node != NULL)
      vec_push(nodes, node);
  }

  Node *node = new_node(ND_TOPLEVEL);
  node->u.toplevel.nodes = nodes;
  return node;
}
#include "sema.h"

#include <assert.h>
#include <stdlib.h>  // malloc

#include "expr.h"
#include "lexer.h"
#include "parser.h"
#include "type.h"
#include "util.h"
#include "var.h"

const int LF_BREAK = 1 << 0;
const int LF_CONTINUE = 1 << 0;

Defun *curfunc;
static int curloopflag;
static Node *curswitch;

// Scope

static Scope *enter_scope(Defun *defun, Vector *vars) {
  Scope *scope = new_scope(curscope, vars);
  curscope = scope;
  vec_push(defun->all_scopes, scope);
  return scope;
}

static void exit_scope(void) {
  assert(curscope != NULL);
  curscope = curscope->parent;
}

static VarInfo *add_cur_scope(const Token *ident, const Type *type, int flag) {
  if (curscope->vars == NULL)
    curscope->vars = new_vector();
  return var_add(curscope->vars, ident, type, flag);
}

static void fix_array_size(Type *type, Initializer *init) {
  assert(init != NULL);
  assert(type->type == TY_ARRAY);

  bool is_str = false;
  if (init->type != vMulti &&
      !(is_char_type(type->u.pa.ptrof) &&
        init->type == vSingle &&
        can_cast(type, init->u.single->valType, init->u.single, false) &&
        (is_str = true))) {
    parse_error(NULL, "Error initializer");
  }

  size_t arr_len = type->u.pa.length;
  if (arr_len == (size_t)-1) {
    if (is_str) {
      type->u.pa.length = init->u.single->u.str.size;
    } else {
      size_t index = 0;
      size_t max_index = 0;
      size_t i, len = init->u.multi->len;
      for (i = 0; i < len; ++i) {
        Initializer *init_elem = init->u.multi->data[i];
        if (init_elem->type == vArr) {
          assert(init_elem->u.arr.index->type == EX_NUM);
          index = init_elem->u.arr.index->u.num.ival;
        }
        ++index;
        if (max_index < index)
          max_index = index;
      }
      type->u.pa.length = max_index;
    }
  } else {
    assert(!is_str || init->u.single->type == EX_STR);
    size_t init_len = is_str ? init->u.single->u.str.size : (size_t)init->u.multi->len;
    if (init_len > arr_len)
      parse_error(NULL, "Initializer more than array size");
  }
}

static void add_func_label(const char *label) {
  assert(curfunc != NULL);
  if (curfunc->labels == NULL)
    curfunc->labels = new_map();
  map_put(curfunc->labels, label, label);  // Put dummy value.
}

static void add_func_goto(Node *node) {
  assert(curfunc != NULL);
  if (curfunc->gotos == NULL)
    curfunc->gotos = new_vector();
  vec_push(curfunc->gotos, node);
}

static Initializer *analyze_initializer(Initializer *init) {
  if (init == NULL)
    return NULL;

  switch (init->type) {
  case vSingle:
    init->u.single = analyze_expr(init->u.single, false);
    break;
  case vMulti:
    for (int i = 0; i < init->u.multi->len; ++i)
      init->u.multi->data[i] = analyze_initializer(init->u.multi->data[i]);
    break;
  case vDot:
    init->u.dot.value = analyze_initializer(init->u.dot.value);
    break;
  case vArr:
    init->u.arr.value = analyze_initializer(init->u.arr.value);
    break;
  }
  return init;
}

static void string_initializer(Expr *dst, Expr *src, Vector *inits) {
  // Initialize char[] with string literal (char s[] = "foo";).
  assert(dst->valType->type == TY_ARRAY && is_char_type(dst->valType->u.pa.ptrof));
  assert(src->valType->type == TY_ARRAY && is_char_type(src->valType->u.pa.ptrof));

  const char *str = src->u.str.buf;
  size_t size = src->u.str.size;
  size_t dstsize = dst->valType->u.pa.length;
  if (dstsize == (size_t)-1) {
    ((Type*)dst->valType)->u.pa.length = dstsize = size;
  } else {
    if (dstsize < size)
      parse_error(NULL, "Buffer is shorter than string: %d for \"%s\"", (int)dstsize, str);
  }

  for (size_t i = 0; i < size; ++i) {
    Num n = {.ival=i};
    Expr *index = new_expr_numlit(&tyInt, NULL, &n);
    vec_push(inits,
             new_node_expr(new_expr_bop(EX_ASSIGN, &tyChar, NULL,
                                        new_expr_deref(NULL, add_expr(NULL, dst, index, true)),
                                        new_expr_deref(NULL, add_expr(NULL, src, index, true)))));
  }
}

static int compare_desig_start(const void *a, const void *b) {
  const size_t *pa = *(size_t**)a;
  const size_t *pb = *(size_t**)b;
  intptr_t d = *pa - *pb;
  return d > 0 ? 1 : d < 0 ? -1 : 0;
}

static Initializer *flatten_array_initializer(Initializer *init) {
  // Check whether vDot or vArr exists.
  int i = 0, len = init->u.multi->len;
  for (; i < len; ++i) {
    Initializer *init_elem = init->u.multi->data[i];
    if (init_elem->type == vDot)
      parse_error(NULL, "dot initializer for array");
    if (init_elem->type == vArr)
      break;
  }
  if (i >= len)  // vArr not exits.
    return init;

  // Enumerate designated initializer.
  Vector *ranges = new_vector();  // <(start, count)>
  size_t lastStartIndex = 0;
  size_t lastStart = 0;
  size_t index = i;
  for (; i <= len; ++i, ++index) {  // '+1' is for last range.
    Initializer *init_elem;
    if (i >= len || (init_elem = init->u.multi->data[i])->type == vArr) {
      if (i < len && init_elem->u.arr.index->type != EX_NUM)
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
      lastStart = index = init_elem->u.arr.index->u.num.ival;
      lastStartIndex = i;
    } else if (init_elem->type == vDot)
      parse_error(NULL, "dot initializer for array");
  }

  // Sort
  qsort(ranges->data, ranges->len, sizeof(size_t*), compare_desig_start);

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
      Initializer *elem = init->u.multi->data[index + j];
      if (j == 0 && index != start && elem->type != vArr) {
        Initializer *arr = malloc(sizeof(*arr));
        arr->type = vArr;
        Num n = {.ival = start};
        arr->u.arr.index = new_expr_numlit(&tyInt, NULL, &n);
        arr->u.arr.value = elem;
        elem = arr;
      }
      vec_push(reordered, elem);
    }
  }

  Initializer *init2 = malloc(sizeof(*init2));
  init2->type = vMulti;
  init2->u.multi = reordered;
  return init2;
}

Initializer *flatten_initializer(const Type *type, Initializer *init) {
  if (init == NULL)
    return NULL;

  switch (type->type) {
  case TY_STRUCT:
    {
      if (init->type != vMulti)
        parse_error(NULL, "`{...}' expected for initializer");

      ensure_struct((Type*)type, NULL);
      const StructInfo *sinfo = type->u.struct_.info;
      int n = sinfo->members->len;
      int m = init->u.multi->len;
      if (n <= 0) {
        if (m > 0)
          parse_error(NULL, "Initializer for empty struct");
        return NULL;
      }
      if (sinfo->is_union && m > 1)
        parse_error(NULL, "Initializer for union more than 1");

      Initializer **values = malloc(sizeof(Initializer*) * n);
      for (int i = 0; i < n; ++i)
        values[i] = NULL;

      int index = 0;
      for (int i = 0; i < m; ++i) {
        Initializer *value = init->u.multi->data[i];
        if (value->type == vArr)
          parse_error(NULL, "indexed initializer for array");

        if (value->type == vDot) {
          index = var_find(sinfo->members, value->u.dot.name);
          if (index < 0)
            parse_error(NULL, "`%s' is not member of struct", value->u.dot.name);
          value = value->u.dot.value;
        }
        if (index >= n)
          parse_error(NULL, "Too many init values");

        // Allocate string literal for char* as a char array.
        if (value->type == vSingle && value->u.single->type == EX_STR) {
          const VarInfo *member = sinfo->members->data[index];
          if (member->type->type == TY_PTR &&
              is_char_type(member->type->u.pa.ptrof)) {
            Expr *expr = value->u.single;
            Initializer *strinit = malloc(sizeof(*strinit));
            strinit->type = vSingle;
            strinit->u.single = expr;

            // Create string and point to it.
            Type* strtype = arrayof(&tyChar, expr->u.str.size);
            const char * label = alloc_label();
            const Token *ident = alloc_ident(label, NULL, NULL);
            VarInfo *varinfo = define_global(strtype, VF_CONST | VF_STATIC, ident, NULL);
            varinfo->u.g.init = strinit;

            // Replace initializer from string literal to string array defined in global.
            value->u.single = new_expr_varref(label, strtype, true, ident);
          }
        }

        values[index++] = value;
      }

      Initializer *flat = malloc(sizeof(*flat));
      flat->type = vMulti;
      //flat->u.multi = new_vector();
      Vector *v = malloc(sizeof(*v));
      v->len = v->capacity = n;
      v->data = (void**)values;
      flat->u.multi = v;

      return flat;
    }
  case TY_ARRAY:
    switch (init->type) {
    case vMulti:
      init = flatten_array_initializer(init);
      break;
    case vSingle:
      // Special handling for string (char[]).
      if (can_cast(type, init->u.single->valType, init->u.single, false))
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

  switch (type->type) {
  case TY_NUM:
    if (init->type == vSingle) {
      switch (init->u.single->type) {
      case EX_NUM:
        return init;
      default:
        parse_error(NULL, "initializer type error");
        break;
      }
    }
    break;
  case TY_PTR:
    {
      if (init->type != vSingle)
        parse_error(NULL, "initializer type error");
      Expr *value = init->u.single;
      switch (value->type) {
      case EX_REF:
        {
          value = value->u.unary.sub;
          if (value->type != EX_VARREF)
            parse_error(NULL, "pointer initializer must be varref");
          if (!value->u.varref.global)
            parse_error(NULL, "Allowed global reference only");

          VarInfo *info = find_global(value->u.varref.ident);
          assert(info != NULL);

          if (!same_type(type->u.pa.ptrof, info->type))
            parse_error(NULL, "Illegal type");

          return init;
        }
      case EX_VARREF:
        {
          if (!value->u.varref.global)
            parse_error(NULL, "Allowed global reference only");

          VarInfo *info = find_global(value->u.varref.ident);
          assert(info != NULL);

          if (info->type->type != TY_ARRAY || !same_type(type->u.pa.ptrof, info->type->u.pa.ptrof))
            parse_error(NULL, "Illegal type");

          return init;
        }
      case EX_CAST:
        // Handle NULL assignment.
        while (value->type == EX_CAST)
          value = value->u.unary.sub;
        if (!is_number(value->valType->type))
          break;
        // Fallthrough
      case EX_NUM:
        {
          Initializer *init2 = malloc(sizeof(*init2));
          init2->type = vSingle;
          init2->u.single = value;
          return init2;
        }
        break;
      case EX_STR:
        {
          if (!(is_char_type(type->u.pa.ptrof) && value->type == EX_STR))
            parse_error(NULL, "Illegal type");

          // Create string and point to it.
          Type* type2 = arrayof(type->u.pa.ptrof, value->u.str.size);
          const char *label = alloc_label();
          const Token *ident = alloc_ident(label, NULL, NULL);
          VarInfo *varinfo = define_global(type2, VF_CONST | VF_STATIC, ident, NULL);
          varinfo->u.g.init = init;

          Initializer *init2 = malloc(sizeof(*init2));
          init2->type = vSingle;
          init2->u.single = new_expr_varref(label, type2, true, ident);
          return init2;
        }
      default:
        break;
      }
      parse_error(NULL, "initializer type error: type=%d", value->type);
    }
    break;
  case TY_ARRAY:
    switch (init->type) {
    case vMulti:
      {
        const Type *elemtype = type->u.pa.ptrof;
        Vector *multi = init->u.multi;
        for (int i = 0, len = multi->len; i < len; ++i) {
          Initializer *eleminit = multi->data[i];
          multi->data[i] = check_global_initializer(elemtype, eleminit);
        }
      }
      break;
    case vSingle:
      if (is_char_type(type->u.pa.ptrof) && init->u.single->type == EX_STR) {
        assert(type->u.pa.length != (size_t)-1);
        if (type->u.pa.length < init->u.single->u.str.size) {
          parse_error(NULL, "Array size shorter than initializer");
        }
        return init;
      }
      // Fallthrough
    case vDot:
    default:
      parse_error(NULL, "Illegal initializer");
      break;
    }
    break;
  case TY_STRUCT:
    {
      const StructInfo *sinfo = type->u.struct_.info;
      for (int i = 0, n = sinfo->members->len; i < n; ++i) {
        VarInfo* varinfo = sinfo->members->data[i];
        Initializer *init_elem = init->u.multi->data[i];
        if (init_elem != NULL)
          init->u.multi->data[i] = check_global_initializer(varinfo->type, init_elem);
      }
    }
    break;
  default:
    parse_error(NULL, "Global initial value for type %d not implemented (yet)\n", type->type);
    break;
  }
  return init;
}

static Vector *assign_initial_value(Expr *expr, Initializer *init, Vector *inits) {
  if (init == NULL)
    return inits;

  if (inits == NULL)
    inits = new_vector();

  Initializer *org_init = init;
  init = flatten_initializer(expr->valType, init);

  switch (expr->valType->type) {
  case TY_ARRAY:
    switch (init->type) {
    case vMulti:
      {
        size_t arr_len = expr->valType->u.pa.length;
        assert(arr_len != (size_t)-1);
        if ((size_t)init->u.multi->len > arr_len)
          parse_error(NULL, "Initializer more than array size");
        size_t len = init->u.multi->len;
        size_t index = 0;
        for (size_t i = 0; i < len; ++i, ++index) {
          Initializer *init_elem = init->u.multi->data[i];
          if (init_elem->type == vArr) {
            Expr *ind = init_elem->u.arr.index;
            if (ind->type != EX_NUM)
              parse_error(NULL, "Number required");
            index = ind->u.num.ival;
            init_elem = init_elem->u.arr.value;
          }

          Num n = {.ival=index};
          Expr *add = add_expr(NULL, expr, new_expr_numlit(&tyInt, NULL, &n), true);

          assign_initial_value(new_expr_deref(NULL, add), init_elem, inits);
        }
      }
      break;
    case vSingle:
      // Special handling for string (char[]).
      if (can_cast(expr->valType, init->u.single->valType, init->u.single, false)) {
        string_initializer(expr, init->u.single, inits);
        break;
      }
      // Fallthrough
    default:
      parse_error(NULL, "Error initializer");
      break;

    }
    break;
  case TY_STRUCT:
    {
      if (init->type != vMulti)
        parse_error(NULL, "`{...}' expected for initializer");

      const StructInfo *sinfo = expr->valType->u.struct_.info;
      if (!sinfo->is_union) {
        for (int i = 0, n = sinfo->members->len; i < n; ++i) {
          VarInfo* varinfo = sinfo->members->data[i];
          Expr *member = new_expr_member(NULL, varinfo->type, expr, NULL, NULL, i);
          Initializer *init_elem = init->u.multi->data[i];
          if (init_elem != NULL)
            assign_initial_value(member, init_elem, inits);
        }
      } else {
        int n = sinfo->members->len;
        int m = init->u.multi->len;
        if (n <= 0 && m > 0)
          parse_error(NULL, "Initializer for empty union");
        if (org_init->u.multi->len > 1)
          parse_error(NULL, "More than one initializer for union");

        for (int i = 0; i < n; ++i) {
          Initializer *init_elem = init->u.multi->data[i];
          if (init_elem == NULL)
            continue;
          VarInfo* varinfo = sinfo->members->data[i];
          Expr *member = new_expr_member(NULL, varinfo->type, expr, NULL, NULL, i);
          assign_initial_value(member, init_elem, inits);
          break;
        }
      }
    }
    break;
  default:
    if (init->type != vSingle)
      parse_error(NULL, "Error initializer");
    vec_push(inits,
             new_node_expr(new_expr_bop(EX_ASSIGN, expr->valType, NULL, expr,
                                        new_expr_cast(expr->valType, NULL, init->u.single, false))));
    break;
  }

  return inits;
}

static Node *sema_vardecl(Node *node) {
  assert(node->type == ND_VARDECL);
  Vector *decls = node->u.vardecl.decls;
  Vector *inits = NULL;
  for (int i = 0, len = decls->len; i < len; ++i) {
    VarDecl *decl = decls->data[i];
    const Type *type = decl->type;
    const Token *ident = decl->ident;
    int flag = decl->flag;
    Initializer *init = decl->init;

    if (type->type == TY_ARRAY && init != NULL)
      fix_array_size((Type*)type, init);

    if (curfunc != NULL) {
      VarInfo *varinfo = add_cur_scope(ident, type, flag);
      init = analyze_initializer(init);

      // TODO: Check `init` can be cast to `type`.
      if (flag & VF_STATIC) {
        varinfo->u.g.init = check_global_initializer(type, init);
        // static variable initializer is handled in codegen, same as global variable.
      } else if (init != NULL) {
        inits = assign_initial_value(
            new_expr_varref(ident->u.ident, type, false, NULL), init, inits);
      }
    } else {
      intptr_t eval;
      if (find_enum_value(ident->u.ident, &eval))
        parse_error(NULL, "`%s' is already defined", ident->u.ident);
      if (flag & VF_EXTERN && init != NULL)
        parse_error(/*tok*/ NULL, "extern with initializer");
      // Toplevel
      VarInfo *varinfo = define_global(type, flag, ident, NULL);
      init = analyze_initializer(init);
      varinfo->u.g.init = check_global_initializer(type, init);
    }
  }

  node->u.vardecl.inits = inits;
  return node;
}

static void sema_nodes(Vector *nodes) {
  if (nodes == NULL)
    return;
  for (int i = 0, len = nodes->len; i < len; ++i)
    nodes->data[i] = sema(nodes->data[i]);
}

static void sema_defun(Defun *defun) {
  const Token *ident = NULL;

  Vector *param_types = NULL;
  if (defun->params != NULL) {
    param_types = new_vector();
    for (int i = 0, len = defun->params->len; i < len; ++i)
      vec_push(param_types, ((VarInfo*)defun->params->data[i])->type);
  }
  defun->type = new_func_type(defun->rettype, param_types, defun->vaargs);

  VarInfo *def = find_global(defun->name);
  if (def == NULL) {
    define_global(defun->type, defun->flag | VF_CONST, ident, defun->name);
  } else {
    if (def->type->type != TY_FUNC)
      parse_error(ident, "Definition conflict: `%s'");
    // TODO: Check type.
    // TODO: Check duplicated definition.
    if (def->u.g.init != NULL)
      parse_error(ident, "`%s' function already defined");
  }

  if (defun->stmts != NULL) {  // Not prototype defintion.
    curfunc = defun;
    enter_scope(defun, defun->params);  // Scope for parameters.
    curscope = defun->top_scope = enter_scope(defun, NULL);
    sema_nodes(defun->stmts);
    exit_scope();
    exit_scope();
    curfunc = NULL;
    curscope = NULL;

    // Check goto labels.
    if (defun->gotos != NULL) {
      Vector *gotos = defun->gotos;
      Map *labels = defun->labels;
      for (int i = 0; i < gotos->len; ++i) {
        Node *node = gotos->data[i];
        if (labels == NULL || map_get(labels, node->u.goto_.ident) == NULL)
          parse_error(node->u.goto_.tok, "`%s' not found", node->u.goto_.ident);
      }
    }
  }
}

Node *sema(Node *node) {
  if (node == NULL)
    return node;

  switch (node->type) {
  case ND_EXPR:
    node->u.expr = analyze_expr(node->u.expr, false);
    break;

  case ND_DEFUN:
    sema_defun(node->u.defun);
    break;

  case ND_BLOCK:
    {
      Scope *parent_scope = curscope;
      if (curfunc != NULL)
        node->u.block.scope = curscope = enter_scope(curfunc, NULL);
      sema_nodes(node->u.block.nodes);
      curscope = parent_scope;
    }
    break;

  case ND_IF:
    node->u.if_.cond = analyze_expr(node->u.if_.cond, false);
    node->u.if_.tblock = sema(node->u.if_.tblock);
    node->u.if_.fblock = sema(node->u.if_.fblock);
    break;

  case ND_SWITCH:
    {
      Node *save_switch = curswitch;
      int save_flag = curloopflag;
      curloopflag |= LF_BREAK;
      curswitch = node;

      node->u.switch_.value = analyze_expr(node->u.switch_.value, false);
      node->u.switch_.body = sema(node->u.switch_.body);

      curloopflag = save_flag;
      curswitch = save_switch;
    }
    break;

  case ND_WHILE:
  case ND_DO_WHILE:
    {
      node->u.while_.cond = analyze_expr(node->u.while_.cond, false);

      int save_flag = curloopflag;
      curloopflag |= LF_BREAK | LF_CONTINUE;

      node->u.while_.body = sema(node->u.while_.body);

      curloopflag = save_flag;
    }
    break;

  case ND_FOR:
    {
      node->u.for_.pre = analyze_expr(node->u.for_.pre, false);
      node->u.for_.cond = analyze_expr(node->u.for_.cond, false);
      node->u.for_.post = analyze_expr(node->u.for_.post, false);

      int save_flag = curloopflag;
      curloopflag |= LF_BREAK | LF_CONTINUE;

      node->u.for_.body = sema(node->u.for_.body);

      curloopflag = save_flag;
    }
    break;

  case ND_BREAK:
    if ((curloopflag & LF_BREAK) == 0)
      parse_error(/*tok*/ NULL, "`break' cannot be used outside of loop");
    break;

  case ND_CONTINUE:
    if ((curloopflag & LF_CONTINUE) == 0)
      parse_error(/*tok*/ NULL, "`continue' cannot be used outside of loop");
    break;

  case ND_RETURN:
    {
      assert(curfunc != NULL);
      const Type *rettype = curfunc->type->u.func.ret;
      Expr *val = node->u.return_.val;
      Token *tok = NULL;
      if (val == NULL) {
        if (rettype->type != TY_VOID)
          parse_error(tok, "`return' required a value");
      } else {
        if (rettype->type == TY_VOID)
          parse_error(tok, "void function `return' a value");

        const Token *tok = NULL;
        Expr *val = analyze_expr(node->u.return_.val, false);
        node->u.return_.val = new_expr_cast(rettype, tok, val, false);
      }
    }
    break;

  case ND_CASE:
    {
      if (curswitch == NULL)
        parse_error(/*tok*/ NULL, "`case' cannot use outside of `switch`");

      intptr_t value = node->u.case_.value;
      // Check duplication.
      Vector *values = curswitch->u.switch_.case_values;
      for (int i = 0, len = values->len; i < len; ++i) {
        if ((intptr_t)values->data[i] == value)
          parse_error(/*tok*/ NULL, "Case value `%lld' already defined: %s", value);
      }
      vec_push(values, (void*)value);
    }
    break;

  case ND_DEFAULT:
    if (curswitch == NULL)
      parse_error(/*tok*/ NULL, "`default' cannot use outside of `switch'");
    if (curswitch->u.switch_.has_default)
      parse_error(/*tok*/ NULL, "`default' already defined in `switch'");

    curswitch->u.switch_.has_default = true;
    break;

  case ND_GOTO:
    add_func_goto(node);
    break;

  case ND_LABEL:
    add_func_label(node->u.label.name);
    node->u.label.stmt = sema(node->u.label.stmt);
    break;

  case ND_VARDECL:
    return sema_vardecl(node);

  case ND_TOPLEVEL:
    sema_nodes(node->u.toplevel.nodes);
    break;

  default:
    fprintf(stderr, "sema: Unhandled node, type=%d\n", node->type);
    assert(false);
    break;
  }
  return node;
}
#include "codegen.h"

#include <assert.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "expr.h"
#include "lexer.h"
#include "parser.h"
#include "sema.h"
#include "type.h"
#include "util.h"
#include "var.h"

const int FRAME_ALIGN = 8;
const int STACK_PARAM_BASE_OFFSET = (2 - MAX_REG_ARGS) * 8;

#include "x86_64.h"

#define ALIGN_SECTION_SIZE(sec, align_)  do { int align = (int)(align_); add_asm_align(align); } while (0)

size_t type_size(const Type *type) {
  switch (type->type) {
  case TY_VOID:
    return 1;  // ?
  case TY_NUM:
    switch (type->u.num.type) {
    case NUM_CHAR:
      return 1;
    case NUM_SHORT:
      return 2;
    case NUM_INT:
    case NUM_ENUM:
      return 4;
    case NUM_LONG:
      return 8;
    default:
      assert(!"Error");
      return 1;
    }
  case TY_PTR:
  case TY_FUNC:
    return 8;
  case TY_ARRAY:
    assert(type->u.pa.length != (size_t)-1);
    return type_size(type->u.pa.ptrof) * type->u.pa.length;
  case TY_STRUCT:
    calc_struct_size(type->u.struct_.info);
    return type->u.struct_.info->size;
  default:
    assert(false);
    return 1;
  }
}

static int align_size(const Type *type) {
  switch (type->type) {
  case TY_VOID:
    return 1;  // ?
  case TY_NUM:
    switch (type->u.num.type) {
    case NUM_CHAR:
      return 1;
    case NUM_SHORT:
      return 2;
    case NUM_INT:
    case NUM_ENUM:
      return 4;
    case NUM_LONG:
      return 8;
    default:
      assert(!"Error");
      return 1;
    }
  case TY_PTR:
  case TY_FUNC:
    return 8;
  case TY_ARRAY:
    return align_size(type->u.pa.ptrof);
  case TY_STRUCT:
    calc_struct_size(type->u.struct_.info);
    return type->u.struct_.info->align;
  default:
    assert(false);
    return 1;
  }
}

void calc_struct_size(StructInfo *sinfo) {
  assert(sinfo != NULL);
  if (sinfo->size >= 0)
    return;

  size_t size = 0;
  size_t maxsize = 0;
  int max_align = 1;

  for (int i = 0, len = sinfo->members->len; i < len; ++i) {
    VarInfo *varinfo = (VarInfo*)sinfo->members->data[i];
    size_t sz = type_size(varinfo->type);
    int align = align_size(varinfo->type);
    size = ALIGN(size, align);
    varinfo->offset = size;
    if (!sinfo->is_union)
      size += sz;
    else
      if (maxsize < sz)
        maxsize = sz;
    if (max_align < align)
      max_align = align;
  }

  if (sinfo->is_union)
    size = maxsize;
  size = ALIGN(size, max_align);
  sinfo->size = size;
  sinfo->align = max_align;
}

static FILE *asm_fp;

void add_asm(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  fprintf(asm_fp, "\t");
  vfprintf(asm_fp, fmt, ap);
  fprintf(asm_fp, "\n");
  va_end(ap);
}

void add_asm_label(const char *label) {
  fprintf(asm_fp, "%s:\n", label);
}

static void add_asm_comment(const char *comment, ...) {
  if (comment == NULL) {
    fprintf(asm_fp, "\n");
    return;
  }

  va_list ap;
  va_start(ap, comment);
  fprintf(asm_fp, "// ");
  vfprintf(asm_fp, comment, ap);
  fprintf(asm_fp, "\n");
  va_end(ap);
}

static void add_asm_align(int align) {
  if ((align) > 1)
    add_asm(".align %d", (int)(align));
}

static const char *escape(int c) {
  switch (c) {
  case '\0': return "\\0";
  case '\n': return "\\n";
  case '\r': return "\\r";
  case '\t': return "\\t";
  case '"': return "\\\"";
  case '\\': return "\\\\";
  default:   return NULL;
  }
}

static char *append_str(const char *str, const char *add, size_t size) {
  if (size == 0)
    size = strlen(add);
  size_t len = str == NULL ? 0 : strlen(str);
  char *newstr = malloc(len + size + 1);
  if (str != NULL)
    memcpy(newstr, str, len);
  memcpy(newstr + len, add, size);
  newstr[len + size] = '\0';
  return newstr;
}

static char *escape_string(const char *str, size_t size) {
  const char *s, *p;
  char *escaped = NULL;
  for (s = p = str; ; ++p) {
    bool is_end = (size_t)(p - str) >= size;
    const char *e = NULL;
    if (!is_end && (e = escape(*p)) == NULL)
      continue;

    if (p - s > 0) {
      char *newstr1 = append_str(escaped, s, p - s);
      free(escaped);
      escaped = newstr1;
    }
    if (is_end)
      return escaped;
    char *newstr2 = append_str(escaped, e, 0);
    free(escaped);
    escaped = newstr2;
    s = p + 1;
  }
}

void construct_initial_value(unsigned char *buf, const Type *type, Initializer *init, Vector **pptrinits) {
  assert(init == NULL || init->type != vDot);

  add_asm_align(align_size(type));

  switch (type->type) {
  case TY_NUM:
    {
      intptr_t v = 0;
      if (init != NULL) {
        assert(init->type == vSingle);
        Expr *value = init->u.single;
        if (!(is_const(value) && is_number(value->valType->type)))
          error("Illegal initializer: constant number expected");
        v = value->u.num.ival;
      }

      int size = type_size(type);
      for (int i = 0; i < size; ++i)
        buf[i] = v >> (i * 8);  // Little endian

      const char *fmt;
      switch (type->u.num.type) {
      case NUM_CHAR:  fmt = ".byte %"PRIdPTR; break;
      case NUM_SHORT: fmt = ".word %"PRIdPTR; break;
      case NUM_LONG:  fmt = ".quad %"PRIdPTR; break;
      default:
      case NUM_INT: case NUM_ENUM:
        fmt = ".long %"PRIdPTR;
        break;
      }
      UNUSED(fmt);
      add_asm(fmt, v);
    }
    break;
  case TY_PTR:
    if (init != NULL) {
      assert(init->type == vSingle);
      Expr *value = init->u.single;
      while (value->type == EX_CAST)
        value = value->u.unary.sub;
      if (value->type == EX_REF || value->type == EX_VARREF) {
        if (value->type == EX_REF)
          value = value->u.unary.sub;
        // TODO: Type check.

        assert(value->type == EX_VARREF);
        assert(value->u.varref.global);

        void **init = malloc(sizeof(void*) * 2);
        init[0] = buf;
        init[1] = (void*)value->u.varref.ident;
        if (*pptrinits == NULL)
          *pptrinits = new_vector();
        vec_push(*pptrinits, init);

        add_asm(".quad %s", value->u.varref.ident);
      } else if (value->type == EX_STR) {
        assert(!"`char* s = \"...\"`; should be handled in parser");
      } else if (is_const(value) && value->type == EX_NUM) {
        intptr_t x = value->u.num.ival;
        for (int i = 0; i < WORD_SIZE; ++i)
          buf[i] = x >> (i * 8);  // Little endian

        add_asm(".quad 0x%"PRIxPTR, x);
      } else {
        assert(!"initializer type error");
      }
    } else {
      add_asm(".quad 0");
    }
    break;
  case TY_ARRAY:
    if (init == NULL || init->type == vMulti) {
      const Type *elem_type = type->u.pa.ptrof;
      size_t elem_size = type_size(elem_type);
      if (init != NULL) {
        Vector *init_array = init->u.multi;
        size_t index = 0;
        size_t len = init_array->len;
        for (size_t i = 0; i < len; ++i, ++index) {
          Initializer *init_elem = init_array->data[i];
          if (init_elem->type == vArr) {
            size_t next = init_elem->u.arr.index->u.num.ival;
            for (size_t j = index; j < next; ++j)
              construct_initial_value(buf + (j * elem_size), elem_type, NULL, pptrinits);
            index = next;
            init_elem = init_elem->u.arr.value;
          }
          construct_initial_value(buf + (index * elem_size), elem_type, init_elem, pptrinits);
        }
        assert((size_t)len <= type->u.pa.length);
      }
    } else {
      if (init->type == vSingle &&
          is_char_type(type->u.pa.ptrof) && init->u.single->type == EX_STR) {
        int src_size = init->u.single->u.str.size;
        size_t size = type_size(type);
        assert(size >= (size_t)src_size);
        memcpy(buf, init->u.single->u.str.buf, src_size);

        UNUSED(size);
        add_asm(".ascii \"%s\"", escape_string((char*)buf, size));
      } else {
        error("Illegal initializer");
      }
    }
    break;
  case TY_STRUCT:
    {
      assert(init == NULL || init->type == vMulti);

      const StructInfo *sinfo = type->u.struct_.info;
      int count = 0;
      for (int i = 0, n = sinfo->members->len; i < n; ++i) {
        VarInfo* varinfo = sinfo->members->data[i];
        Initializer *mem_init;
        if (init == NULL) {
          if (sinfo->is_union)
            continue;
          mem_init = NULL;
        } else {
          mem_init = init->u.multi->data[i];
        }
        if (mem_init != NULL || !sinfo->is_union) {
          construct_initial_value(buf + varinfo->offset, varinfo->type, mem_init, pptrinits);
          ++count;
        }
      }
      if (sinfo->is_union && count <= 0) {
        VarInfo* varinfo = sinfo->members->data[0];
        construct_initial_value(buf + varinfo->offset, varinfo->type, NULL, pptrinits);
      }
    }
    break;
  default:
    fprintf(stderr, "Global initial value for type %d not implemented (yet)\n", type->type);
    assert(false);
    break;
  }
}

static void put_data(const char *label, const VarInfo *varinfo) {
  size_t size = type_size(varinfo->type);
  unsigned char *buf = calloc(size, 1);
  if (buf == NULL)
    error("Out of memory");

  ALIGN_SECTION_SIZE(sec, align_size(varinfo->type));
  if ((varinfo->flag & VF_STATIC) == 0)  // global
    add_asm(".globl %s", label);
  ADD_LABEL(label);

  Vector *ptrinits = NULL;  // <[ptr, label]>
  construct_initial_value(buf, varinfo->type, varinfo->u.g.init, &ptrinits);
  //add_section_data(sec, buf, size);

  free(buf);
}

// Put RoData into code.
static void put_rodata(void) {
  for (int i = 0, len = map_count(gvar_map); i < len; ++i) {
    const VarInfo *varinfo = (const VarInfo*)gvar_map->vals->data[i];
    if (varinfo->type->type == TY_FUNC ||
        (varinfo->flag & VF_EXTERN) != 0 || varinfo->u.g.init == NULL ||
        (varinfo->flag & VF_CONST) == 0)
      continue;

    const char *name = (const char *)gvar_map->keys->data[i];
    put_data(name, varinfo);
  }
}

// Put global with initial value (RwData).
static void put_rwdata(void) {
  for (int i = 0, len = map_count(gvar_map); i < len; ++i) {
    const VarInfo *varinfo = (const VarInfo*)gvar_map->vals->data[i];
    if (varinfo->type->type == TY_FUNC ||
        (varinfo->flag & VF_EXTERN) != 0 || varinfo->u.g.init == NULL ||
        (varinfo->flag & VF_CONST) != 0)
      continue;

    const char *name = (const char *)gvar_map->keys->data[i];
    put_data(name, varinfo);
  }
}

// Put global without initial value (bss).
static void put_bss(void) {
  for (int i = 0, len = map_count(gvar_map); i < len; ++i) {
    const char *name = (const char *)gvar_map->keys->data[i];
    const VarInfo *varinfo = (const VarInfo*)gvar_map->vals->data[i];
    if (varinfo->type->type == TY_FUNC || varinfo->u.g.init != NULL ||
        (varinfo->flag & VF_EXTERN) != 0)
      continue;
    //ALIGN_SECTION_SIZE(SEC_DATA, align_size(varinfo->type));
    int align = align_size(varinfo->type);
    add_asm_align(align);
    //instruction_pointer = ALIGN(instruction_pointer, align);
    size_t size = type_size(varinfo->type);
    if (size < 1)
      size = 1;
    //add_label(name);
    //add_bss(size);
    add_asm(".comm %s, %d", name, size);
  }
}

void fixup_locations(void) {
  add_asm(".section .rodata");
  put_rodata();

  // Data section
  //sections[SEC_DATA].start = instruction_pointer = ALIGN(instruction_pointer, 0x1000);  // Page size.

  add_asm_comment(NULL);
  add_asm(".data");
  put_rwdata();

  add_asm_comment(NULL);
  add_asm_comment("bss");
  put_bss();
}

//

static const char *s_break_label;
static const char *s_continue_label;
int stackpos;

static const char *push_break_label(const char **save) {
  *save = s_break_label;
  return s_break_label = alloc_label();
}

static void pop_break_label(const char *save) {
  s_break_label = save;
}

static const char *push_continue_label(const char **save) {
  *save = s_continue_label;
  return s_continue_label = alloc_label();
}

static void pop_continue_label(const char *save) {
  s_continue_label = save;
}

static int arrange_variadic_func_params(Scope *scope) {
  // Arrange parameters increasing order in stack,
  // and each parameter occupies sizeof(intptr_t).
  for (int i = 0; i < scope->vars->len; ++i) {
    VarInfo *varinfo = (VarInfo*)scope->vars->data[i];
    varinfo->offset = (i - MAX_REG_ARGS) * WORD_SIZE;
  }
  return MAX_REG_ARGS * WORD_SIZE;
}

static size_t arrange_scope_vars(Defun *defun) {
  // Calc local variable offsets.
  // Map parameters from the bottom (to reduce offsets).
  size_t frame_size = 0;
  for (int i = 0; i < defun->all_scopes->len; ++i) {
    Scope *scope = (Scope*)defun->all_scopes->data[i];
    size_t scope_size = scope->parent != NULL ? scope->parent->size : 0;
    if (scope->vars != NULL) {
      if (i == 0) {  // Function parameters.
        if (defun->type->u.func.vaargs) {
          // Special arrangement for va_list.
          scope_size = arrange_variadic_func_params(scope);
        } else {
          for (int j = 0; j < scope->vars->len; ++j) {
            VarInfo *varinfo = (VarInfo*)scope->vars->data[j];
            if (j < MAX_REG_ARGS) {
              size_t size = type_size(varinfo->type);
              int align = align_size(varinfo->type);
              if (size < 1)
                size = 1;
              scope_size = ALIGN(scope_size + size, align);
              varinfo->offset = -scope_size;
            } else {
              // Assumes little endian, and put all types in WORD_SIZE.
              varinfo->offset = STACK_PARAM_BASE_OFFSET + j * WORD_SIZE;
            }
          }
        }
      } else {
        for (int j = 0; j < scope->vars->len; ++j) {
          VarInfo *varinfo = (VarInfo*)scope->vars->data[j];
          if (varinfo->flag & VF_STATIC)
            continue;  // Static variable is not allocated on stack.
          size_t size = type_size(varinfo->type);
          int align = align_size(varinfo->type);
          if (size < 1)
            size = 1;
          scope_size = ALIGN(scope_size + size, align);
          varinfo->offset = -scope_size;
        }
      }
    }
    scope->size = scope_size;
    if (frame_size < scope_size)
      frame_size = scope_size;
  }
  return ALIGN(frame_size, FRAME_ALIGN);
}

static void put_args_to_stack(Defun *defun) {
  // Store arguments into local frame.
  Vector *params = defun->params;
  int len = params != NULL ? params->len : 0;
  int n = defun->type->u.func.vaargs ? MAX_REG_ARGS : len;
  for (int i = 0; i < n; ++i) {
    const Type *type;
    int offset;
    if (i < len) {
      const VarInfo *varinfo = (const VarInfo*)params->data[i];
      type = varinfo->type;
      offset = varinfo->offset;
    } else {  // vaargs
      type = &tyLong;
      offset = (i - MAX_REG_ARGS) * WORD_SIZE;
    }

    int size = 0;
    switch (type->type) {
    case TY_NUM:
      switch (type->u.num.type) {
      case NUM_CHAR:  size = 1; break;
      case NUM_INT:
      case NUM_ENUM:
        size = 4;
        break;
      case NUM_LONG:  size = 8; break;
      default: break;
      }
      break;
    case TY_PTR:  size = 8; break;
    default:
      break;
    }
    switch (size) {
    case 1:
      switch (i) {
      case 0:  MOV_DIL_IND8_RBP(offset); break;
      case 1:  MOV_SIL_IND8_RBP(offset); break;
      case 2:  MOV_DL_IND8_RBP(offset); break;
      case 3:  MOV_CL_IND8_RBP(offset); break;
      case 4:  MOV_R8B_IND8_RBP(offset); break;
      case 5:  MOV_R9B_IND8_RBP(offset); break;
      default: break;
      }
      break;
    case 4:
      switch (i) {
      case 0:  MOV_EDI_IND8_RBP(offset); break;
      case 1:  MOV_ESI_IND8_RBP(offset); break;
      case 2:  MOV_EDX_IND8_RBP(offset); break;
      case 3:  MOV_ECX_IND8_RBP(offset); break;
      case 4:  MOV_R8D_IND8_RBP(offset); break;
      case 5:  MOV_R9D_IND8_RBP(offset); break;
      default: break;
      }
      break;
    case 8:
      switch (i) {
      case 0:  MOV_RDI_IND8_RBP(offset); break;
      case 1:  MOV_RSI_IND8_RBP(offset); break;
      case 2:  MOV_RDX_IND8_RBP(offset); break;
      case 3:  MOV_RCX_IND8_RBP(offset); break;
      case 4:  MOV_R8_IND8_RBP(offset); break;
      case 5:  MOV_R9_IND8_RBP(offset); break;
      default: break;
      }
      break;
    default:
      assert(false);
      break;
    }
  }
}

static bool is_funcall(Expr *expr, const char *funcname) {
  if (expr->type == EX_FUNCALL) {
    Expr *func = expr->u.funcall.func;
    if (func->type == EX_VARREF &&
        strcmp(func->u.varref.ident, funcname) == 0)
      return true;
  }
  return false;
}

static bool is_asm(Node *node) {
  return node->type == ND_EXPR &&
    is_funcall(node->u.expr, "__asm");
}

static void out_asm(Node *node) {
  Expr *funcall = node->u.expr;
  Vector *args = funcall->u.funcall.args;
  int len = args->len;

  Expr *arg0;
  if (len != 1 || (arg0 = (Expr*)args->data[0])->type != EX_STR)
    error("__asm takes string at 1st argument");
  else
    add_asm("%s", arg0->u.str.buf);
}

static void gen_nodes(Vector *nodes) {
  if (nodes == NULL)
    return;

  for (int i = 0, len = nodes->len; i < len; ++i) {
    Node *node = nodes->data[i];
    if (node == NULL)
      continue;
    if (is_asm(node))
      out_asm(node);
    else
      gen(node);
  }
}

static void gen_defun(Node *node) {
  assert(stackpos == 0);
  Defun *defun = node->u.defun;
  if (defun->top_scope == NULL)  // Prototype definition
    return;

  bool global = true;
  VarInfo *varinfo = find_global(defun->name);
  if (varinfo != NULL) {
    global = (varinfo->flag & VF_STATIC) == 0;
  }
  if (global)
    add_asm(".globl %s", defun->name);
  else
    add_asm_comment("%s: static func", defun->name);

  ADD_LABEL(defun->name);

  // Allocate labels for goto.
  if (defun->labels != NULL) {
    Map *labels = defun->labels;
    for (int i = 0, n = map_count(labels); i < n; ++i)
      labels->vals->data[i] = alloc_label();
  }

  size_t frame_size = arrange_scope_vars(defun);

  bool no_stmt = true;
  if (defun->stmts != NULL) {
    for (int i = 0; i < defun->stmts->len; ++i) {
      Node *node = defun->stmts->data[i];
      if (node == NULL)
        continue;
      if (!is_asm(node)) {
        no_stmt = false;
        break;
      }
    }
  }

  curfunc = defun;
  curscope = defun->top_scope;
  defun->ret_label = alloc_label();

  // Prologue
  // Allocate variable bufer.
  if (!no_stmt) {
    PUSH_RBP(); PUSH_STACK_POS();
    MOV_RSP_RBP();
    if (frame_size > 0) {
      SUB_IM32_RSP(frame_size);
      stackpos += frame_size;
    }

    put_args_to_stack(defun);
  }

  // Statements
  gen_nodes(defun->stmts);

  // Epilogue
  if (!no_stmt) {
    ADD_LABEL(defun->ret_label);
    MOV_RBP_RSP();
    stackpos -= frame_size;
    POP_RBP(); POP_STACK_POS();
  }
  RET();
  add_asm_comment(NULL);
  curfunc = NULL;
  curscope = NULL;
  assert(stackpos == 0);
}

static void gen_block(Node *node) {
  if (node->u.block.nodes != NULL) {
    if (node->u.block.scope != NULL) {
      assert(curscope == node->u.block.scope->parent);
      curscope = node->u.block.scope;
    }
    gen_nodes(node->u.block.nodes);
    if (node->u.block.scope != NULL)
      curscope = curscope->parent;
  }
}

static void gen_return(Node *node) {
  if (node->u.return_.val != NULL)
    gen_expr(node->u.return_.val);
  assert(curfunc != NULL);
  JMP32(curfunc->ret_label);
}

static void gen_if(Node *node) {
  const char *flabel = alloc_label();
  gen_cond_jmp(node->u.if_.cond, false, flabel);
  gen(node->u.if_.tblock);
  if (node->u.if_.fblock == NULL) {
    ADD_LABEL(flabel);
  } else {
    const char *nlabel = alloc_label();
    JMP32(nlabel);
    ADD_LABEL(flabel);
    gen(node->u.if_.fblock);
    ADD_LABEL(nlabel);
  }
}

static Vector *cur_case_values;
static Vector *cur_case_labels;

static void gen_switch(Node *node) {
  Vector *save_case_values = cur_case_values;
  Vector *save_case_labels = cur_case_labels;
  const char *save_break;
  const char *l_break = push_break_label(&save_break);

  Vector *labels = new_vector();
  Vector *case_values = node->u.switch_.case_values;
  int len = case_values->len;
  for (int i = 0; i < len; ++i) {
    const char *label = alloc_label();
    vec_push(labels, label);
  }
  vec_push(labels, alloc_label());  // len+0: Extra label for default.
  vec_push(labels, l_break);  // len+1: Extra label for break.

  Expr *value = node->u.switch_.value;
  gen_expr(value);

  enum NumType valtype = value->valType->u.num.type;
  for (int i = 0; i < len; ++i) {
    intptr_t x = (intptr_t)case_values->data[i];
    switch (valtype) {
    case NUM_CHAR:
      CMP_IM8_AL(x);
      break;
    case NUM_INT: case NUM_ENUM:
      CMP_IM32_EAX(x);
      break;
    case NUM_LONG:
      if (x <= 0x7fL && x >= -0x80L) {
        CMP_IM8_RAX(x);
      } else if (x <= 0x7fffffffL && x >= -0x80000000L) {
        CMP_IM32_RAX(x);
      } else {
        MOV_IM64_RDI(x);
        CMP_RDI_RAX();
      }
      break;
    default: assert(false); break;
    }
    JE32(labels->data[i]);
  }
  JMP32(labels->data[len]);

  cur_case_values = case_values;
  cur_case_labels = labels;

  gen(node->u.switch_.body);

  if (!node->u.switch_.has_default)
    ADD_LABEL(labels->data[len]);  // No default: Locate at the end of switch statement.
  ADD_LABEL(l_break);

  cur_case_values = save_case_values;
  cur_case_labels = save_case_labels;
  pop_break_label(save_break);
}

static void gen_case(Node *node) {
  assert(cur_case_values != NULL);
  assert(cur_case_labels != NULL);
  intptr_t x = node->u.case_.value;
  int i, len = cur_case_values->len;
  for (i = 0; i < len; ++i) {
    if ((intptr_t)cur_case_values->data[i] == x)
      break;
  }
  assert(i < len);
  assert(i < cur_case_labels->len);
  ADD_LABEL(cur_case_labels->data[i]);
}

static void gen_default(void) {
  assert(cur_case_values != NULL);
  assert(cur_case_labels != NULL);
  int i = cur_case_values->len;  // Label for default is stored at the size of values.
  assert(i < cur_case_labels->len);
  ADD_LABEL(cur_case_labels->data[i]);
}

static void gen_while(Node *node) {
  const char *save_break, *save_cont;
  const char *l_cond = push_continue_label(&save_cont);
  const char *l_break = push_break_label(&save_break);
  const char *l_loop = alloc_label();
  JMP32(l_cond);
  ADD_LABEL(l_loop);
  gen(node->u.while_.body);
  ADD_LABEL(l_cond);
  gen_cond_jmp(node->u.while_.cond, true, l_loop);
  ADD_LABEL(l_break);
  pop_continue_label(save_cont);
  pop_break_label(save_break);
}

static void gen_do_while(Node *node) {
  const char *save_break, *save_cont;
  const char *l_cond = push_continue_label(&save_cont);
  const char *l_break = push_break_label(&save_break);
  const char * l_loop = alloc_label();
  ADD_LABEL(l_loop);
  gen(node->u.while_.body);
  ADD_LABEL(l_cond);
  gen_cond_jmp(node->u.while_.cond, true, l_loop);
  ADD_LABEL(l_break);
  pop_continue_label(save_cont);
  pop_break_label(save_break);
}

static void gen_for(Node *node) {
  const char *save_break, *save_cont;
  const char *l_continue = push_continue_label(&save_cont);
  const char *l_break = push_break_label(&save_break);
  const char * l_cond = alloc_label();
  if (node->u.for_.pre != NULL)
    gen_expr(node->u.for_.pre);
  ADD_LABEL(l_cond);
  if (node->u.for_.cond != NULL) {
    gen_cond_jmp(node->u.for_.cond, false, l_break);
  }
  gen(node->u.for_.body);
  ADD_LABEL(l_continue);
  if (node->u.for_.post != NULL)
    gen_expr(node->u.for_.post);
  JMP32(l_cond);
  ADD_LABEL(l_break);
  pop_continue_label(save_cont);
  pop_break_label(save_break);
}

static void gen_break(void) {
  assert(s_break_label != NULL);
  JMP32(s_break_label);
}

static void gen_continue(void) {
  assert(s_continue_label != NULL);
  JMP32(s_continue_label);
}

static void gen_goto(Node *node) {
  assert(curfunc->labels != NULL);
  const char *label = map_get(curfunc->labels, node->u.goto_.ident);
  assert(label != NULL);
  JMP32(label);
}

static void gen_label(Node *node) {
  assert(curfunc->labels != NULL);
  const char *label = map_get(curfunc->labels, node->u.label.name);
  assert(label != NULL);
  ADD_LABEL(label);
  gen(node->u.label.stmt);
}

static void gen_clear_local_var(const VarInfo *varinfo) {
  // Fill with zeros regardless of variable type.
  int offset = varinfo->offset;
  const char *loop = alloc_label();
  LEA_OFS32_RBP_RSI(offset);
  MOV_IM32_EDI(type_size(varinfo->type));
  XOR_AL_AL();
  ADD_LABEL(loop);
  MOV_AL_IND_RSI();
  INC_RSI();
  DEC_EDI();
  JNE8(loop);
}

static void gen_vardecl(Node *node) {
  if (curfunc != NULL) {
    Vector *decls = node->u.vardecl.decls;
    for (int i = 0; i < decls->len; ++i) {
      VarDecl *decl = decls->data[i];
      if (decl->init == NULL)
        continue;
      VarInfo *varinfo = scope_find(curscope, decl->ident->u.ident);
      if (varinfo == NULL || (varinfo->flag & VF_STATIC) ||
          !(varinfo->type->type == TY_STRUCT ||
            varinfo->type->type == TY_ARRAY))
        continue;
      gen_clear_local_var(varinfo);
    }
  }
  gen_nodes(node->u.vardecl.inits);
}

static void gen_toplevel(Node *node) {
  add_asm(".text");
  gen_nodes(node->u.toplevel.nodes);
}

void gen(Node *node) {
  if (node == NULL)
    return;

  switch (node->type) {
  case ND_EXPR:  gen_expr(node->u.expr); break;
  case ND_DEFUN:  gen_defun(node); break;
  case ND_RETURN:  gen_return(node); break;
  case ND_BLOCK:  gen_block(node); break;
  case ND_IF:  gen_if(node); break;
  case ND_SWITCH:  gen_switch(node); break;
  case ND_CASE:  gen_case(node); break;
  case ND_DEFAULT:  gen_default(); break;
  case ND_WHILE:  gen_while(node); break;
  case ND_DO_WHILE:  gen_do_while(node); break;
  case ND_FOR:  gen_for(node); break;
  case ND_BREAK:  gen_break(); break;
  case ND_CONTINUE:  gen_continue(); break;
  case ND_GOTO:  gen_goto(node); break;
  case ND_LABEL:  gen_label(node); break;
  case ND_VARDECL:  gen_vardecl(node); break;
  case ND_TOPLEVEL:  gen_toplevel(node); break;

  default:
    error("Unhandled node: %d", node->type);
    break;
  }
}

void init_gen(FILE *fp) {
  asm_fp = fp;
}
#include "codegen.h"

#include <assert.h>
#include <stdlib.h>  // malloc

#include "expr.h"
#include "sema.h"
#include "type.h"
#include "util.h"
#include "var.h"
#include "x86_64.h"

static void gen_lval(Expr *expr);

static bool is_im32(intptr_t x) {
  return x < (1L << 32) && x >= -(1L << 32);
}

static enum ExprType flip_cmp(enum ExprType type) {
  assert(EX_EQ <= type && type <= EX_LE);
  if (type >= EX_LT)
    type = EX_LE - (type - EX_LT);
  return type;
}

static enum ExprType gen_compare_expr(enum ExprType type, Expr *lhs, Expr *rhs) {
  const Type *ltype = lhs->valType;
  UNUSED(ltype);
  assert(ltype->type == rhs->valType->type);

  if (rhs->type != EX_NUM && lhs->type == EX_NUM) {
    Expr *tmp = lhs;
    lhs = rhs;
    rhs = tmp;
    type = flip_cmp(type);
  }

  enum NumType numtype;
  switch (lhs->valType->type) {
  case TY_NUM:
    numtype = lhs->valType->u.num.type;
    break;
  default:
    assert(false);
    // Fallthrough to avoid compile error.
  case TY_PTR:
    numtype = NUM_LONG;
    break;
  }

  gen_expr(lhs);
  if (rhs->type == EX_NUM && rhs->u.num.ival == 0 &&
      (type == EX_EQ || type == EX_NE)) {
    switch (numtype) {
    case NUM_CHAR: TEST_AL_AL(); break;
    case NUM_SHORT: TEST_AX_AX(); break;
    case NUM_INT: case NUM_ENUM:
      TEST_EAX_EAX();
      break;
    case NUM_LONG:
      TEST_RAX_RAX();
      break;
    default: assert(false); break;
    }
  } else if (rhs->type == EX_NUM && !(numtype == NUM_LONG && is_im32(rhs->u.num.ival))) {
    switch (numtype) {
    case NUM_CHAR: CMP_IM8_AL(rhs->u.num.ival); break;
    case NUM_SHORT: CMP_IM16_AX(rhs->u.num.ival); break;
    case NUM_INT: case NUM_ENUM:
      CMP_IM32_EAX(rhs->u.num.ival);
      break;
    case NUM_LONG:
      CMP_IM32_RAX(rhs->u.num.ival);
      break;
    default: assert(false); break;
    }
  } else {
    PUSH_RAX(); PUSH_STACK_POS();
    gen_expr(rhs);
    POP_RDI(); POP_STACK_POS();

    switch (numtype) {
    case NUM_CHAR: CMP_AL_DIL(); break;
    case NUM_SHORT: CMP_AX_DI(); break;
    case NUM_INT: case NUM_ENUM:
      CMP_EAX_EDI();
      break;
    case NUM_LONG: CMP_RAX_RDI(); break;
    default: assert(false); break;
    }
  }

  return type;
}

// test %eax, %eax, and so on.
static void gen_test_opcode(const Type *type) {
  switch (type->type) {
  case TY_NUM:
    switch (type->u.num.type) {
    case NUM_CHAR:  TEST_AL_AL(); break;
    case NUM_SHORT: TEST_AX_AX(); break;
    case NUM_INT: case NUM_ENUM:
      TEST_EAX_EAX();
      break;
    case NUM_LONG:
      TEST_RAX_RAX();
      break;
    default: assert(false); break;
    }
    break;
  case TY_PTR: case TY_ARRAY: case TY_FUNC:
    TEST_RAX_RAX();
    break;
  default: assert(false); break;
  }
}

void gen_cond_jmp(Expr *cond, bool tf, const char *label) {
  // Local optimization: if `cond` is compare expression, then
  // jump using flags after CMP directly.
  switch (cond->type) {
  case EX_NUM:
    if (cond->u.num.ival == 0)
      tf = !tf;
    if (tf)
      JMP32(label);
    return;

  case EX_EQ:
  case EX_NE:
    {
      enum ExprType type = gen_compare_expr(cond->type, cond->u.bop.lhs, cond->u.bop.rhs);
      if (type != EX_EQ)
        tf = !tf;
      if (tf)
        JE32(label);
      else
        JNE32(label);
      return;
    }
  case EX_LT:
  case EX_GT:
  case EX_LE:
  case EX_GE:
    {
      enum ExprType type = gen_compare_expr(cond->type, cond->u.bop.lhs, cond->u.bop.rhs);
      switch (type) {
      case EX_LT:
      case EX_GE:
        if (cond->type != EX_LT)
          tf = !tf;
        if (tf)
          JL32(label);
        else
          JGE32(label);
        break;
      case EX_GT:
      case EX_LE:
        if (cond->type != EX_GT)
          tf = !tf;
        if (tf)
          JG32(label);
        else
          JLE32(label);
        break;
      default:  assert(false); break;
      }
    }
    return;
  case EX_NOT:
    gen_cond_jmp(cond->u.unary.sub, !tf, label);
    return;
  case EX_LOGAND:
    if (!tf) {
      gen_cond_jmp(cond->u.bop.lhs, false, label);
      gen_cond_jmp(cond->u.bop.rhs, false, label);
    } else {
      const char *skip = alloc_label();
      gen_cond_jmp(cond->u.bop.lhs, false, skip);
      gen_cond_jmp(cond->u.bop.rhs, true, label);
      ADD_LABEL(skip);
    }
    return;
  case EX_LOGIOR:
    if (tf) {
      gen_cond_jmp(cond->u.bop.lhs, true, label);
      gen_cond_jmp(cond->u.bop.rhs, true, label);
    } else {
      const char *skip = alloc_label();
      gen_cond_jmp(cond->u.bop.lhs, true, skip);
      gen_cond_jmp(cond->u.bop.rhs, false, label);
      ADD_LABEL(skip);
    }
    return;
  default:
    break;
  }

  gen_expr(cond);
  gen_test_opcode(cond->valType);

  if (tf)
    JNE32(label);
  else
    JE32(label);
}

static void gen_cast(const Type *ltypep, const Type *rtypep) {
  enum eType ltype = ltypep->type;
  enum eType rtype = rtypep->type;

  if (ltype == rtype) {
    if (ltype == TY_NUM) {
      enum NumType lnumtype = ltypep->u.num.type;
      enum NumType rnumtype = rtypep->u.num.type;
      if (lnumtype == rnumtype)
        return;

      switch (lnumtype) {
      case NUM_CHAR:
        switch (rnumtype) {
        case NUM_SHORT: return;
        case NUM_INT:   return;
        case NUM_LONG:  return;
        default: assert(false); break;
        }
        break;
      case NUM_SHORT:
        switch (rnumtype) {
        case NUM_CHAR: MOVSX_AL_AX(); return;
        case NUM_INT:  return;
        case NUM_LONG: return;
        default: assert(false); break;
        }
        break;
      case NUM_INT: case NUM_ENUM:
        switch (rnumtype) {
        case NUM_CHAR:  MOVSX_AL_EAX(); return;
        case NUM_SHORT: MOVSX_AX_EAX(); return;
        case NUM_INT:   return;
        case NUM_LONG:  return;
        case NUM_ENUM:  return;
        default: assert(false); break;
        }
        break;
      case NUM_LONG:
        switch (rnumtype) {
        case NUM_CHAR:  MOVSX_AL_RAX(); return;
        case NUM_SHORT: MOVSX_AX_RAX(); return;
        case NUM_INT: case NUM_ENUM:
          MOVSX_EAX_RAX();
          return;
        default: assert(false); break;
        }
        break;
      default: assert(false); break;
      }
    }
    return;
  }

  switch (ltype) {
  case TY_VOID:
    return;
  case TY_NUM:
    switch (rtype) {
    case TY_PTR:
    case TY_ARRAY:
      if (ltypep->u.num.type == NUM_LONG)
        return;
      break;
    default: assert(false); break;
    }
    break;
  case TY_PTR:
    switch (rtype) {
    case TY_NUM:
      switch (rtypep->u.num.type) {
      case NUM_INT:   MOVSX_EAX_RAX(); return;
      case NUM_LONG:  return;
      default: break;
      }
      break;
    case TY_ARRAY: case TY_FUNC:
      return;
    default: break;
    }
    assert(false);
    break;
  default: assert(false); break;
  }

  fprintf(stderr, "ltype=%d, rtype=%d\n", ltype, rtype);
  assert(!"Cast failed");
}

static void gen_rval(Expr *expr) {
  gen_expr(expr);  // ?
}

static void gen_ref(Expr *expr) {
  gen_lval(expr);
}

static void gen_lval(Expr *expr) {
  switch (expr->type) {
  case EX_VARREF:
    if (expr->u.varref.global) {
      LEA_LABEL32_RIP_RAX(expr->u.varref.ident);
    } else {
      VarInfo *varinfo = scope_find(curscope, expr->u.varref.ident);
      assert(varinfo != NULL);
      assert(!(varinfo->flag & VF_STATIC));
      int offset = varinfo->offset;
      LEA_OFS32_RBP_RAX(offset);
    }
    break;
  case EX_DEREF:
    gen_rval(expr->u.unary.sub);
    break;
  case EX_MEMBER:
    {
      const Type *type = expr->u.member.target->valType;
      if (type->type == TY_PTR || type->type == TY_ARRAY)
        type = type->u.pa.ptrof;
      assert(type->type == TY_STRUCT);
      calc_struct_size(type->u.struct_.info);
      Vector *members = type->u.struct_.info->members;
      VarInfo *varinfo = (VarInfo*)members->data[expr->u.member.index];

      if (expr->u.member.target->valType->type == TY_PTR)
        gen_expr(expr->u.member.target);
      else
        gen_ref(expr->u.member.target);
      if (varinfo->offset != 0)
        ADD_IM32_RAX(varinfo->offset);
    }
    break;
  default:
    error("No lvalue: %d", expr->type);
    break;
  }
}

static void gen_varref(Expr *expr) {
  gen_lval(expr);
  switch (expr->valType->type) {
  case TY_NUM:
    switch (expr->valType->u.num.type) {
    case NUM_CHAR:  MOV_IND_RAX_AL(); break;
    case NUM_SHORT: MOV_IND_RAX_AX(); break;
    case NUM_INT: case NUM_ENUM:
      MOV_IND_RAX_EAX();
      break;
    case NUM_LONG:  MOV_IND_RAX_RAX(); break;
    default: assert(false); break;
    }
    break;
  case TY_PTR: MOV_IND_RAX_RAX(); break;
  case TY_ARRAY: break;  // Use variable address as a pointer.
  case TY_FUNC:  break;
  default: assert(false); break;
  }
}

static void gen_ternary(Expr *expr) {
  const char *nlabel = alloc_label();
  const char *flabel = alloc_label();
  gen_cond_jmp(expr->u.ternary.cond, false, flabel);
  gen_expr(expr->u.ternary.tval);
  JMP32(nlabel);
  ADD_LABEL(flabel);
  gen_expr(expr->u.ternary.fval);
  ADD_LABEL(nlabel);
}

static void gen_funcall(Expr *expr) {
  Expr *func = expr->u.funcall.func;
  Vector *args = expr->u.funcall.args;
  int arg_count = args != NULL ? args->len : 0;

  int stack_args = MAX(arg_count - MAX_REG_ARGS, 0);
  bool align_stack = ((stackpos + stack_args * WORD_SIZE) & 15) != 0;
  if (align_stack) {
    SUB_IM8_RSP(8); PUSH_STACK_POS();
  }

  if (args != NULL) {
    int len = args->len;
    if (len >= MAX_REG_ARGS) {
      bool vaargs = false;
      if (func->type == EX_VARREF && func->u.varref.global) {
        VarInfo *varinfo = find_global(func->u.varref.ident);
        assert(varinfo != NULL && varinfo->type->type == TY_FUNC);
        vaargs = varinfo->type->u.func.vaargs;
      } else {
        // TODO:
      }

      if (vaargs)
        error("Param count exceeds %d (%d)", MAX_REG_ARGS, len);
    }

    for (int i = len; --i >= 0; ) {
      gen_expr((Expr*)args->data[i]);
      PUSH_RAX(); PUSH_STACK_POS();
    }

    int reg_args = MIN(len, MAX_REG_ARGS);
    for (int i = 0; i < reg_args; ++i) {
      switch (i) {
      case 0:  POP_RDI(); break;
      case 1:  POP_RSI(); break;
      case 2:  POP_RDX(); break;
      case 3:  POP_RCX(); break;
      case 4:  POP_R8(); break;
      case 5:  POP_R9(); break;
      default: break;
      }
      POP_STACK_POS();
    }
  }

  if (func->type == EX_VARREF && func->u.varref.global) {
    CALL(func->u.varref.ident);
  } else {
    gen_expr(func);
    CALL_IND_RAX();
  }

  for (int i = 0; i < stack_args; ++i)
    POP_STACK_POS();

  if (align_stack) {
    ADD_IM8_RSP(8); POP_STACK_POS();
  }
}

void gen_arith(enum ExprType exprType, const Type *valType, const Type *rhsType) {
  // lhs=rax, rhs=rdi, result=rax

  switch (exprType) {
  case EX_ADD:
    switch (valType->type) {
    case TY_NUM:
      switch (valType->u.num.type) {
      case NUM_CHAR:  ADD_DIL_AL(); break;
      case NUM_SHORT: ADD_DI_AX(); break;
      case NUM_INT:   ADD_EDI_EAX(); break;
      case NUM_LONG:  ADD_RDI_RAX(); break;
      default: assert(false); break;
      }
      break;
    case TY_PTR:  ADD_RDI_RAX(); break;
    default: assert(false); break;
    }
    break;

  case EX_SUB:
    switch (valType->type) {
    case TY_NUM:
      switch (valType->u.num.type) {
      case NUM_CHAR:  SUB_DIL_AL(); break;
      case NUM_SHORT: SUB_DI_AX(); break;
      case NUM_INT:   SUB_EDI_EAX(); break;
      case NUM_LONG:  SUB_RDI_RAX(); break;
      default: assert(false); break;
      }
      break;
    case TY_PTR:  SUB_RDI_RAX(); break;
    default: assert(false); break;
    }
    break;

  case EX_MUL:
    assert(valType->type == TY_NUM);
    switch (valType->u.num.type) {
    case NUM_CHAR:  MUL_DIL(); break;
    case NUM_SHORT: MUL_DI(); break;
    case NUM_INT:   MUL_EDI(); break;
    case NUM_LONG:  MUL_RDI(); break;
    default: assert(false); break;
    }
    break;

  case EX_DIV:
    XOR_EDX_EDX();  // MOV_IM32_RDX(0);
    assert(valType->type == TY_NUM);
    switch (valType->u.num.type) {
    case NUM_CHAR:  DIV_DIL(); break;
    case NUM_SHORT: DIV_DI(); break;
    case NUM_INT:   DIV_EDI(); break;
    case NUM_LONG:  DIV_RDI(); break;
    default: assert(false); break;
    }
    break;

  case EX_MOD:
    XOR_EDX_EDX();  // MOV_IM32_RDX(0);
    assert(valType->type == TY_NUM);
    switch (valType->u.num.type) {
    case NUM_CHAR:  DIV_DIL(); MOV_DL_AL(); break;
    case NUM_SHORT: DIV_DI();  MOV_DX_AX(); break;
    case NUM_INT:   DIV_EDI(); MOV_EDX_EAX(); break;
    case NUM_LONG:  DIV_RDI(); MOV_RDX_RAX(); break;
    default: assert(false); break;
    }
    break;

  case EX_BITAND:
    assert(valType->type == TY_NUM);
    switch (valType->u.num.type) {
    case NUM_CHAR:  AND_DIL_AL(); break;
    case NUM_SHORT: AND_DI_AX(); break;
    case NUM_INT:   AND_EDI_EAX(); break;
    case NUM_LONG:  AND_RDI_RAX(); break;
    default: assert(false); break;
    }
    break;

  case EX_BITOR:
    assert(valType->type == TY_NUM);
    switch (valType->u.num.type) {
    case NUM_CHAR:  OR_DIL_AL(); break;
    case NUM_SHORT: OR_DI_AX(); break;
    case NUM_INT: case NUM_ENUM:
      OR_EDI_EAX();
      break;
    case NUM_LONG:  OR_RDI_RAX(); break;
    default: assert(false); break;
    }
    break;

  case EX_BITXOR:
    assert(valType->type == TY_NUM);
    switch (valType->u.num.type) {
    case NUM_CHAR:  XOR_DIL_AL(); break;
    case NUM_SHORT: XOR_DI_AX(); break;
    case NUM_INT:   XOR_EDI_EAX(); break;
    case NUM_LONG:  XOR_RDI_RAX(); break;
    default: assert(false); break;
    }
    break;

  case EX_LSHIFT:
  case EX_RSHIFT:
    assert(rhsType->type == TY_NUM);
    switch (rhsType->u.num.type) {
    case NUM_CHAR:  MOV_DIL_CL(); break;
    case NUM_SHORT: MOV_DI_CX(); break;
    case NUM_INT:   MOV_EDI_ECX(); break;
    case NUM_LONG:  MOV_RDI_RCX(); break;
    default: assert(false); break;
    }
    assert(valType->type == TY_NUM);
    if (exprType == EX_LSHIFT) {
      switch (valType->u.num.type) {
      case NUM_CHAR:  SHL_CL_AL(); break;
      case NUM_SHORT: SHL_CL_AX(); break;
      case NUM_INT:   SHL_CL_EAX(); break;
      case NUM_LONG:  SHL_CL_RAX(); break;
      default: assert(false); break;
      }
    } else {
      switch (valType->u.num.type) {
      case NUM_CHAR:  SHR_CL_AL(); break;
      case NUM_SHORT: SHR_CL_AX(); break;
      case NUM_INT:   SHR_CL_EAX(); break;
      case NUM_LONG:  SHR_CL_RAX(); break;
      default: assert(false); break;
      }
    }
    break;

  default:
    assert(false);
    break;
  }
}

static void gen_num(enum NumType numtype, intptr_t value) {
  switch (numtype) {
  case NUM_CHAR:
    if (value == 0)
      XOR_AL_AL();
    else
      MOV_IM8_AL(value);
    return;

  case NUM_SHORT:
    if (value == 0)
      XOR_AX_AX();
    else
      MOV_IM16_AX(value);
    return;

  case NUM_INT: case NUM_ENUM:
    if (value == 0)
      XOR_EAX_EAX();
    else
      MOV_IM32_EAX(value);
    return;

  case NUM_LONG:
    if (value == 0)
      XOR_EAX_EAX();  // upper 32bit is also cleared.
    else if (value <= 0x7fffffffL && value >= -0x80000000L)
      MOV_IM32_RAX(value);
    else
      MOV_IM64_RAX(value);
    return;

  default: assert(false); break;
  }
}

void gen_expr(Expr *expr) {
  switch (expr->type) {
  case EX_NUM:
    assert(expr->valType->type == TY_NUM);
    gen_num(expr->valType->u.num.type, expr->u.num.ival);
    break;

  case EX_STR:
    {
      Initializer *init = malloc(sizeof(*init));
      init->type = vSingle;
      init->u.single = expr;

      // Create string and point to it.
      const char * label = alloc_label();
      Type* strtype = arrayof(&tyChar, expr->u.str.size);
      VarInfo *varinfo = define_global(strtype, VF_CONST | VF_STATIC, NULL, label);

      varinfo->u.g.init = init;

      LEA_LABEL32_RIP_RAX(label);
    }
    return;

  case EX_SIZEOF:
    {
      size_t size = type_size(expr->u.sizeof_.type);
      if (size <= 0x7fffffffL)
        MOV_IM32_RAX(size);
      else
        MOV_IM64_RAX(size);
    }
    return;

  case EX_VARREF:
    gen_varref(expr);
    return;

  case EX_REF:
    gen_ref(expr->u.unary.sub);
    return;

  case EX_DEREF:
    gen_rval(expr->u.unary.sub);
    switch (expr->valType->type) {
    case TY_NUM:
      switch (expr->valType->u.num.type) {
      case NUM_CHAR:  MOV_IND_RAX_AL(); break;
      case NUM_SHORT: MOV_IND_RAX_AX(); break;
      case NUM_INT: case NUM_ENUM:
        MOV_IND_RAX_EAX();
        break;
      case NUM_LONG:  MOV_IND_RAX_RAX(); break;
      default: assert(false); break;
      }
      break;
    case TY_PTR:  MOV_IND_RAX_RAX(); break;
    case TY_ARRAY: break;
    default: assert(false); break;
    }
    return;

  case EX_MEMBER:
    gen_lval(expr);
    switch (expr->valType->type) {
    case TY_NUM:
      switch (expr->valType->u.num.type) {
      case NUM_CHAR:  MOV_IND_RAX_AL(); break;
      case NUM_SHORT: MOV_IND_RAX_AX(); break;
      case NUM_INT: case NUM_ENUM:
        MOV_IND_RAX_EAX();
        break;
      case NUM_LONG:  MOV_IND_RAX_RAX(); break;
      default: assert(false); break;
      }
      break;
    case TY_PTR:  MOV_IND_RAX_RAX(); break;
    case TY_ARRAY:
      break;
    default:
      assert(false);
      break;
    }
    return;

  case EX_COMMA:
    {
      Vector *list = expr->u.comma.list;
      for (int i = 0, len = list->len; i < len; ++i)
        gen_expr(list->data[i]);
    }
    break;

  case EX_TERNARY:
    gen_ternary(expr);
    break;

  case EX_CAST:
    if (expr->u.cast.sub->type == EX_NUM) {
      assert(expr->u.cast.sub->valType->type == TY_NUM);
      intptr_t value = expr->u.cast.sub->u.num.ival;
      enum NumType numtype = expr->u.cast.sub->valType->u.num.type;
      switch (numtype) {
      case NUM_CHAR:
        value = (int8_t)value;
        break;
      case NUM_SHORT:
        value = (int16_t)value;
        break;
      case NUM_INT: case NUM_ENUM:
        value = (int32_t)value;
        break;
      case NUM_LONG:
        value = (int64_t)value;
        break;
      default:
        assert(false);
        value = -1;
        break;
      }

      enum NumType targettype;
      switch (expr->valType->type) {
      case TY_NUM:
        targettype = expr->valType->u.num.type;
        break;
      default:
        assert(false);
        // Fallthrough to avoid compile error.
      case TY_PTR:
        targettype = NUM_LONG;
        break;
      }
      gen_num(targettype, value);
    } else {
      gen_expr(expr->u.cast.sub);
      gen_cast(expr->valType, expr->u.cast.sub->valType);
    }
    break;

  case EX_ASSIGN:
    gen_lval(expr->u.bop.lhs);
    PUSH_RAX(); PUSH_STACK_POS();
    gen_expr(expr->u.bop.rhs);

    POP_RDI(); POP_STACK_POS();
    switch (expr->u.bop.lhs->valType->type) {
    case TY_NUM:
      switch (expr->u.bop.lhs->valType->u.num.type) {
      case NUM_CHAR:  MOV_AL_IND_RDI(); break;
      case NUM_SHORT: MOV_AX_IND_RDI(); break;
      case NUM_INT: case NUM_ENUM:
        MOV_EAX_IND_RDI();
        break;
      case NUM_LONG:  MOV_RAX_IND_RDI(); break;
      default: assert(false); break;
      }
      break;
    case TY_PTR:  MOV_RAX_IND_RDI(); break;
    default: assert(false); break;
    }
    return;

  case EX_ASSIGN_WITH:
    {
      Expr *sub = expr->u.unary.sub;
      gen_expr(sub->u.bop.rhs);
      PUSH_RAX(); PUSH_STACK_POS();
      gen_lval(sub->u.bop.lhs);
      MOV_RAX_RSI();  // Save lhs address to %rsi.

      // Move lhs to %?ax
      switch (expr->u.bop.lhs->valType->type) {
      case TY_NUM:
        switch (expr->u.bop.lhs->valType->u.num.type) {
        case NUM_CHAR:  MOV_IND_RAX_AL(); break;
        case NUM_SHORT: MOV_IND_RAX_AX(); break;
        case NUM_INT:   MOV_IND_RAX_EAX(); break;
        case NUM_LONG:  MOV_IND_RAX_RAX(); break;
        default: assert(false); break;
        }
        break;
      case TY_PTR:  MOV_IND_RAX_RAX(); break;
      default: assert(false); break;
      }

      POP_RDI(); POP_STACK_POS();  // %rdi=rhs
      gen_arith(sub->type, sub->valType, sub->u.bop.rhs->valType);
      gen_cast(expr->valType, sub->valType);

      switch (expr->valType->type) {
      case TY_NUM:
        switch (expr->valType->u.num.type) {
        case NUM_CHAR:  MOV_AL_IND_RSI(); break;
        case NUM_SHORT: MOV_AX_IND_RSI(); break;
        case NUM_INT:   MOV_EAX_IND_RSI(); break;
        case NUM_LONG:  MOV_RAX_IND_RSI(); break;
        default: assert(false); break;
        }
        break;
      case TY_PTR:  MOV_RAX_IND_RSI(); break;
      default: assert(false); break;
      }
    }
    return;

  case EX_PREINC:
  case EX_PREDEC:
    gen_lval(expr->u.unary.sub);
    switch (expr->valType->type) {
    case TY_NUM:
      switch (expr->valType->u.num.type) {
      case NUM_CHAR:
        if (expr->type == EX_PREINC)  INCB_IND_RAX();
        else                          DECB_IND_RAX();
        MOV_IND_RAX_AL();
        break;
      case NUM_SHORT:
        if (expr->type == EX_PREINC)  INCW_IND_RAX();
        else                          DECW_IND_RAX();
        MOV_IND_RAX_AX();
        break;
      case NUM_INT:
        if (expr->type == EX_PREINC)  INCL_IND_RAX();
        else                          DECL_IND_RAX();
        MOV_IND_RAX_EAX();
        break;
      case NUM_LONG:
        if (expr->type == EX_PREINC)  INCQ_IND_RAX();
        else                          DECQ_IND_RAX();
        MOV_IND_RAX_RAX();
        break;
      default: assert(false); break;
      }
      break;
    case TY_PTR:
      {
        MOV_RAX_RDI();
        size_t size = type_size(expr->valType->u.pa.ptrof);
        MOV_IM32_RAX(expr->type == EX_PREINC ? size : -size);
        ADD_IND_RDI_RAX();
        MOV_RAX_IND_RDI();
      }
      break;
    default:
      assert(false);
      break;
    }
    return;

  case EX_POSTINC:
  case EX_POSTDEC:
    gen_lval(expr->u.unary.sub);
    switch (expr->valType->type) {
    case TY_NUM:
      switch (expr->valType->u.num.type) {
      case NUM_CHAR:
        MOV_IND_RAX_DIL();
        if (expr->type == EX_POSTINC)  INCB_IND_RAX();
        else                           DECB_IND_RAX();
        MOV_DIL_AL();
        break;
      case NUM_SHORT:
        MOV_IND_RAX_DI();
        if (expr->type == EX_POSTINC)  INCW_IND_RAX();
        else                           DECW_IND_RAX();
        MOV_DI_AX();
        break;
      case NUM_INT:
        MOV_IND_RAX_EDI();
        if (expr->type == EX_POSTINC)  INCL_IND_RAX();
        else                           DECL_IND_RAX();
        MOV_EDI_EAX();
        break;
      case NUM_LONG:
        MOV_IND_RAX_RDI();
        if (expr->type == EX_POSTINC)  INCQ_IND_RAX();
        else                           DECQ_IND_RAX();
        MOV_RDI_RAX();
        break;
      default: assert(false); break;
      }
      break;
    case TY_PTR:
      {
        MOV_IND_RAX_RDI();
        size_t size = type_size(expr->valType->u.pa.ptrof);
        assert(size < ((size_t)1 << 31));  // TODO:
        if (expr->type == EX_POSTINC) {
          if (size < 256)  ADDQ_IM8_IND_RAX(size);
          else             ADDQ_IM32_IND_RAX(size);
        } else {
          if (size < 256)  SUBQ_IM8_IND_RAX(size);
          else             SUBQ_IM32_IND_RAX(size);
        }
        MOV_RDI_RAX();
      }
      break;
    default:
      assert(false);
      break;
    }
    return;

  case EX_FUNCALL:
    gen_funcall(expr);
    return;

  case EX_NEG:
    gen_expr(expr->u.unary.sub);
    assert(expr->valType->type == TY_NUM);
    switch (expr->u.unary.sub->valType->u.num.type) {
    case NUM_CHAR:   NEG_AL(); break;
    case NUM_SHORT:  NEG_AX(); break;
    case NUM_INT:    NEG_EAX(); break;
    case NUM_LONG:   NEG_RAX(); break;
    default:  assert(false); break;
    }
    break;

  case EX_NOT:
    gen_expr(expr->u.unary.sub);
    switch (expr->u.unary.sub->valType->type) {
    case TY_NUM:
      switch (expr->u.unary.sub->valType->u.num.type) {
      case NUM_CHAR:   TEST_AL_AL(); break;
      case NUM_SHORT:  TEST_AX_AX(); break;
      case NUM_INT:    TEST_EAX_EAX(); break;
      case NUM_LONG:   TEST_RAX_RAX(); break;
      default:  assert(false); break;
      }
      break;
    case TY_PTR: case TY_ARRAY: case TY_FUNC:
      TEST_RAX_RAX();
      break;
    default:  assert(false); break;
    }
    SETE_AL();
    MOVSX_AL_EAX();
    break;

  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_GT:
  case EX_LE:
  case EX_GE:
    {
      enum ExprType type = gen_compare_expr(expr->type, expr->u.bop.lhs, expr->u.bop.rhs);
      switch (type) {
      case EX_EQ:  SETE_AL(); break;
      case EX_NE:  SETNE_AL(); break;
      case EX_LT:  SETL_AL(); break;
      case EX_GT:  SETG_AL(); break;
      case EX_LE:  SETLE_AL(); break;
      case EX_GE:  SETGE_AL(); break;
      default: assert(false); break;
      }
    }
    MOVSX_AL_EAX();
    return;

  case EX_LOGAND:
    {
      const char *l_false = alloc_label();
      const char *l_next = alloc_label();
      gen_cond_jmp(expr->u.bop.lhs, false, l_false);
      gen_cond_jmp(expr->u.bop.rhs, false, l_false);
      MOV_IM32_EAX(1);
      JMP8(l_next);
      ADD_LABEL(l_false);
      XOR_EAX_EAX();  // 0
      ADD_LABEL(l_next);
    }
    return;

  case EX_LOGIOR:
    {
      const char *l_true = alloc_label();
      const char *l_next = alloc_label();
      gen_cond_jmp(expr->u.bop.lhs, true, l_true);
      gen_cond_jmp(expr->u.bop.rhs, true, l_true);
      XOR_EAX_EAX();  // 0
      JMP8(l_next);
      ADD_LABEL(l_true);
      MOV_IM32_EAX(1);
      ADD_LABEL(l_next);
    }
    return;

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
    gen_expr(expr->u.bop.rhs);
    PUSH_RAX(); PUSH_STACK_POS();
    gen_expr(expr->u.bop.lhs);

    POP_RDI(); POP_STACK_POS();

    gen_arith(expr->type, expr->valType, expr->u.bop.rhs->valType);
    return;

  default:
    fprintf(stderr, "Expr type=%d, ", expr->type);
    assert(!"Unhandled in gen_expr");
    break;
  }
}
#include "stdarg.h"
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"

#include "codegen.h"
#include "expr.h"
#include "lexer.h"
#include "parser.h"
#include "sema.h"
#include "type.h"
#include "util.h"
#include "var.h"

////////////////////////////////////////////////

static void init_compiler(FILE *fp) {
  init_gen(fp);
  enum_map = new_map();
  enum_value_map = new_map();
  struct_map = new_map();
  typedef_map = new_map();
  gvar_map = new_map();
}

static void compile(FILE *fp, const char *filename) {
  init_lexer(fp, filename);
  Node *node = parse_program();
  node = sema(node);
  gen(node);
}

static const char LOCAL_LABEL_PREFIX[] = "--local-label-prefix=";

int main(int argc, char* argv[]) {
  int iarg;

  for (iarg = 1; iarg < argc; ++iarg) {
    if (*argv[iarg] != '-')
      break;
    if (strncmp(argv[iarg], LOCAL_LABEL_PREFIX, sizeof(LOCAL_LABEL_PREFIX) - 1) == 0) {
      set_local_label_prefix(&argv[iarg][sizeof(LOCAL_LABEL_PREFIX) - 1]);
    }
  }

  // Compile.
  init_compiler(stdout);

  // Test.
  define_global(new_func_type(&tyVoid, NULL, true), 0, NULL, "__asm");

  compile(stdin, "*stdin*");

  fixup_locations();

  return 0;
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

void error(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fprintf(stderr, "\n");
  exit(1);
}

// Container

Vector *new_vector(void) {
  Vector *vec = malloc(sizeof(Vector));
  vec->data = malloc(sizeof(void *) * 16);
  vec->capacity = 16;
  vec->len = 0;
  return vec;
}

void vec_push(Vector *vec, const void *elem) {
  if (vec->capacity == vec->len) {
    vec->capacity *= 2;
    vec->data = realloc(vec->data, sizeof(void *) * vec->capacity);
  }
  vec->data[vec->len++] = (void*)elem;
}

//

Map *new_map(void) {
  Map *map = malloc(sizeof(Map));
  map->keys = new_vector();
  map->vals = new_vector();
  return map;
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
#include "elfutil.h"

#include <stdio.h>
#include <stdlib.h>  // calloc

#if defined(__XV6)
// XV6
#include "../kernel/types.h"
#include "../kernel/elf.h"

#elif defined(__linux__)
// Linux
#include <elf.h>

#else

#error Target not supported

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
