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
  enum TokenKind kind;
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
  { "__asm", TK_ASM },
};

static const struct {
  const char ident[4];
  enum TokenKind kind;
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
};

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

static Token *alloc_token(enum TokenKind kind, const char *begin, const char *end) {
  Token *token = malloc(sizeof(*token));
  token->kind = kind;
  token->line = lexer.line;
  token->begin = begin;
  token->end = end;
  return token;
}

Token *alloc_ident(const char *ident, const char *begin, const char *end) {
  Token *tok = alloc_token(TK_IDENT, begin, end);
  tok->ident = ident;
  return tok;
}

static enum TokenKind reserved_word(const char *word) {
  for (int i = 0, n = (int)(sizeof(kReservedWords) / sizeof(*kReservedWords)); i < n; ++i) {
    if (strcmp(kReservedWords[i].str, word) == 0)
      return kReservedWords[i].kind;
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
          lex_error(p, "Block comment not closed");
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
    if (p[1] == 'x') {
      base = 16;
      p += 2;
    } else {
      base = 8;
    }
  }
  const char *q = p;
  long val = strtol(p, (char**)&p, base);
  if (p == q)
    lex_error(p, "Illegal literal");

  enum TokenKind tt = TK_INTLIT;
  if (*p == 'L') {
    tt = TK_LONGLIT;
    ++p;
  }
  Token *tok = alloc_token(tt, start, p);
  tok->value = val;
  *pp = p;
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
  tok->value = c;
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
  if (isalnum(*p))
    return NULL;

  Token *tok = NULL;
  for (int i = 0, n = sizeof(kMultiOperators) / sizeof(*kMultiOperators); i < n; ++i) {
    const char *ident = kMultiOperators[i].ident;
    size_t len = strlen(ident);
    if (strncmp(p, ident, len) == 0) {
      const char *q = p + len;
      tok = alloc_token(kMultiOperators[i].kind, p, q);
      p = q;
      break;
    }
  }

  if (tok == NULL) {
    char c = *p;
    if (c >= 0 /*&& c < 128*/) {
      enum TokenKind single = kSingleOperatorTypeMap[(int)c];
      if (single != 0) {
        tok = alloc_token(single, p, p + 1);
        ++p;
      }
    }
  }

  if (tok != NULL)
    *pp = p;

  return tok;
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
  char *ident = read_ident(&p);
  if (ident != NULL) {
    enum TokenKind word = reserved_word(ident);
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

Token *consume(enum TokenKind kind) {
  Token *tok = fetch_token();
  if (tok->kind != kind && (int)kind != -1)
    return NULL;
  if (tok->kind != TK_EOF)
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

const Type tyChar =  {.kind=TY_NUM, .num={.kind=NUM_CHAR}};
const Type tyShort = {.kind=TY_NUM, .num={.kind=NUM_SHORT}};
const Type tyInt =   {.kind=TY_NUM, .num={.kind=NUM_INT}};
const Type tyLong =  {.kind=TY_NUM, .num={.kind=NUM_LONG}};
const Type tyEnum =  {.kind=TY_NUM, .num={.kind=NUM_ENUM}};
const Type tyVoid =  {.kind=TY_VOID};
const Type tyVoidPtr =  {.kind=TY_PTR, .pa={.ptrof=&tyVoid}};

bool is_number(enum TypeKind kind) {
  return kind == TY_NUM;
}

bool is_char_type(const Type *type) {
  return type->kind == TY_NUM && type->num.kind == NUM_CHAR;
}

bool is_void_ptr(const Type *type) {
  return type->kind == TY_PTR && type->pa.ptrof->kind == TY_VOID;
}

bool same_type(const Type *type1, const Type *type2) {
  for (;;) {
    if (type1->kind != type2->kind)
      return false;

    switch (type1->kind) {
    case TY_VOID:
      return true;
    case TY_NUM:
      return type1->num.kind == type2->num.kind;
    case TY_ARRAY:
    case TY_PTR:
      type1 = type1->pa.ptrof;
      type2 = type2->pa.ptrof;
      continue;
    case TY_FUNC:
      if (!same_type(type1->func.ret, type2->func.ret) ||
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
          const Type *tmp = type1;
          type1 = type2;
          type2 = tmp;
        } else if (type2->struct_.info == NULL) {
          return strcmp(type1->struct_.name, type2->struct_.name) == 0;
        }
        // Find type1 from name.
        StructInfo *sinfo = find_struct(type1->struct_.name);
        if (sinfo == NULL)
          return false;
        return sinfo == type2->struct_.info;
      }
    }
  }
}

Type* ptrof(const Type *type) {
  Type *ptr = malloc(sizeof(*ptr));
  ptr->kind = TY_PTR;
  ptr->pa.ptrof = type;
  return ptr;
}

const Type *array_to_ptr(const Type *type) {
  if (type->kind != TY_ARRAY)
    return type;
  return ptrof(type->pa.ptrof);
}

Type* arrayof(const Type *type, size_t length) {
  Type *arr = malloc(sizeof(*arr));
  arr->kind = TY_ARRAY;
  arr->pa.ptrof = type;
  arr->pa.length = length;
  return arr;
}

Type* new_func_type(const Type *ret, Vector *param_types, bool vaargs) {
  Type *f = malloc(sizeof(*f));
  f->kind = TY_FUNC;
  f->func.ret = ret;
  f->func.vaargs = vaargs;
  f->func.param_types = param_types;
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
  type->kind = TY_NUM;
  type->num.kind = NUM_ENUM;
  type->num.enum_.ident = ident;
  type->num.enum_.members = new_vector();

  if (ident != NULL) {
    map_put(enum_map, ident->ident, type);
  }

  return type;
}

void add_enum_member(Type *type, const Token *ident, int value) {
  assert(type->kind == TY_NUM && type->num.kind == NUM_ENUM);
  EnumMember *member = malloc(sizeof(*member));
  member->ident = ident;
  member->value = value;
  vec_push(type->num.enum_.members, member);

  map_put(enum_value_map, ident->ident, (void*)(intptr_t)value);
}

bool find_enum_value(const char *name, intptr_t *output) {
  return map_try_get(enum_value_map, name, (void**)output);
}

#if 0
void dump_type(FILE *fp, const Type *type) {
  switch (type->kind) {
  case TY_VOID: fprintf(fp, "void"); break;
  case TY_NUM:
    switch (type->num.kind) {
    case NUM_CHAR:  fprintf(fp, "char"); break;
    case NUM_SHORT: fprintf(fp, "short"); break;
    case NUM_INT:   fprintf(fp, "int"); break;
    case NUM_LONG:  fprintf(fp, "long"); break;
    case NUM_ENUM:  fprintf(fp, "enum"); break;
    default: assert(false); break;
    }
    break;
  case TY_PTR: dump_type(fp, type->pa.ptrof); fprintf(fp, "*"); break;
  case TY_ARRAY: dump_type(fp, type->pa.ptrof); fprintf(fp, "[%d]", (int)type->pa.length); break;
  default: assert(false); break;
  }
}
#endif

// Typedef

Map *typedef_map;  // <char*, Type*>

const Type *find_typedef(const char *ident) {
  return map_get(typedef_map, ident);
}

bool add_typedef(const char *ident, const Type *type) {
  if (map_get(typedef_map, ident) != NULL)
    return false;
  map_put(typedef_map, ident, type);
  return true;
}
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
    name = ident->ident;
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
  info->local.label = label;
  info->reg = NULL;
  vec_push(lvars, info);
  return ginfo != NULL ? ginfo : info;
}

Vector *extract_varinfo_types(Vector *params) {
  Vector *param_types = NULL;
  if (params != NULL) {
    param_types = new_vector();
    for (int i = 0, len = params->len; i < len; ++i)
      vec_push(param_types, ((VarInfo*)params->data[i])->type);
  }
  return param_types;
}

// Global

Map *gvar_map;

VarInfo *find_global(const char *name) {
  return (VarInfo*)map_get(gvar_map, name);
}

VarInfo *define_global(const Type *type, int flag, const Token *ident, const char *name) {
  if (name == NULL)
    name = ident->ident;
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
  varinfo->global.init = NULL;
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

VarInfo *scope_find(Scope **pscope, const char *name) {
  Scope *scope = *pscope;
  VarInfo *varinfo = NULL;
  for (;; scope = scope->parent) {
    if (scope == NULL)
      break;
    if (scope->vars != NULL) {
      int idx = var_find(scope->vars, name);
      if (idx >= 0) {
        varinfo = (VarInfo*)scope->vars->data[idx];
        break;
      }
    }
  }
  *pscope = scope;
  return varinfo;
}

// Function

Function *new_func(const Type *type, const char *name, Vector *params) {
  Function *func = malloc(sizeof(*func));
  func->type = type;
  func->name = name;
  func->params = params;

  func->top_scope = NULL;
  func->all_scopes = new_vector();
  func->frame_size = 0;
  func->used_reg_bits = 0;
  func->bbcon = NULL;
  func->ret_bb = NULL;
  return func;
}
#include "expr.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "lexer.h"
#include "sema.h"
#include "type.h"
#include "util.h"
#include "var.h"

static StructInfo *parse_struct(bool is_union);
static Expr *cast_expr(void);
static Expr *unary(void);

bool is_const(Expr *expr) {
  // TODO: Handle constant variable.

  switch (expr->kind) {
  case EX_NUM:
  case EX_STR:
    return true;
  default:
    return false;
  }
}

void not_void(const Type *type) {
  if (type->kind == TY_VOID)
    parse_error(NULL, "`void' not allowed");
}

//

static Expr *new_expr(enum ExprKind kind, const Type *type, const Token *token) {
  Expr *expr = malloc(sizeof(*expr));
  expr->kind = kind;
  expr->type = type;
  expr->token = token;
  return expr;
}

Expr *new_expr_numlit(const Type *type, const Token *token, const Num *num) {
  assert(type->kind == TY_NUM);
  Expr *expr = new_expr(EX_NUM, type, token);
  expr->num = *num;
  return expr;
}

static Expr *new_expr_str(const Token *token, const char *str, size_t size) {
  Type *type = malloc(sizeof(*type));
  type->kind = TY_ARRAY;
  type->pa.ptrof = &tyChar;
  type->pa.length = size;

  Expr *expr = new_expr(EX_STR, type, token);
  expr->str.buf = str;
  expr->str.size = size;
  return expr;
}

Expr *new_expr_varref(const char *name, const Type *type, const Token *token) {
  Expr *expr = new_expr(EX_VARREF, type, token);
  expr->varref.ident = name;
  expr->varref.scope = NULL;
  return expr;
}

Expr *new_expr_bop(enum ExprKind kind, const Type *type, const Token *token, Expr *lhs, Expr *rhs) {
  Expr *expr = new_expr(kind, type, token);
  expr->bop.lhs = lhs;
  expr->bop.rhs = rhs;
  return expr;
}

static Expr *new_expr_unary(enum ExprKind kind, const Type *type, const Token *token, Expr *sub) {
  Expr *expr = new_expr(kind, type, token);
  expr->unary.sub = sub;
  return expr;
}

Expr *new_expr_deref(const Token *token, Expr *sub) {
  if (sub->type->kind != TY_PTR && sub->type->kind != TY_ARRAY)
    parse_error(token, "Cannot dereference raw type");
  return new_expr_unary(EX_DEREF, sub->type->pa.ptrof, token, sub);
}

static Expr *new_expr_ternary(const Token *token, Expr *cond, Expr *tval, Expr *fval, const Type *type) {
  Expr *expr = new_expr(EX_TERNARY, type, token);
  expr->ternary.cond = cond;
  expr->ternary.tval = tval;
  expr->ternary.fval = fval;
  return expr;
}

Expr *new_expr_member(const Token *token, const Type *type, Expr *target, const Token *acctok, const Token *ident, int index) {
  Expr *expr = new_expr(EX_MEMBER, type, token);
  expr->member.target = target;
  expr->member.acctok = acctok;
  expr->member.ident = ident;
  expr->member.index = index;
  return expr;
}

static Expr *new_expr_funcall(const Token *token, Expr *func, Vector *args) {
  Expr *expr = new_expr(EX_FUNCALL, NULL, token);
  expr->funcall.func = func;
  expr->funcall.args = args;
  return expr;
}

static Expr *new_expr_comma(Vector *list) {
  Expr *expr = new_expr(EX_COMMA, NULL, NULL);
  expr->comma.list = list;
  return expr;
}

Expr *new_expr_sizeof(const Token *token, const Type *type, Expr *sub) {
  Expr *expr = new_expr(EX_SIZEOF, &tySize, token);
  expr->sizeof_.type = type;
  expr->sizeof_.sub = sub;
  return expr;
}

Expr *new_expr_cast(const Type *type, const Token *token, Expr *sub) {
  Expr *expr = new_expr(EX_CAST, type, token);
  expr->unary.sub = sub;
  return expr;
}

Vector *parse_args(Token **ptoken) {
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

  *ptoken = token;
  return args;
}

static Expr *funcall(Expr *func) {
  Token *token;
  Vector *args = parse_args(&token);
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
  Type *type = typeIdent != NULL ? find_enum(typeIdent->ident) : NULL;
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
          if (!(is_const(expr) && is_number(expr->type->kind))) {
            parse_error(numtok, "const expected for enum");
          }
          value = expr->num.ival;
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
      bool is_union = structtok->kind == TK_UNION;
      const char *name = NULL;
      Token *ident;
      if ((ident = consume(TK_IDENT)) != NULL)
        name = ident->ident;

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
      stype->kind = TY_STRUCT;
      stype->struct_.name = name;
      stype->struct_.info = sinfo;
      type = stype;
    } else if (consume(TK_ENUM)) {
      type = parse_enum();
    } else if ((ident = consume(TK_IDENT)) != NULL) {
      type = find_typedef(ident->ident);
      if (type == NULL)
        unget_token(ident);
    } else {
      static const enum TokenKind kKeywords[] = {
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
    if (!(is_const(expr) && is_number(expr->type->kind)))
      parse_error(NULL, "syntax error");
    if (expr->num.ival <= 0)
      parse_error(tok, "Array size must be greater than 0, but %d", (int)expr->num.ival);
    length = expr->num.ival;
    if (!consume(TK_RBRACKET))
      parse_error(NULL, "`]' expected");
  }
  return arrayof(parse_type_suffix(type), length);
}

Vector *parse_funparam_types(bool *pvaargs) {  // Vector<Type*>
  Vector *params = parse_funparams(pvaargs);
  return extract_varinfo_types(params);
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
    ident = consume(TK_IDENT);
    //if (ident == NULL && !allow_noname)
    //  parse_error(NULL, "Ident expected");
  }
  if (type->kind != TY_VOID)
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
        if (type->kind == TY_VOID) {  // fun(void)
          if (ident != NULL || !consume(TK_RPAR))
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
      Num num = {tok->value};
      return new_expr_numlit(type, tok, &num);
    }
  }
  if ((tok = consume(TK_STR)))
    return new_expr_str(tok, tok->str.buf, tok->str.size);

  Token *ident;
  if ((ident = consume(TK_IDENT)) != NULL) {
    const char *name = ident->ident;
    return new_expr_varref(name, NULL, ident);
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
    else if ((tok = consume(TK_DOT)) != NULL || (tok = consume(TK_ARROW)) != NULL)
      expr = member_access(expr, tok);
    else if ((tok = consume(TK_INC)) != NULL)
      expr = new_expr_unary(EX_POSTINC, NULL, tok, expr);
    else if ((tok = consume(TK_DEC)) != NULL)
      expr = new_expr_unary(EX_POSTDEC, NULL, tok, expr);
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
    switch (expr->kind) {
    case EX_NUM:
      return expr;
    default:
      return new_expr_unary(EX_POS, NULL, tok, expr);
    }

    return expr;
  }

  if ((tok = consume(TK_SUB)) != NULL) {
    Expr *expr = cast_expr();
    switch (expr->kind) {
    case EX_NUM:
      expr->num.ival = -expr->num.ival;
      return expr;
    default:
      return new_expr_unary(EX_NEG, NULL, tok, expr);
    }
  }

  if ((tok = consume(TK_NOT)) != NULL) {
    Expr *expr = cast_expr();
    return new_expr_unary(EX_NOT, &tyBool, tok, expr);
  }

  if ((tok = consume(TK_TILDA)) != NULL) {
    Expr *expr = cast_expr();
    return new_expr_unary(EX_BITNOT, NULL, tok, expr);
  }

  if ((tok = consume(TK_AND)) != NULL) {
    Expr *expr = cast_expr();
    return new_expr_unary(EX_REF, NULL, tok, expr);
  }

  if ((tok = consume(TK_MUL)) != NULL) {
    Expr *expr = cast_expr();
    return new_expr_unary(EX_DEREF, NULL, tok, expr);
  }

  if ((tok = consume(TK_INC)) != NULL) {
    Expr *expr = unary();
    return new_expr_unary(EX_PREINC, NULL, tok, expr);
  }

  if ((tok = consume(TK_DEC)) != NULL) {
    Expr *expr = unary();
    return new_expr_unary(EX_PREDEC, NULL, tok, expr);
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
      expr->unary.sub = sub;
      return expr;
    }
    unget_token(lpar);
  }
  return unary();
}

static Expr *mul(void) {
  Expr *expr = cast_expr();

  for (;;) {
    enum ExprKind kind;
    Token *tok;
    if ((tok = consume(TK_MUL)) != NULL)
      kind = EX_MUL;
    else if ((tok = consume(TK_DIV)) != NULL)
      kind = EX_DIV;
    else if ((tok = consume(TK_MOD)) != NULL)
      kind = EX_MOD;
    else
      return expr;

    expr = new_expr_bop(kind, NULL, tok, expr, cast_expr());
  }
}

static Expr *add(void) {
  Expr *expr = mul();

  for (;;) {
    enum ExprKind t;
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
    enum ExprKind t;
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
    enum ExprKind t;
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
    enum ExprKind t;
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
      expr = new_expr_bop(EX_BITAND, NULL, tok, lhs, rhs);
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
      expr = new_expr_bop(EX_BITXOR, NULL, tok, lhs, rhs);
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
      expr = new_expr_bop(EX_BITOR, NULL, tok, lhs, rhs);
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
    expr = new_expr_ternary(tok, expr, t, f, NULL);
  }
}

Expr *parse_assign(void) {
  Expr *expr = conditional();

  Token *tok;
  if ((tok = consume(TK_ASSIGN)) != NULL)
    return new_expr_bop(EX_ASSIGN, NULL, tok, expr, parse_assign());
  enum ExprKind t;
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

  return new_expr_unary(EX_ASSIGN_WITH, NULL, tok,
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
#include "parser.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc

#include "expr.h"
#include "lexer.h"
#include "type.h"
#include "util.h"
#include "var.h"

static Node *stmt(void);

static VarDecl *new_vardecl(const Type *type, const Token *ident, Initializer *init, int flag) {
  VarDecl *decl = malloc(sizeof(*decl));
  decl->type = type;
  decl->ident = ident;
  decl->init = init;
  decl->flag = flag;
  return decl;
}

static Defun *new_defun(Function *func, int flag) {
  Defun *defun = malloc(sizeof(*defun));
  defun->func = func;
  defun->flag = flag;

  defun->stmts = NULL;
  defun->label_map = NULL;
  defun->gotos = NULL;
  return defun;
}

static Node *new_node(enum NodeKind kind) {
  Node *node = malloc(sizeof(Node));
  node->kind = kind;
  return node;
}

Node *new_node_expr(Expr *e) {
  Node *node = new_node(ND_EXPR);
  node->expr = e;
  return node;
}

static Node *new_node_block(Vector *nodes) {
  Node *node = new_node(ND_BLOCK);
  node->block.scope = NULL;
  node->block.nodes = nodes;
  return node;
}

static Node *new_node_if(Expr *cond, Node *tblock, Node *fblock) {
  Node *node = new_node(ND_IF);
  node->if_.cond = cond;
  node->if_.tblock = tblock;
  node->if_.fblock = fblock;
  return node;
}

static Node *new_node_switch(Expr *value) {
  Node *node = new_node(ND_SWITCH);
  node->switch_.value = value;
  node->switch_.body = NULL;
  node->switch_.case_values = new_vector();
  node->switch_.has_default = false;
  return node;
}

static Node *new_node_case(Expr *value) {
  Node *node = new_node(ND_CASE);
  node->case_.value = value;
  return node;
}

static Node *new_node_default(void) {
  Node *node = new_node(ND_DEFAULT);
  return node;
}

static Node *new_node_while(Expr *cond, Node *body) {
  Node *node = new_node(ND_WHILE);
  node->while_.cond = cond;
  node->while_.body = body;
  return node;
}

static Node *new_node_do_while(Node *body, Expr *cond) {
  Node *node = new_node(ND_DO_WHILE);
  node->while_.body = body;
  node->while_.cond = cond;
  return node;
}

static Node *new_node_for(Expr *pre, Expr *cond, Expr *post, Node *body) {
  Node *node = new_node(ND_FOR);
  node->for_.pre = pre;
  node->for_.cond = cond;
  node->for_.post = post;
  node->for_.body = body;
  return node;
}

static Node *new_node_return(Expr *val) {
  Node *node = new_node(ND_RETURN);
  node->return_.val = val;
  return node;
}

static Node *new_node_goto(const Token *label) {
  Node *node = new_node(ND_GOTO);
  node->goto_.tok = label;
  node->goto_.ident = label->ident;
  return node;
}

static Node *new_node_label(const char *name, Node *stmt) {
  Node *node = new_node(ND_LABEL);
  node->label.name = name;
  node->label.stmt = stmt;
  return node;
}

static Node *new_node_vardecl(Vector *decls) {
  Node *node = new_node(ND_VARDECL);
  node->vardecl.decls = decls;
  node->vardecl.inits = NULL;
  return node;
}

static Node *new_node_asm(Expr *str) {
  Node *node = new_node(ND_ASM);
  node->asm_.str = str;
  return node;
}

static Node *new_node_defun(Defun *defun) {
  Node *node = new_node(ND_DEFUN);
  node->defun = defun;
  return node;
}

Node *new_top_node(Vector *nodes) {
  Node *top = new_node(ND_TOPLEVEL);
  top->toplevel.nodes = nodes;
  return top;
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
          init->kind = vDot;
          init->dot.name = ident->ident;
          init->dot.value = value;
        } else if (consume(TK_LBRACKET)) {
          Expr *index = parse_const();
          if (!consume(TK_RBRACKET))
            parse_error(NULL, "`]' expected");
          consume(TK_ASSIGN);  // both accepted: `[1] = 2`, and `[1] 2`
          Initializer *value = parse_initializer();
          init = malloc(sizeof(*init));
          init->kind = vArr;
          init->arr.index = index;
          init->arr.value = value;
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
    result->kind = vMulti;
    result->multi = multi;
  } else {
    result->kind = vSingle;
    result->single = parse_assign();
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

    Initializer *init = NULL;
    if (consume(TK_LPAR)) {  // Function prototype.
      bool vaargs;
      Vector *param_types = parse_funparam_types(&vaargs);
      type = ptrof(new_func_type(type, param_types, vaargs));
      flag |= VF_EXTERN;
    } else {
      not_void(type);
      if (consume(TK_ASSIGN)) {
        init = parse_initializer();
      }
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
      swtch->switch_.body = stmt();
      return swtch;
    }
  }
  parse_error(NULL, "Illegal syntax in `switch'");
  return NULL;
}

static Node *parse_case(void) {
  // Token *tok = fetch_token();
  Expr *valnode = parse_const();

  if (!consume(TK_COLON))
    parse_error(NULL, "`:' expected");

  return new_node_case(valnode);
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
        if (type->kind == TY_VOID && ident != NULL) {

        }

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

static Node *parse_break_continue(enum NodeKind kind) {
  if (!consume(TK_SEMICOL))
    parse_error(NULL, "`;' expected");
  return new_node(kind);
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

static Node *parse_asm(void) {
  if (!consume(TK_LPAR))
    parse_error(NULL, "`(' expected");

  Token *token;
  Vector *args = parse_args(&token);

  if (args == NULL || args->len != 1 || ((Expr*)args->data[0])->kind != EX_STR)
    parse_error(token, "`__asm' expected one string");

  return new_node_asm(args->data[0]);
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
      return new_node_label(label->ident, stmt());
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

  if (consume(TK_ASM))
    return parse_asm();

  // expression statement.
  Expr *val = parse_expr();
  if (!consume(TK_SEMICOL))
    parse_error(NULL, "Semicolon required");
  return new_node_expr(val);
}

static Node *parse_defun(const Type *rettype, int flag, Token *ident) {
  const char *name = ident->ident;
  bool vaargs;
  Vector *params = parse_funparams(&vaargs);

  // Definition.
  Vector *param_types = extract_varinfo_types(params);
  Function *func = new_func(new_func_type(rettype, param_types, vaargs), name, params);
  Defun *defun = new_defun(func, flag);
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
  const char *name = ident->ident;
  add_typedef(name, type);

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

    if (type->kind == TY_VOID)
      parse_error(ident, "`void' not allowed1");

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
    if ((type->kind == TY_STRUCT ||
         (type->kind == TY_NUM && type->num.kind == NUM_ENUM)) &&
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

Vector *parse_program(Vector *nodes) {
  if (nodes == NULL)
    nodes = new_vector();
  while (!consume(TK_EOF)) {
    Node *node = toplevel();
    if (node != NULL)
      vec_push(nodes, node);
  }
  return nodes;
}
#include "sema.h"

#include <assert.h>
#include <inttypes.h>  // PRIdPTR
#include <stdlib.h>  // malloc

#include "expr.h"
#include "lexer.h"
#include "parser.h"
#include "type.h"
#include "util.h"
#include "var.h"

const int LF_BREAK = 1 << 0;
const int LF_CONTINUE = 1 << 0;

Defun *curdefun;
static int curloopflag;
static Node *curswitch;

// Scope

static Scope *enter_scope(Defun *defun, Vector *vars) {
  Scope *scope = new_scope(curscope, vars);
  curscope = scope;
  vec_push(defun->func->all_scopes, scope);
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
  assert(type->kind == TY_ARRAY);

  bool is_str = (is_char_type(type->pa.ptrof) &&
                 init->kind == vSingle &&
                 init->single->kind == EX_STR);
  if (!is_str && init->kind != vMulti) {
    parse_error(NULL, "Error initializer");
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
        if (init_elem->kind == vArr) {
          assert(init_elem->arr.index->kind == EX_NUM);
          index = init_elem->arr.index->num.ival;
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

static void add_func_label(const char *label) {
  assert(curdefun != NULL);
  if (curdefun->label_map == NULL)
    curdefun->label_map = new_map();
  map_put(curdefun->label_map, label, NULL);  // Put dummy value.
}

static void add_func_goto(Node *node) {
  assert(curdefun != NULL);
  if (curdefun->gotos == NULL)
    curdefun->gotos = new_vector();
  vec_push(curdefun->gotos, node);
}

static Initializer *analyze_initializer(Initializer *init) {
  if (init == NULL)
    return NULL;

  switch (init->kind) {
  case vSingle:
    init->single = analyze_expr(init->single, false);
    break;
  case vMulti:
    for (int i = 0; i < init->multi->len; ++i)
      init->multi->data[i] = analyze_initializer(init->multi->data[i]);
    break;
  case vDot:
    init->dot.value = analyze_initializer(init->dot.value);
    break;
  case vArr:
    init->arr.value = analyze_initializer(init->arr.value);
    break;
  }
  return init;
}

VarInfo *str_to_char_array(const Type *type, Initializer *init) {
  assert(type->kind == TY_ARRAY && is_char_type(type->pa.ptrof));
  const Token *ident = alloc_ident(alloc_label(), NULL, NULL);
  VarInfo *varinfo = define_global(type, VF_CONST | VF_STATIC, ident, NULL);
  varinfo->global.init = init;
  return varinfo;
}

static void string_initializer(Expr *dst, Initializer *src, Vector *inits) {
  // Initialize char[] with string literal (char s[] = "foo";).
  assert(src->kind == vSingle);
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

  const Type* strtype = dst->type;
  VarInfo *varinfo = str_to_char_array(strtype, src);
  Expr *varref = new_expr_varref(varinfo->name, strtype, NULL);

  for (size_t i = 0; i < size; ++i) {
    Num n = {.ival=i};
    Expr *index = new_expr_numlit(&tyInt, NULL, &n);
    vec_push(inits,
             new_node_expr(new_expr_bop(EX_ASSIGN, &tyChar, NULL,
                                        new_expr_deref(NULL, add_expr(NULL, dst, index, true)),
                                        new_expr_deref(NULL, add_expr(NULL, varref, index, true)))));
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
  int i = 0, len = init->multi->len;
  for (; i < len; ++i) {
    Initializer *init_elem = init->multi->data[i];
    if (init_elem->kind == vDot)
      parse_error(NULL, "dot initializer for array");
    if (init_elem->kind == vArr)
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
    if (i >= len || (init_elem = init->multi->data[i])->kind == vArr) {
      if (i < len && init_elem->arr.index->kind != EX_NUM)
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
      lastStart = index = init_elem->arr.index->num.ival;
      lastStartIndex = i;
    } else if (init_elem->kind == vDot)
      parse_error(NULL, "dot initializer for array");
  }

  // Sort
  myqsort(ranges->data, ranges->len, sizeof(size_t*), compare_desig_start);

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
      if (j == 0 && index != start && elem->kind != vArr) {
        Initializer *arr = malloc(sizeof(*arr));
        arr->kind = vArr;
        Num n = {.ival = start};
        arr->arr.index = new_expr_numlit(&tyInt, NULL, &n);
        arr->arr.value = elem;
        elem = arr;
      }
      vec_push(reordered, elem);
    }
  }

  Initializer *init2 = malloc(sizeof(*init2));
  init2->kind = vMulti;
  init2->multi = reordered;
  return init2;
}

Initializer *flatten_initializer(const Type *type, Initializer *init) {
  if (init == NULL)
    return NULL;

  switch (type->kind) {
  case TY_STRUCT:
    if (init->kind == vMulti) {
      ensure_struct((Type*)type, NULL);
      const StructInfo *sinfo = type->struct_.info;
      int n = sinfo->members->len;
      int m = init->multi->len;
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
        Initializer *value = init->multi->data[i];
        if (value->kind == vArr)
          parse_error(NULL, "indexed initializer for array");

        if (value->kind == vDot) {
          const char *name = value->dot.name;
          index = var_find(sinfo->members, name);
          if (index >= 0) {
            value = value->dot.value;
          } else {
            Vector *stack = new_vector();
            bool res = search_from_anonymous(type, name, NULL, stack);
            if (!res)
              parse_error(NULL, "`%s' is not member of struct", name);

            index = (intptr_t)stack->data[0];
            Vector *multi = new_vector();
            vec_push(multi, value);
            Initializer *init2 = malloc(sizeof(*init2));
            init2->kind = vMulti;
            init2->multi = multi;
            value = init2;
          }
        }
        if (index >= n)
          parse_error(NULL, "Too many init values");

        // Allocate string literal for char* as a char array.
        if (value->kind == vSingle && value->single->kind == EX_STR) {
          const VarInfo *member = sinfo->members->data[index];
          if (member->type->kind == TY_PTR &&
              is_char_type(member->type->pa.ptrof)) {
            Expr *expr = value->single;
            Initializer *strinit = malloc(sizeof(*strinit));
            strinit->kind = vSingle;
            strinit->single = expr;

            // Create string and point to it.
            Type* strtype = arrayof(&tyChar, expr->str.size);
            const char * label = alloc_label();
            const Token *ident = alloc_ident(label, NULL, NULL);
            VarInfo *varinfo = define_global(strtype, VF_CONST | VF_STATIC, ident, NULL);
            varinfo->global.init = strinit;

            // Replace initializer from string literal to string array defined in global.
            value->single = new_expr_varref(label, strtype, ident);
          }
        }

        values[index++] = value;
      }

      Initializer *flat = malloc(sizeof(*flat));
      flat->kind = vMulti;
      Vector *v = malloc(sizeof(*v));
      v->len = v->capacity = n;
      v->data = (void**)values;
      flat->multi = v;

      return flat;
    }
    break;
  case TY_ARRAY:
    switch (init->kind) {
    case vMulti:
      init = flatten_array_initializer(init);
      break;
    case vSingle:
      // Special handling for string (char[]).
      if (can_cast(type, init->single->type, init->single, false))
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
  case TY_NUM:
    if (init->kind == vSingle) {
      switch (init->single->kind) {
      case EX_NUM:
        return init;
      default:
        parse_error(NULL, "Constant expression expected");
        break;
      }
    }
    break;
  case TY_PTR:
    {
      if (init->kind != vSingle)
        parse_error(NULL, "initializer type error");
      Expr *value = init->single;
      switch (value->kind) {
      case EX_REF:
        {
          value = value->unary.sub;
          if (value->kind != EX_VARREF)
            parse_error(NULL, "pointer initializer must be varref");
          if (value->varref.scope != NULL)
            parse_error(NULL, "Allowed global reference only");

          VarInfo *info = find_global(value->varref.ident);
          assert(info != NULL);

          if (!same_type(type->pa.ptrof, info->type))
            parse_error(NULL, "Illegal type");

          return init;
        }
      case EX_VARREF:
        {
          if (value->varref.scope != NULL)
            parse_error(NULL, "Allowed global reference only");

          VarInfo *info = find_global(value->varref.ident);
          assert(info != NULL);

          if (info->type->kind != TY_ARRAY || !same_type(type->pa.ptrof, info->type->pa.ptrof))
            parse_error(NULL, "Illegal type");

          return init;
        }
      case EX_CAST:
        // Handle NULL assignment.
        while (value->kind == EX_CAST)
          value = value->unary.sub;
        if (!is_number(value->type->kind))
          break;
        // Fallthrough
      case EX_NUM:
        {
          Initializer *init2 = malloc(sizeof(*init2));
          init2->kind = vSingle;
          init2->single = value;
          return init2;
        }
        break;
      case EX_STR:
        {
          if (!(is_char_type(type->pa.ptrof) && value->kind == EX_STR))
            parse_error(NULL, "Illegal type");

          // Create string and point to it.
          Type* strtype = arrayof(type->pa.ptrof, value->str.size);
          VarInfo *varinfo = str_to_char_array(strtype, init);

          Initializer *init2 = malloc(sizeof(*init2));
          init2->kind = vSingle;
          init2->single = new_expr_varref(varinfo->name, strtype, NULL);
          return init2;
        }
      default:
        break;
      }
      parse_error(NULL, "initializer type error: kind=%d", value->kind);
    }
    break;
  case TY_ARRAY:
    switch (init->kind) {
    case vMulti:
      {
        const Type *elemtype = type->pa.ptrof;
        Vector *multi = init->multi;
        for (int i = 0, len = multi->len; i < len; ++i) {
          Initializer *eleminit = multi->data[i];
          multi->data[i] = check_global_initializer(elemtype, eleminit);
        }
      }
      break;
    case vSingle:
      if (is_char_type(type->pa.ptrof) && init->single->kind == EX_STR) {
        assert(type->pa.length != (size_t)-1);
        if (type->pa.length < init->single->str.size) {
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

static Vector *assign_initial_value(Expr *expr, Initializer *init, Vector *inits) {
  if (init == NULL)
    return inits;

  if (inits == NULL)
    inits = new_vector();

  Initializer *org_init = init;
  init = flatten_initializer(expr->type, init);

  switch (expr->type->kind) {
  case TY_ARRAY:
    switch (init->kind) {
    case vMulti:
      {
        size_t arr_len = expr->type->pa.length;
        assert(arr_len != (size_t)-1);
        if ((size_t)init->multi->len > arr_len)
          parse_error(NULL, "Initializer more than array size");
        size_t len = init->multi->len;
        size_t index = 0;
        for (size_t i = 0; i < len; ++i, ++index) {
          Initializer *init_elem = init->multi->data[i];
          if (init_elem->kind == vArr) {
            Expr *ind = init_elem->arr.index;
            if (ind->kind != EX_NUM)
              parse_error(NULL, "Number required");
            index = ind->num.ival;
            init_elem = init_elem->arr.value;
          }

          Num n = {.ival=index};
          Expr *add = add_expr(NULL, expr, new_expr_numlit(&tyInt, NULL, &n), true);

          assign_initial_value(new_expr_deref(NULL, add), init_elem, inits);
        }
      }
      break;
    case vSingle:
      // Special handling for string (char[]).
      if (is_char_type(expr->type->pa.ptrof) &&
          init->single->kind == EX_STR) {
        string_initializer(expr, init, inits);
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
      if (init->kind != vMulti) {
        vec_push(inits,
                 new_node_expr(new_expr_bop(EX_ASSIGN, expr->type, NULL, expr,
                                            init->single)));
        break;
      }

      const StructInfo *sinfo = expr->type->struct_.info;
      if (!sinfo->is_union) {
        for (int i = 0, n = sinfo->members->len; i < n; ++i) {
          const VarInfo* member = sinfo->members->data[i];
          Expr *mem = new_expr_member(NULL, member->type, expr, NULL, NULL, i);
          Initializer *init_elem = init->multi->data[i];
          if (init_elem != NULL)
            assign_initial_value(mem, init_elem, inits);
        }
      } else {
        int n = sinfo->members->len;
        int m = init->multi->len;
        if (n <= 0 && m > 0)
          parse_error(NULL, "Initializer for empty union");
        if (org_init->multi->len > 1)
          parse_error(NULL, "More than one initializer for union");

        for (int i = 0; i < n; ++i) {
          Initializer *init_elem = init->multi->data[i];
          if (init_elem == NULL)
            continue;
          const VarInfo* member = sinfo->members->data[i];
          Expr *mem = new_expr_member(NULL, member->type, expr, NULL, NULL, i);
          assign_initial_value(mem, init_elem, inits);
          break;
        }
      }
    }
    break;
  default:
    if (init->kind != vSingle)
      parse_error(NULL, "Error initializer");
    vec_push(inits,
             new_node_expr(new_expr_bop(EX_ASSIGN, expr->type, NULL, expr,
                                        make_cast(expr->type, NULL, init->single, false))));
    break;
  }

  return inits;
}

static Node *sema_vardecl(Node *node) {
  assert(node->kind == ND_VARDECL);
  Vector *decls = node->vardecl.decls;
  Vector *inits = NULL;
  for (int i = 0, len = decls->len; i < len; ++i) {
    VarDecl *decl = decls->data[i];
    const Type *type = decl->type;
    const Token *ident = decl->ident;
    int flag = decl->flag;
    Initializer *init = decl->init;

    if (type->kind == TY_ARRAY && init != NULL)
      fix_array_size((Type*)type, init);

    if (curdefun != NULL) {
      VarInfo *varinfo = add_cur_scope(ident, type, flag);
      init = analyze_initializer(init);

      // TODO: Check `init` can be cast to `type`.
      if (flag & VF_STATIC) {
        varinfo->global.init = check_global_initializer(type, init);
        // static variable initializer is handled in codegen, same as global variable.
      } else if (init != NULL) {
        Expr *varref = new_expr_varref(ident->ident, type, NULL);
        varref->varref.scope = curscope;
        inits = assign_initial_value(varref, init, inits);
      }
    } else {
      intptr_t eval;
      if (find_enum_value(ident->ident, &eval))
        parse_error(NULL, "`%s' is already defined", ident->ident);
      if (flag & VF_EXTERN && init != NULL)
        parse_error(/*tok*/ NULL, "extern with initializer");
      // Toplevel
      VarInfo *varinfo = define_global(type, flag, ident, NULL);
      init = analyze_initializer(init);
      varinfo->global.init = check_global_initializer(type, init);
    }
  }

  node->vardecl.inits = inits;
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

  VarInfo *def = find_global(defun->func->name);
  if (def == NULL) {
    define_global(defun->func->type, defun->flag | VF_CONST, ident, defun->func->name);
  } else {
    if (def->type->kind != TY_FUNC)
      parse_error(ident, "Definition conflict: `%s'");
    // TODO: Check type.
    // TODO: Check duplicated definition.
    if (def->global.init != NULL)
      parse_error(ident, "`%s' function already defined");
  }

  if (defun->stmts != NULL) {  // Not prototype defintion.
    curdefun = defun;
    enter_scope(defun, defun->func->params);  // Scope for parameters.
    curscope = defun->func->top_scope = enter_scope(defun, NULL);
    sema_nodes(defun->stmts);
    exit_scope();
    exit_scope();
    curdefun = NULL;
    curscope = NULL;

    // Check goto labels.
    if (defun->gotos != NULL) {
      Vector *gotos = defun->gotos;
      Map *label_map = defun->label_map;
      for (int i = 0; i < gotos->len; ++i) {
        Node *node = gotos->data[i];
        void *bb;
        if (label_map == NULL || !map_try_get(label_map, node->goto_.ident, &bb))
          parse_error(node->goto_.tok, "`%s' not found", node->goto_.ident);
      }
    }
  }
}

Node *sema(Node *node) {
  if (node == NULL)
    return node;

  switch (node->kind) {
  case ND_EXPR:
    node->expr = analyze_expr(node->expr, false);
    break;

  case ND_DEFUN:
    sema_defun(node->defun);
    break;

  case ND_BLOCK:
    {
      Scope *parent_scope = curscope;
      if (curdefun != NULL)
        node->block.scope = curscope = enter_scope(curdefun, NULL);
      sema_nodes(node->block.nodes);
      curscope = parent_scope;
    }
    break;

  case ND_IF:
    node->if_.cond = analyze_expr(node->if_.cond, false);
    node->if_.tblock = sema(node->if_.tblock);
    node->if_.fblock = sema(node->if_.fblock);
    break;

  case ND_SWITCH:
    {
      Node *save_switch = curswitch;
      int save_flag = curloopflag;
      curloopflag |= LF_BREAK;
      curswitch = node;

      node->switch_.value = analyze_expr(node->switch_.value, false);
      node->switch_.body = sema(node->switch_.body);

      curloopflag = save_flag;
      curswitch = save_switch;
    }
    break;

  case ND_WHILE:
  case ND_DO_WHILE:
    {
      node->while_.cond = analyze_expr(node->while_.cond, false);

      int save_flag = curloopflag;
      curloopflag |= LF_BREAK | LF_CONTINUE;

      node->while_.body = sema(node->while_.body);

      curloopflag = save_flag;
    }
    break;

  case ND_FOR:
    {
      node->for_.pre = analyze_expr(node->for_.pre, false);
      node->for_.cond = analyze_expr(node->for_.cond, false);
      node->for_.post = analyze_expr(node->for_.post, false);

      int save_flag = curloopflag;
      curloopflag |= LF_BREAK | LF_CONTINUE;

      node->for_.body = sema(node->for_.body);

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
      assert(curdefun != NULL);
      const Type *rettype = curdefun->func->type->func.ret;
      Expr *val = node->return_.val;
      Token *tok = NULL;
      if (val == NULL) {
        if (rettype->kind != TY_VOID)
          parse_error(tok, "`return' required a value");
      } else {
        if (rettype->kind == TY_VOID)
          parse_error(tok, "void function `return' a value");

        const Token *tok = NULL;
        Expr *val = analyze_expr(node->return_.val, false);
        node->return_.val = make_cast(rettype, tok, val, false);
      }
    }
    break;

  case ND_CASE:
    {
      if (curswitch == NULL)
        parse_error(/*tok*/ NULL, "`case' cannot use outside of `switch`");

      node->case_.value = analyze_expr(node->case_.value, false);
      if (!is_number(node->case_.value->type->kind))
        parse_error(/*tok*/ NULL, "Cannot use expression");
      intptr_t value = node->case_.value->num.ival;

      // Check duplication.
      Vector *values = curswitch->switch_.case_values;
      for (int i = 0, len = values->len; i < len; ++i) {
        if ((intptr_t)values->data[i] == value)
          parse_error(/*tok*/ NULL, "Case value `%"PRIdPTR"' already defined", value);
      }
      vec_push(values, (void*)value);
    }
    break;

  case ND_DEFAULT:
    if (curswitch == NULL)
      parse_error(/*tok*/ NULL, "`default' cannot use outside of `switch'");
    if (curswitch->switch_.has_default)
      parse_error(/*tok*/ NULL, "`default' already defined in `switch'");

    curswitch->switch_.has_default = true;
    break;

  case ND_GOTO:
    add_func_goto(node);
    break;

  case ND_LABEL:
    add_func_label(node->label.name);
    node->label.stmt = sema(node->label.stmt);
    break;

  case ND_VARDECL:
    return sema_vardecl(node);

  case ND_ASM:
    return node;

  case ND_TOPLEVEL:
    sema_nodes(node->toplevel.nodes);
    break;

  default:
    fprintf(stderr, "sema: Unhandled node, kind=%d\n", node->kind);
    assert(false);
    break;
  }
  return node;
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
  assert(type->kind == TY_STRUCT);
  if (type->struct_.info == NULL) {
    StructInfo *sinfo = (StructInfo*)map_get(struct_map, type->struct_.name);
    if (sinfo == NULL)
      parse_error(token, "Accessing unknown struct(%s)'s member", type->struct_.name);
    type->struct_.info = sinfo;
  }
}

bool can_cast(const Type *dst, const Type *src, Expr *src_expr, bool is_explicit) {
  if (same_type(dst, src))
    return true;

  if (dst->kind == TY_VOID)
    return src->kind == TY_VOID || is_explicit;
  if (src->kind == TY_VOID)
    return false;

  switch (dst->kind) {
  case TY_NUM:
    switch (src->kind) {
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
    switch (src->kind) {
    case TY_NUM:
      if (src_expr->kind == EX_NUM && src_expr->num.ival == 0)  // Special handling for 0 to pointer.
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
      break;
    case TY_ARRAY:
      if (is_explicit)
        return true;
      if (same_type(dst->pa.ptrof, src->pa.ptrof) ||
          can_cast(dst, ptrof(src->pa.ptrof), src_expr, is_explicit))
        return true;
      break;
    case TY_FUNC:
      if (is_explicit)
        return true;
      if (dst->pa.ptrof->kind == TY_FUNC && same_type(dst->pa.ptrof, src))
        return true;
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

static bool check_cast(const Type *dst, const Type *src, Expr *src_expr, bool is_explicit) {
  if (can_cast(dst, src, src_expr, is_explicit))
    return true;
  parse_error(NULL, "Cannot convert value from type %d to %d", src->kind, dst->kind);
  return false;
}

Expr *make_cast(const Type *type, const Token *token, Expr *sub, bool is_explicit) {
  if (type->kind == TY_VOID || sub->type->kind == TY_VOID)
    parse_error(NULL, "cannot use `void' as a value");

  if (same_type(type, sub->type))
    return sub;
  //if (is_const(sub)) {
  //  // Casting number types needs its value range info,
  //  // so handlded in codegen.
  //  sub->type = type;
  //  return sub;
  //}

  check_cast(type, sub->type, sub, is_explicit);

  return new_expr_cast(type, token, sub);
}

// num +|- num
static Expr *add_num(enum ExprKind kind, const Token *tok, Expr *lhs, Expr *rhs, bool keep_left) {
  const Type *ltype = lhs->type;
  const Type *rtype = rhs->type;
  assert(ltype->kind == TY_NUM && rtype->kind == TY_NUM);
  enum NumKind lnt = ltype->num.kind;
  enum NumKind rnt = rtype->num.kind;
  if (lnt == NUM_ENUM)
    lnt = NUM_INT;
  if (rnt == NUM_ENUM)
    rnt = NUM_INT;

  if (is_const(lhs) && is_const(rhs)) {
    intptr_t lval = lhs->num.ival;
    intptr_t rval = rhs->num.ival;
    intptr_t value;
    switch (kind) {
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
    const Type *type = lnt >= rnt ? lhs->type : rhs->type;
    return new_expr_numlit(type, lhs->token, &num);
  }

  const Type *type;
  if (lnt >= rnt || keep_left) {
    type = tyNumTable[lnt];
    rhs = make_cast(type, rhs->token, rhs, false);
  } else {
    type = tyNumTable[rnt];
    lhs = make_cast(type, lhs->token, lhs, false);
  }
  return new_expr_bop(kind, type, tok, lhs, rhs);
}

// pointer +|- num
static Expr *add_ptr_num(enum ExprKind kind, const Token *token, Expr *ptr, Expr *num) {
  const Type *ptr_type = ptr->type;
  if (ptr_type->kind == TY_ARRAY)
    ptr_type = array_to_ptr(ptr_type);
  return new_expr_bop(kind, ptr_type, token, ptr,
                      new_expr_bop(EX_MUL, &tySize, token,
                                   make_cast(&tySize, token, num, false),
                                   new_expr_sizeof(token, ptr_type->pa.ptrof, NULL)));
}

Expr *add_expr(const Token *tok, Expr *lhs, Expr *rhs, bool keep_left) {
  const Type *ltype = lhs->type;
  const Type *rtype = rhs->type;

  if (is_number(ltype->kind)) {
    if (is_number(rtype->kind))
      return add_num(EX_ADD, tok, lhs, rhs, keep_left);
    if (same_type(ltype, rtype))
      return new_expr_bop(EX_ADD, ltype, tok, lhs, rhs);
  }

  switch (ltype->kind) {
  case TY_NUM:
    switch (rtype->kind) {
    case TY_PTR: case TY_ARRAY:
      if (!keep_left)
        return add_ptr_num(EX_ADD, tok, rhs, lhs);
      break;
    default:
      break;
    }
    break;

  case TY_PTR: case TY_ARRAY:
    switch (rtype->kind) {
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
  const Type *ltype = array_to_ptr(lhs->type);
  const Type *rtype = array_to_ptr(rhs->type);
  if (!same_type(ltype, rtype))
    parse_error(tok, "Different pointer diff");
  const Type *elem_type = ltype;
  if (elem_type->kind == TY_PTR)
    elem_type = elem_type->pa.ptrof;
  return new_expr_bop(EX_DIV, &tySize, tok,
                      new_expr_bop(EX_SUB, &tySize, tok, lhs, rhs),
                      new_expr_sizeof(tok, elem_type, NULL));
}

static Expr *sub_expr(const Token *tok, Expr *lhs, Expr *rhs, bool keep_left) {
  if (is_number(lhs->type->kind)) {
    if (is_number(rhs->type->kind))
      return add_num(EX_SUB, tok, lhs, rhs, keep_left);
    if (same_type(lhs->type, rhs->type))
      return new_expr_bop(EX_SUB, lhs->type, tok, lhs, rhs);
  }

  switch (lhs->type->kind) {
  case TY_PTR:
    switch (rhs->type->kind) {
    case TY_NUM:
      return add_ptr_num(EX_SUB, tok, lhs, rhs);
    case TY_PTR: case TY_ARRAY:
      return diff_ptr(tok, lhs, rhs);
    default:
      break;
    }
    break;

  case TY_ARRAY:
    switch (rhs->type->kind) {
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
  Expr *lhs = *pLhs;
  Expr *rhs = *pRhs;
  const Type *ltype = lhs->type;
  const Type *rtype = rhs->type;
  if (!is_number(ltype->kind) || !is_number(rtype->kind))
    return false;

  enum NumKind lkind = ltype->num.kind;
  enum NumKind rkind = rtype->num.kind;
  if (lkind == NUM_ENUM)
    lkind = NUM_INT;
  if (rkind == NUM_ENUM)
    rkind = NUM_INT;
  if (lkind != rkind) {
    if (lkind > rkind || keep_left)
      *pRhs = make_cast(ltype, rhs->token, rhs, false);
    else if (lkind < rkind)
      *pLhs = make_cast(rtype, lhs->token, lhs, false);
  }
  return true;
}

bool search_from_anonymous(const Type *type, const char *name, const Token *ident, Vector *stack) {
  assert(type->kind == TY_STRUCT);
  ensure_struct((Type*)type, ident);

  const Vector *members = type->struct_.info->members;
  for (int i = 0, len = members->len; i < len; ++i) {
    const VarInfo *member = members->data[i];
    if (member->name != NULL) {
      if (strcmp(member->name, name) == 0) {
        vec_push(stack, (void*)(long)i);
        return true;
      }
    } else if (member->type->kind == TY_STRUCT) {
      vec_push(stack, (void*)(intptr_t)i);
      bool res = search_from_anonymous(member->type, name, ident, stack);
      if (res)
        return true;
      vec_pop(stack);
    }
  }
  return false;
}

static enum ExprKind swap_cmp(enum ExprKind kind) {
  assert(EX_EQ <= kind && kind <= EX_GT);
  if (kind >= EX_LT)
    kind = EX_GT - (kind - EX_LT);
  return kind;
}

static Expr *analyze_cmp(Expr *expr) {
  Expr *lhs = expr->bop.lhs, *rhs = expr->bop.rhs;
  if (lhs->type->kind == TY_PTR || rhs->type->kind == TY_PTR) {
    if (lhs->type->kind != TY_PTR) {
      Expr *tmp = lhs;
      lhs = rhs;
      rhs = tmp;
      expr->bop.lhs = lhs;
      expr->bop.rhs = rhs;
      expr->kind = swap_cmp(expr->kind);
    }
    const Type *lt = lhs->type, *rt = rhs->type;
    if (!can_cast(lt, rt, rhs, false))
      parse_error(expr->token, "Cannot compare pointer to other types");
    if (rt->kind != TY_PTR)
      expr->bop.rhs = make_cast(lhs->type, expr->token, rhs, false);
  } else {
    if (!cast_numbers(&expr->bop.lhs, &expr->bop.rhs, false))
      parse_error(expr->token, "Cannot compare except numbers");
    // cast_numbers might change lhs and rhs, so need to be updated.
    lhs = expr->bop.lhs;
    rhs = expr->bop.rhs;

    if (is_const(lhs) && !is_const(rhs)) {
      Expr *tmp = lhs;
      expr->bop.lhs = rhs;
      expr->bop.rhs = tmp;
      expr->kind = swap_cmp(expr->kind);
      // No `lhs` nor `rhs` usage after here, so omit assignments.
    }
  }
  return expr;
}

// Traverse expr to check semantics and determine value type.
Expr *analyze_expr(Expr *expr, bool keep_left) {
  if (expr == NULL)
    return NULL;

  switch (expr->kind) {
  // Literals
  case EX_NUM:
  case EX_STR:
    assert(expr->type != NULL);
    break;

  case EX_VARREF:
    {
      const char *name = expr->varref.ident;
      const Type *type = NULL;
      Scope *scope = NULL;
      if (curscope != NULL) {
        scope = curscope;
        VarInfo *varinfo = scope_find(&scope, name);
        if (varinfo != NULL) {
          if (varinfo->flag & VF_STATIC) {
            // Replace local variable reference to global.
            name = varinfo->local.label;
            expr = new_expr_varref(name, varinfo->type, expr->token);
            scope = NULL;
          } else {
            type = varinfo->type;
          }
        }
      }
      if (type == NULL) {
        VarInfo *varinfo = find_global(name);
        if (varinfo != NULL) {
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
      expr->type = type;
      expr->varref.scope = scope;
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
    expr->bop.lhs = analyze_expr(expr->bop.lhs, false);
    expr->bop.rhs = analyze_expr(expr->bop.rhs, false);
    assert(expr->bop.lhs->type != NULL);
    assert(expr->bop.rhs->type != NULL);

    switch (expr->kind) {
    case EX_ADD:
      return add_expr(expr->token, expr->bop.lhs, expr->bop.rhs, keep_left);
    case EX_SUB:
      return sub_expr(expr->token, expr->bop.lhs, expr->bop.rhs, keep_left);
    case EX_MUL:
    case EX_DIV:
    case EX_MOD:
    case EX_BITAND:
    case EX_BITOR:
    case EX_BITXOR:
      if (!cast_numbers(&expr->bop.lhs, &expr->bop.rhs, keep_left))
        parse_error(expr->token, "Cannot use `%d' except numbers.", expr->kind);

      if (is_const(expr->bop.lhs) && is_const(expr->bop.rhs)) {
        Expr *lhs = expr->bop.lhs, *rhs = expr->bop.rhs;
        intptr_t lval = lhs->num.ival;
        intptr_t rval = rhs->num.ival;
        intptr_t value;
        switch (expr->kind) {
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
        const Type *type = lhs->type->num.kind >= rhs->type->num.kind ? lhs->type : rhs->type;
        return new_expr_numlit(type, lhs->token, &num);
      }

      expr->type = expr->bop.lhs->type;
      break;

    case EX_LSHIFT:
    case EX_RSHIFT:
      {
        enum TypeKind k;
        if (!is_number(k = expr->bop.lhs->type->kind) ||
            !is_number(k = expr->bop.rhs->type->kind))
          parse_error(expr->token, "Cannot use `%d' except numbers.", k);

        if (is_const(expr->bop.lhs) && is_const(expr->bop.rhs)) {
          intptr_t lval = expr->bop.lhs->num.ival;
          intptr_t rval = expr->bop.rhs->num.ival;
          intptr_t value = expr->kind == EX_LSHIFT ? lval << rval : lval >> rval;
          Num num = {value};
          return new_expr_numlit(expr->bop.lhs->type, expr->bop.lhs->token, &num);
        }

        expr->type = expr->bop.lhs->type;
      }
      break;

    case EX_EQ:
    case EX_NE:
    case EX_LT:
    case EX_GT:
    case EX_LE:
    case EX_GE:
      expr = analyze_cmp(expr);
      break;

    case EX_LOGAND:
    case EX_LOGIOR:
      break;

    case EX_ASSIGN:
      expr->type = expr->bop.lhs->type;
      expr->bop.rhs = make_cast(expr->type, expr->token, expr->bop.rhs, false);
      break;

    default:
      fprintf(stderr, "expr kind=%d\n", expr->kind);
      assert(!"analyze not handled!");
      break;
    }
    break;

  // Unary operators
  case EX_POS:
  case EX_NEG:
  case EX_NOT:
  case EX_BITNOT:
  case EX_PREINC:
  case EX_PREDEC:
  case EX_POSTINC:
  case EX_POSTDEC:
  case EX_REF:
  case EX_DEREF:
  case EX_CAST:
  case EX_ASSIGN_WITH:
    expr->unary.sub = analyze_expr(expr->unary.sub, expr->kind == EX_ASSIGN_WITH);
    assert(expr->unary.sub->type != NULL);

    switch (expr->kind) {
    case EX_POS:
      if (!is_number(expr->unary.sub->type->kind))
        parse_error(expr->token, "Cannot apply `+' except number types");
      return expr->unary.sub;

    case EX_NEG:
      if (!is_number(expr->unary.sub->type->kind))
        parse_error(expr->token, "Cannot apply `-' except number types");
      if (is_const(expr->unary.sub)) {
        Expr *sub = expr->unary.sub;
        sub->num.ival = -sub->num.ival;
        return sub;
      }
      expr->type = expr->unary.sub->type;
      break;

    case EX_NOT:
      switch (expr->unary.sub->type->kind) {
      case TY_NUM:
      case TY_PTR:
      case TY_ARRAY:
        break;
      default:
        parse_error(expr->token, "Cannot apply `!' except number or pointer types");
        break;
      }
      break;

    case EX_BITNOT:
      switch (expr->unary.sub->type->kind) {
      case TY_NUM:
        expr->type = expr->unary.sub->type;
        break;
      default:
        parse_error(expr->token, "Cannot apply `~' except number type");
        break;
      }
      break;

    case EX_PREINC:
    case EX_PREDEC:
    case EX_POSTINC:
    case EX_POSTDEC:
      expr->type = expr->unary.sub->type;
      break;

    case EX_REF:
      expr->type = ptrof(expr->unary.sub->type);
      break;

    case EX_DEREF:
      {
        Expr *sub = expr->unary.sub;
        if (sub->type->kind != TY_PTR && sub->type->kind != TY_ARRAY)
          parse_error(expr->token, "Cannot dereference raw type");
        expr->type = sub->type->pa.ptrof;
      }
      break;

    case EX_ASSIGN_WITH:
      expr->type = expr->unary.sub->bop.lhs->type;
      break;

    case EX_CAST:
      {
        Expr *sub = expr->unary.sub;
        if (same_type(expr->type, sub->type))
          return sub;
        check_cast(expr->type, sub->type, sub, true);
      }
      break;

    default:
      fprintf(stderr, "expr kind=%d\n", expr->kind);
      assert(!"analyze not handled!");
      break;
    }
    break;

  case EX_TERNARY:
    expr->ternary.cond = analyze_expr(expr->ternary.cond, false);
    expr->ternary.tval = analyze_expr(expr->ternary.tval, false);
    expr->ternary.fval = analyze_expr(expr->ternary.fval, false);
    {
      const Type *ttype = expr->ternary.tval->type;
      const Type *ftype = expr->ternary.fval->type;
      if (same_type(ttype, ftype)) {
        expr->type = ttype;
      } else if (is_void_ptr(ttype) && ftype->kind == TY_PTR) {
        expr->type = ftype;
      } else if (is_void_ptr(ftype) && ttype->kind == TY_PTR) {
        expr->type = ttype;
      } else {
        parse_error(NULL, "lhs and rhs must be same type");
      }
    }
    break;

  case EX_MEMBER:  // x.member or x->member
    {
      Expr *target = expr->member.target;
      expr->member.target = target = analyze_expr(target, false);
      assert(target->type != NULL);

      const Token *acctok = expr->member.acctok;
      const Token *ident = expr->member.ident;
      const char *name = ident->ident;

      // Find member's type from struct info.
      const Type *targetType = target->type;
      if (acctok->kind == TK_DOT) {
        if (targetType->kind != TY_STRUCT)
          parse_error(acctok, "`.' for non struct value");
      } else {  // TK_ARROW
        if (targetType->kind == TY_PTR)
          targetType = targetType->pa.ptrof;
        else if (targetType->kind == TY_ARRAY)
          targetType = targetType->pa.ptrof;
        else
          parse_error(acctok, "`->' for non pointer value");
        if (targetType->kind != TY_STRUCT)
          parse_error(acctok, "`->' for non struct value");
      }

      ensure_struct((Type*)targetType, ident);
      int index = var_find(targetType->struct_.info->members, name);
      if (index >= 0) {
        const VarInfo *member = targetType->struct_.info->members->data[index];
        expr->type = member->type;
        expr->member.index = index;
      } else {
        Vector *stack = new_vector();
        bool res = search_from_anonymous(targetType, ident->ident, ident, stack);
        if (!res)
          parse_error(ident, "`%s' doesn't exist in the struct", name);
        Expr *p = target;
        const Type *type = targetType;
        for (int i = 0; i < stack->len; ++i) {
          int index = (int)(long)stack->data[i];
          const VarInfo *member = type->struct_.info->members->data[index];
          type = member->type;
          p = new_expr_member(acctok, type, p, NULL, NULL, index);
        }
        expr = p;
      }
    }
    break;

  case EX_SIZEOF:
    {
      Expr *sub = expr->sizeof_.sub;
      if (sub != NULL) {
        sub = analyze_expr(sub, false);
        assert(sub->type != NULL);
        expr->sizeof_.type = sub->type;
      }
    }
    break;

  case EX_FUNCALL:
    {
      Expr *func = expr->funcall.func;
      Vector *args = expr->funcall.args;  // <Expr*>
      expr->funcall.func = func = analyze_expr(func, false);
      if (args != NULL) {
        for (int i = 0, len = args->len; i < len; ++i)
          args->data[i] = analyze_expr(args->data[i], false);
      }

      const Type *functype;
      if (!((functype = func->type)->kind == TY_FUNC ||
            (func->type->kind == TY_PTR && (functype = func->type->pa.ptrof)->kind == TY_FUNC)))
        parse_error(NULL, "Cannot call except funtion");
      expr->type = functype->func.ret;

      Vector *param_types = functype->func.param_types;  // <const Type*>
      bool vaargs = functype->func.vaargs;
      if (param_types != NULL) {
        int argc = args != NULL ? args->len : 0;
        int paramc = param_types->len;
        if (!(argc == paramc ||
              (vaargs && argc >= paramc)))
          parse_error(func->token, "function `%s' expect %d arguments, but %d", func->varref.ident, paramc, argc);
      }

      if (args != NULL && param_types != NULL) {
        int paramc = param_types->len;
        for (int i = 0, len = args->len; i < len; ++i) {
          if (i < param_types->len) {
            Expr *arg = args->data[i];
            const Type *type = (const Type*)param_types->data[i];
            args->data[i] = make_cast(type, arg->token, arg, false);
          } else if (vaargs && i >= paramc) {
            Expr *arg = args->data[i];
            const Type *type = arg->type;
            if (type->kind == TY_NUM && type->num.kind < NUM_INT)  // Promote variadic argument.
              args->data[i] = make_cast(&tyInt, arg->token, arg, false);
          }
        }
      }
    }
    break;

  case EX_COMMA:
    {
      Vector *list = expr->comma.list;
      int len = list->len;
      for (int i = 0; i < len; ++i)
        list->data[i] = analyze_expr(list->data[i], false);
      expr->type = ((Expr*)list->data[len - 1])->type;
    }
    break;

  default:
    fprintf(stderr, "expr kind=%d\n", expr->kind);
    assert(!"analyze not handled!");
    break;
  }

  assert(expr->type != NULL);
  return expr;
}
#include "codegen.h"

#include <assert.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#if defined(__linux) && !defined(__XV6)
#define USE_ALLOCA
#include <alloca.h>
#endif

#include "expr.h"
#include "ir.h"
#include "lexer.h"
#include "parser.h"
#include "regalloc.h"
#include "sema.h"
#include "type.h"
#include "util.h"
#include "var.h"
#include "x86_64.h"

const int FRAME_ALIGN = 8;
const int STACK_PARAM_BASE_OFFSET = (2 - MAX_REG_ARGS) * 8;

static void gen_expr_stmt(Expr *expr);

void set_curbb(BB *bb) {
  assert(curdefun != NULL);
  curbb = bb;
  vec_push(curdefun->func->bbcon->bbs, bb);
}

size_t type_size(const Type *type) {
  switch (type->kind) {
  case TY_VOID:
    return 1;  // ?
  case TY_NUM:
    switch (type->num.kind) {
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
    assert(type->pa.length != (size_t)-1);
    return type_size(type->pa.ptrof) * type->pa.length;
  case TY_STRUCT:
    ensure_struct((Type*)type, NULL);
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
    return 1;  // ?
  case TY_NUM:
    switch (type->num.kind) {
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
    return align_size(type->pa.ptrof);
  case TY_STRUCT:
    ensure_struct((Type*)type, NULL);
    calc_struct_size(type->struct_.info);
    return type->struct_.info->align;
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
    VarInfo *member = sinfo->members->data[i];
    size_t sz = type_size(member->type);
    int align = align_size(member->type);
    size = ALIGN(size, align);
    member->struct_.offset = size;
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

static void escape_string(const char *str, size_t size, StringBuffer *sb) {
  const char *s, *p;
  const char *end = str + size;
  for (s = p = str; p < end ; ++p) {
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

void construct_initial_value(unsigned char *buf, const Type *type, Initializer *init, Vector **pptrinits) {
  assert(init == NULL || init->kind != vDot);

  switch (type->kind) {
  case TY_NUM:
    {
      intptr_t v = 0;
      if (init != NULL) {
        assert(init->kind == vSingle);
        Expr *value = init->single;
        if (!(is_const(value) && is_number(value->type->kind)))
          error("Illegal initializer: constant number expected");
        v = value->num.ival;
      }

      int size = type_size(type);
      for (int i = 0; i < size; ++i)
        buf[i] = v >> (i * 8);  // Little endian

      switch (type->num.kind) {
      case NUM_CHAR:  _BYTE(NUM(v)); break;
      case NUM_SHORT: _WORD(NUM(v)); break;
      case NUM_LONG:  _QUAD(NUM(v)); break;
      default:
      case NUM_INT: case NUM_ENUM:
        _LONG(NUM(v));
        break;
      }
    }
    break;
  case TY_PTR:
    if (init != NULL) {
      assert(init->kind == vSingle);
      Expr *value = init->single;
      while (value->kind == EX_CAST)
        value = value->unary.sub;
      if (value->kind == EX_REF || value->kind == EX_VARREF) {
        if (value->kind == EX_REF)
          value = value->unary.sub;
        // TODO: Type check.

        assert(value->kind == EX_VARREF);
        assert(value->varref.scope == NULL);

        void **init = malloc(sizeof(void*) * 2);
        init[0] = buf;
        init[1] = (void*)value->varref.ident;
        if (*pptrinits == NULL)
          *pptrinits = new_vector();
        vec_push(*pptrinits, init);

        const char *label = value->varref.ident;
        const VarInfo *varinfo = find_global(label);
        assert(varinfo != NULL);
        if ((varinfo->flag & VF_STATIC) == 0)
          label = MANGLE(label);
        _QUAD(label);
      } else if (value->kind == EX_STR) {
        assert(!"`char* s = \"...\"`; should be handled in parser");
      } else if (is_const(value) && value->kind == EX_NUM) {
        intptr_t x = value->num.ival;
        for (int i = 0; i < WORD_SIZE; ++i)
          buf[i] = x >> (i * 8);  // Little endian

        _QUAD(NUM(x));
      } else {
        assert(!"initializer type error");
      }
    } else {
      _QUAD(NUM(0));
    }
    break;
  case TY_ARRAY:
    if (init == NULL || init->kind == vMulti) {
      const Type *elem_type = type->pa.ptrof;
      size_t elem_size = type_size(elem_type);
      if (init != NULL) {
        Vector *init_array = init->multi;
        size_t index = 0;
        size_t len = init_array->len;
        for (size_t i = 0; i < len; ++i, ++index) {
          Initializer *init_elem = init_array->data[i];
          if (init_elem->kind == vArr) {
            size_t next = init_elem->arr.index->num.ival;
            for (size_t j = index; j < next; ++j)
              construct_initial_value(buf + (j * elem_size), elem_type, NULL, pptrinits);
            index = next;
            init_elem = init_elem->arr.value;
          }
          construct_initial_value(buf + (index * elem_size), elem_type, init_elem, pptrinits);
        }
        // Padding
        for (size_t i = index, n = type->pa.length; i < n; ++i)
          construct_initial_value(buf + (i * elem_size), elem_type, NULL, pptrinits);
      }
    } else {
      if (init->kind == vSingle &&
          is_char_type(type->pa.ptrof) && init->single->kind == EX_STR) {
        int src_size = init->single->str.size;
        size_t size = type_size(type);
        assert(size >= (size_t)src_size);
        memcpy(buf, init->single->str.buf, src_size);

        UNUSED(size);
        StringBuffer sb;
        sb_init(&sb);
        sb_append(&sb, "\"", NULL);
        escape_string((char*)buf, size, &sb);
        sb_append(&sb, "\"", NULL);
        _ASCII(sb_to_string(&sb));
      } else {
        error("Illegal initializer");
      }
    }
    break;
  case TY_STRUCT:
    {
      assert(init == NULL || init->kind == vMulti);

      const StructInfo *sinfo = type->struct_.info;
      int count = 0;
      int offset = 0;
      for (int i = 0, n = sinfo->members->len; i < n; ++i) {
        const VarInfo* member = sinfo->members->data[i];
        Initializer *mem_init;
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
            emit_align(align);
            offset = ALIGN(offset, align);
          }
          construct_initial_value(buf + member->struct_.offset, member->type, mem_init, pptrinits);
          ++count;
          offset = ALIGN(offset, align);
          offset += type_size(member->type);
        }
      }
      if (sinfo->is_union && count <= 0) {
        const VarInfo* member = sinfo->members->data[0];
        construct_initial_value(buf + member->struct_.offset, member->type, NULL, pptrinits);
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

static void put_data(const char *label, const VarInfo *varinfo) {
  size_t size = type_size(varinfo->type);
  unsigned char *buf = calloc(size, 1);
  if (buf == NULL)
    error("Out of memory");

  emit_align(align_size(varinfo->type));
  if ((varinfo->flag & VF_STATIC) == 0) {  // global
    label = MANGLE(label);
    _GLOBL(label);
  }
  EMIT_LABEL(label);

  Vector *ptrinits = NULL;  // <[ptr, label]>
  construct_initial_value(buf, varinfo->type, varinfo->global.init, &ptrinits);

  free(buf);
}

// Put RoData into code.
static void put_rodata(void) {
  for (int i = 0, len = map_count(gvar_map); i < len; ++i) {
    const VarInfo *varinfo = (const VarInfo*)gvar_map->vals->data[i];
    if (varinfo->type->kind == TY_FUNC ||
        (varinfo->flag & VF_EXTERN) != 0 || varinfo->global.init == NULL ||
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
    if (varinfo->type->kind == TY_FUNC ||
        (varinfo->flag & VF_EXTERN) != 0 || varinfo->global.init == NULL ||
        (varinfo->flag & VF_CONST) != 0)
      continue;

    const char *name = (const char *)gvar_map->keys->data[i];
    put_data(name, varinfo);
  }
}

// Put global without initial value (bss).
static void put_bss(void) {
  for (int i = 0, len = map_count(gvar_map); i < len; ++i) {
    const char *label = (const char *)gvar_map->keys->data[i];
    const VarInfo *varinfo = (const VarInfo*)gvar_map->vals->data[i];
    if (varinfo->type->kind == TY_FUNC || varinfo->global.init != NULL ||
        (varinfo->flag & VF_EXTERN) != 0)
      continue;

    emit_align(align_size(varinfo->type));
    size_t size = type_size(varinfo->type);
    if (size < 1)
      size = 1;
    if ((varinfo->flag & VF_STATIC) == 0) {  // global
      label = MANGLE(label);
      _GLOBL(label);
    }
    _COMM(label, NUM(size));
  }
}

//

static BB *s_break_bb;
static BB *s_continue_bb;
int stackpos = 8;

static void pop_break_bb(BB *save) {
  s_break_bb = save;
}

static void pop_continue_bb(BB *save) {
  s_continue_bb = save;
}

static BB *push_continue_bb(BB *parent_bb, BB **save) {
  *save = s_continue_bb;
  BB *bb = bb_split(parent_bb);
  s_continue_bb = bb;
  return bb;
}

static BB *push_break_bb(BB *parent_bb, BB **save) {
  *save = s_break_bb;
  BB *bb = bb_split(parent_bb);
  s_break_bb = bb;
  return bb;
}

static int arrange_variadic_func_params(Scope *scope) {
  // Arrange parameters increasing order in stack,
  // and each parameter occupies sizeof(intptr_t).
  for (int i = 0; i < scope->vars->len; ++i) {
    VarInfo *varinfo = (VarInfo*)scope->vars->data[i];
    VReg *vreg = add_new_reg(varinfo->type);
    vreg_spill(vreg);
    vreg->offset = (i - MAX_REG_ARGS) * WORD_SIZE;
    varinfo->reg = vreg;
  }
  return MAX_REG_ARGS * WORD_SIZE;
}

static void alloc_variable_registers(Function *func) {
  for (int i = 0; i < func->all_scopes->len; ++i) {
    Scope *scope = (Scope*)func->all_scopes->data[i];
    if (scope->vars != NULL) {
      if (i == 0 && func->type->func.vaargs) {  // Variadic function parameters.
        // Special arrangement for va_list.
        arrange_variadic_func_params(scope);
      } else {
        for (int j = 0; j < scope->vars->len; ++j) {
          VarInfo *varinfo = (VarInfo*)scope->vars->data[j];
          if (varinfo->flag & (VF_STATIC | VF_EXTERN))
            continue;  // Static variable is not allocated on stack.

          VReg *vreg = add_new_reg(varinfo->type);
          bool spill = false;
          switch (varinfo->type->kind) {
          case TY_ARRAY:
          case TY_STRUCT:
            // Make non-primitive variable spilled.
            spill = true;
            break;
          default:
            break;
          }
          if (i == 0 && j >= MAX_REG_ARGS) {  // Function argument passed through the stack.
            spill = true;
            vreg->offset = (j - MAX_REG_ARGS + 2) * WORD_SIZE;
          }

          if (spill)
            vreg_spill(vreg);

          varinfo->reg = vreg;
        }
      }
    }
  }
}

static void put_args_to_stack(Defun *defun) {
  static const char *kReg8s[] = {DIL, SIL, DL, CL, R8B, R9B};
  static const char *kReg16s[] = {DI, SI, DX, CX, R8W, R9W};
  static const char *kReg32s[] = {EDI, ESI, EDX, ECX, R8D, R9D};
  static const char *kReg64s[] = {RDI, RSI, RDX, RCX, R8, R9};

  // Store arguments into local frame.
  Vector *params = defun->func->params;
  int len = params != NULL ? params->len : 0;
  int n = defun->func->type->func.vaargs ? MAX_REG_ARGS : len;
  for (int i = 0; i < n; ++i) {
    const Type *type;
    int offset;
    if (i < len) {
      const VarInfo *varinfo = (const VarInfo*)params->data[i];
      type = varinfo->type;
      offset = varinfo->reg->offset;
    } else {  // vaargs
      type = &tyLong;
      offset = (i - MAX_REG_ARGS) * WORD_SIZE;
    }

    int size = 0;
    switch (type->kind) {
    case TY_NUM:
      switch (type->num.kind) {
      case NUM_CHAR:  size = 1; break;
      case NUM_SHORT: size = 2; break;
      case NUM_INT: case NUM_ENUM:
        size = 4;
        break;
      case NUM_LONG:  size = 8; break;
      default: assert(false); break;
      }
      break;
    case TY_PTR:  size = 8; break;
    default: assert(false); break;
    }

    if (i < MAX_REG_ARGS) {
      switch (size) {
      case 1:
        MOV(kReg8s[i], OFFSET_INDIRECT(offset, RBP));
        break;
      case 2:
        MOV(kReg16s[i], OFFSET_INDIRECT(offset, RBP));
        break;
      case 4:
        MOV(kReg32s[i], OFFSET_INDIRECT(offset, RBP));
        break;
      case 8:
        MOV(kReg64s[i], OFFSET_INDIRECT(offset, RBP));
        break;
      default:
        assert(false);
        break;
      }
    }
  }
}

static bool is_asm(Node *node) {
  return node->kind == ND_ASM;
}

static void gen_asm(Node *node) {
  assert(node->asm_.str->kind == EX_STR);
  new_ir_asm(node->asm_.str->str.buf);
}

static void gen_nodes(Vector *nodes) {
  if (nodes == NULL)
    return;

  for (int i = 0, len = nodes->len; i < len; ++i) {
    Node *node = nodes->data[i];
    if (node == NULL)
      continue;
    gen(node);
  }
}

static void gen_defun(Defun *defun) {
  Function *func = defun->func;
  if (func->top_scope == NULL)  // Prototype definition
    return;

  curdefun = defun;
  func->bbcon = new_func_blocks();
  set_curbb(new_bb());
  func->ra = curra = new_reg_alloc();

  // Allocate labels for goto.
  if (defun->label_map != NULL) {
    Map *label_map = defun->label_map;
    for (int i = 0, n = map_count(label_map); i < n; ++i)
      label_map->vals->data[i] = new_bb();
  }

  alloc_variable_registers(func);

  curscope = func->top_scope;
  func->ret_bb = bb_split(curbb);

  // Statements
  gen_nodes(defun->stmts);

  set_curbb(func->ret_bb);
  curbb = NULL;

  func->frame_size = alloc_real_registers(func);

  remove_unnecessary_bb(func->bbcon);

  curdefun = NULL;
  curscope = NULL;
  curra = NULL;
}

static void gen_block(Node *node) {
  if (node->block.nodes != NULL) {
    if (node->block.scope != NULL) {
      assert(curscope == node->block.scope->parent);
      curscope = node->block.scope;
    }
    gen_nodes(node->block.nodes);
    if (node->block.scope != NULL)
      curscope = curscope->parent;
  }
}

static void gen_return(Node *node) {
  BB *bb = bb_split(curbb);
  if (node->return_.val != NULL) {
    VReg *reg = gen_expr(node->return_.val);
    new_ir_result(reg, type_size(node->return_.val->type));
  }
  assert(curdefun != NULL);
  new_ir_jmp(COND_ANY, curdefun->func->ret_bb);
  set_curbb(bb);
}

static void gen_if(Node *node) {
  BB *tbb = bb_split(curbb);
  BB *fbb = bb_split(tbb);
  gen_cond_jmp(node->if_.cond, false, fbb);
  set_curbb(tbb);
  gen(node->if_.tblock);
  if (node->if_.fblock == NULL) {
    set_curbb(fbb);
  } else {
    BB *nbb = bb_split(fbb);
    new_ir_jmp(COND_ANY, nbb);
    set_curbb(fbb);
    gen(node->if_.fblock);
    set_curbb(nbb);
  }
}

static Vector *cur_case_values;
static Vector *cur_case_bbs;

static int compare_cases(const void *pa, const void *pb) {
  const int ia = *(int*)pa;
  const int ib = *(int*)pb;
  intptr_t d = (intptr_t)cur_case_values->data[ia] - (intptr_t)cur_case_values->data[ib];
  return d > 0 ? 1 : d < 0 ? -1 : 0;
}

static void gen_switch_cond_recur(Node *node, VReg *reg, const int *order, int len) {
  Vector *case_values = node->switch_.case_values;
  Expr *value = node->switch_.value;
  size_t size = type_size(value->type);

  if (len <= 2) {
    for (int i = 0; i < len; ++i) {
      BB *nextbb = bb_split(curbb);
      int index = order[i];
      intptr_t x = (intptr_t)case_values->data[index];
      VReg *num = new_ir_imm(x, value->type);
      new_ir_cmp(reg, num, size);
      new_ir_jmp(COND_EQ, cur_case_bbs->data[index]);
      set_curbb(nextbb);
    }
    new_ir_jmp(COND_ANY, cur_case_bbs->data[cur_case_bbs->len - 2]);  // Jump to default.
  } else {
    BB *bbne = bb_split(curbb);
    int m = len >> 1;
    int index = order[m];
    intptr_t x = (intptr_t)case_values->data[index];
    VReg *num = new_ir_imm(x, value->type);
    new_ir_cmp(reg, num, size);
    new_ir_jmp(COND_EQ, cur_case_bbs->data[index]);
    set_curbb(bbne);

    BB *bblt = bb_split(curbb);
    BB *bbgt = bb_split(bblt);
    new_ir_jmp(COND_GT, bbgt);
    set_curbb(bblt);
    gen_switch_cond_recur(node, reg, order, m);
    set_curbb(bbgt);
    gen_switch_cond_recur(node, reg, order + (m + 1), len - (m + 1));
  }
}

static void gen_switch_cond(Node *node) {
  Vector *case_values = node->switch_.case_values;
  int len = case_values->len;

  Expr *value = node->switch_.value;
  VReg *reg = gen_expr(value);

  // Sort cases in increasing order.
#if defined(USE_ALLOCA)
  int *order = alloca(sizeof(int) * len);
#else
  int *order = malloc(sizeof(int) * len);
#endif
  for (int i = 0; i < len; ++i)
    order[i] = i;
  myqsort(order, len, sizeof(int), compare_cases);

  gen_switch_cond_recur(node, reg, order, len);
  set_curbb(bb_split(curbb));
}

static void gen_switch(Node *node) {
  BB *pbb = curbb;

  Vector *save_case_values = cur_case_values;
  Vector *save_case_bbs = cur_case_bbs;
  BB *save_break;
  BB *break_bb = push_break_bb(pbb, &save_break);

  Vector *bbs = new_vector();
  Vector *case_values = node->switch_.case_values;
  int len = case_values->len;
  for (int i = 0; i < len; ++i) {
    BB *bb = bb_split(pbb);
    vec_push(bbs, bb);
    pbb = bb;
  }
  vec_push(bbs, new_bb());  // len+0: Extra label for default.
  vec_push(bbs, break_bb);  // len+1: Extra label for break.

  cur_case_values = case_values;
  cur_case_bbs = bbs;

  gen_switch_cond(node);

  // No bb setting.

  gen(node->switch_.body);

  if (!node->switch_.has_default) {
    // No default: Locate at the end of switch statement.
    BB *bb = bbs->data[len];
    bb_insert(curbb, bb);
    set_curbb(bb);
  }
  set_curbb(break_bb);

  cur_case_values = save_case_values;
  cur_case_bbs = save_case_bbs;
  pop_break_bb(save_break);
}

static void gen_case(Node *node) {
  assert(cur_case_values != NULL);
  assert(cur_case_bbs != NULL);
  Expr *valnode = node->case_.value;
  assert(is_const(valnode));
  intptr_t x = valnode->num.ival;
  int i, len = cur_case_values->len;
  for (i = 0; i < len; ++i) {
    if ((intptr_t)cur_case_values->data[i] == x)
      break;
  }
  assert(i < len);
  assert(i < cur_case_bbs->len);
  set_curbb(cur_case_bbs->data[i]);
}

static void gen_default(void) {
  assert(cur_case_values != NULL);
  assert(cur_case_bbs != NULL);
  int i = cur_case_values->len;  // Label for default is stored at the size of values.
  assert(i < cur_case_bbs->len);
  BB *bb = cur_case_bbs->data[i];
  bb_insert(curbb, bb);
  set_curbb(bb);
}

static void gen_while(Node *node) {
  BB *loop_bb = bb_split(curbb);

  BB *save_break, *save_cont;
  BB *cond_bb = push_continue_bb(loop_bb, &save_cont);
  BB *next_bb = push_break_bb(cond_bb, &save_break);

  new_ir_jmp(COND_ANY, cond_bb);

  set_curbb(loop_bb);
  gen(node->while_.body);

  set_curbb(cond_bb);
  gen_cond_jmp(node->while_.cond, true, loop_bb);

  set_curbb(next_bb);
  pop_continue_bb(save_cont);
  pop_break_bb(save_break);
}

static void gen_do_while(Node *node) {
  BB *loop_bb = bb_split(curbb);

  BB *save_break, *save_cont;
  BB *cond_bb = push_continue_bb(loop_bb, &save_cont);
  BB *next_bb = push_break_bb(cond_bb, &save_break);

  set_curbb(loop_bb);
  gen(node->while_.body);

  set_curbb(cond_bb);
  gen_cond_jmp(node->while_.cond, true, loop_bb);

  set_curbb(next_bb);
  pop_continue_bb(save_cont);
  pop_break_bb(save_break);
}

static void gen_for(Node *node) {
  BB *cond_bb = bb_split(curbb);
  BB *body_bb = bb_split(cond_bb);

  BB *save_break, *save_cont;
  BB *continue_bb = push_continue_bb(body_bb, &save_cont);
  BB *next_bb = push_break_bb(continue_bb, &save_break);

  if (node->for_.pre != NULL)
    gen_expr_stmt(node->for_.pre);

  set_curbb(cond_bb);

  if (node->for_.cond != NULL)
    gen_cond_jmp(node->for_.cond, false, next_bb);

  set_curbb(body_bb);
  gen(node->for_.body);

  set_curbb(continue_bb);
  if (node->for_.post != NULL)
    gen_expr_stmt(node->for_.post);
  new_ir_jmp(COND_ANY, cond_bb);

  set_curbb(next_bb);
  pop_continue_bb(save_cont);
  pop_break_bb(save_break);
}

static void gen_break(void) {
  assert(s_break_bb != NULL);
  BB *bb = bb_split(curbb);
  new_ir_jmp(COND_ANY, s_break_bb);
  set_curbb(bb);
}

static void gen_continue(void) {
  assert(s_continue_bb != NULL);
  BB *bb = bb_split(curbb);
  new_ir_jmp(COND_ANY, s_continue_bb);
  set_curbb(bb);
}

static void gen_goto(Node *node) {
  assert(curdefun->label_map != NULL);
  BB *bb = map_get(curdefun->label_map, node->goto_.ident);
  assert(bb != NULL);
  new_ir_jmp(COND_ANY, bb);
}

static void gen_label(Node *node) {
  assert(curdefun->label_map != NULL);
  BB *bb = map_get(curdefun->label_map, node->label.name);
  assert(bb != NULL);
  bb_insert(curbb, bb);
  set_curbb(bb);
  gen(node->label.stmt);
}

static void gen_clear_local_var(const VarInfo *varinfo) {
  // Fill with zeros regardless of variable type.
  VReg *reg = new_ir_bofs(varinfo->reg);
  new_ir_clear(reg, type_size(varinfo->type));
}

static void gen_vardecl(Node *node) {
  if (curdefun != NULL) {
    Vector *decls = node->vardecl.decls;
    for (int i = 0; i < decls->len; ++i) {
      VarDecl *decl = decls->data[i];
      if (decl->init == NULL)
        continue;
      Scope *scope = curscope;
      VarInfo *varinfo = scope_find(&scope, decl->ident->ident);
      if (varinfo == NULL || (varinfo->flag & (VF_STATIC | VF_EXTERN)) ||
          !(varinfo->type->kind == TY_STRUCT ||
            varinfo->type->kind == TY_ARRAY))
        continue;
      gen_clear_local_var(varinfo);
    }
  }
  gen_nodes(node->vardecl.inits);
}

static void gen_expr_stmt(Expr *expr) {
  gen_expr(expr);
}

static void gen_toplevel(Node *node) {
  gen_nodes(node->toplevel.nodes);
}

void gen(Node *node) {
  if (node == NULL)
    return;

  switch (node->kind) {
  case ND_EXPR:  gen_expr_stmt(node->expr); break;
  case ND_DEFUN:
    gen_defun(node->defun);
    break;
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
  case ND_ASM:  gen_asm(node); break;
  case ND_TOPLEVEL:
    gen_toplevel(node);
    break;

  default:
    error("Unhandled node: %d", node->kind);
    break;
  }
}

////////////////////////////////////////////////

static void emit_defun(Defun *defun) {
  Function *func = defun->func;
  if (func->top_scope == NULL)  // Prototype definition
    return;

  assert(stackpos == 8);

  _TEXT();

  bool global = true;
  const VarInfo *varinfo = find_global(func->name);
  if (varinfo != NULL) {
    global = (varinfo->flag & VF_STATIC) == 0;
  }

  if (global) {
    const char *gl = MANGLE(func->name);
    _GLOBL(gl);
    EMIT_LABEL(gl);
  } else {
    emit_comment("%s: static func", func->name);
    EMIT_LABEL(func->name);
  }

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

  // Prologue
  // Allocate variable bufer.
  if (!no_stmt) {
    PUSH(RBP); PUSH_STACK_POS();
    MOV(RSP, RBP);
    if (func->frame_size > 0) {
      SUB(IM(func->frame_size), RSP);
      stackpos += func->frame_size;
    }

    put_args_to_stack(defun);

    // Callee save.
    push_callee_save_regs(func);
  }

  emit_bb_irs(func->bbcon);

  // Epilogue
  if (!no_stmt) {
    pop_callee_save_regs(func);

    MOV(RBP, RSP);
    stackpos -= func->frame_size;
    POP(RBP); POP_STACK_POS();
  }

  RET();
  emit_comment(NULL);
  assert(stackpos == 8);
}

static void emit_data(void) {
  _RODATA();
  put_rodata();

  emit_comment(NULL);
  _DATA();
  put_rwdata();

  emit_comment(NULL);
  emit_comment("bss");
  put_bss();
}

void emit_code(Node *node) {
  if (node == NULL)
    return;

  switch (node->kind) {
  case ND_DEFUN:
    emit_defun(node->defun);
    break;
  case ND_TOPLEVEL:
    for (int i = 0, len = node->toplevel.nodes->len; i < len; ++i) {
      Node *child = node->toplevel.nodes->data[i];
      if (child == NULL)
        continue;
      emit_code(child);
    }
    emit_data();
    break;
  case ND_VARDECL:
    break;

  default:
    error("Unhandled node in emit_code: %d", node->kind);
    break;
  }
}
#include "codegen.h"

#include <assert.h>
#include <stdlib.h>  // malloc

#include "expr.h"
#include "ir.h"
#include "parser.h"  // Initializer
#include "regalloc.h"
#include "sema.h"
#include "type.h"
#include "util.h"
#include "var.h"
#include "x86_64.h"

VReg *add_new_reg(const Type *type) {
  return reg_alloc_spawn(curdefun->func->ra, type);
}

static void gen_test_opcode(VReg *reg, const Type *type) {
  int size = type_size(type);
  switch (type->kind) {
  case TY_NUM: case TY_PTR:
    break;
  case TY_ARRAY: case TY_FUNC:
    size = WORD_SIZE;
    break;
  default: assert(false); break;
  }

  new_ir_test(reg, size);
}

static enum ConditionKind swap_cond(enum ConditionKind cond) {
  assert(COND_EQ <= cond && cond <= COND_GT);
  if (cond >= COND_LT)
    cond = COND_GT - (cond - COND_LT);
  return cond;
}

static enum ConditionKind gen_compare_expr(enum ExprKind kind, Expr *lhs, Expr *rhs) {
  const Type *ltype = lhs->type;
  UNUSED(ltype);
  assert(ltype->kind == rhs->type->kind);

  enum ConditionKind cond = kind + (COND_EQ - EX_EQ);
  if (rhs->kind != EX_NUM && lhs->kind == EX_NUM) {
    Expr *tmp = lhs;
    lhs = rhs;
    rhs = tmp;
    cond = swap_cond(cond);
  }

  VReg *lhs_reg = gen_expr(lhs);
  if (rhs->kind == EX_NUM && rhs->num.ival == 0 &&
      (cond == COND_EQ || cond == COND_NE)) {
    gen_test_opcode(lhs_reg, lhs->type);
  } else if (rhs->kind == EX_NUM && (lhs->type->num.kind != NUM_LONG || is_im32(rhs->num.ival))) {
    VReg *num = new_ir_imm(rhs->num.ival, rhs->type);
    new_ir_cmp(lhs_reg, num, type_size(lhs->type));
  } else {
    switch (lhs->type->kind) {
    case TY_NUM: case TY_PTR:
      break;
    default: assert(false); break;
    }

    VReg *rhs_reg = gen_expr(rhs);
    // Allocate new register to avoid comparing spilled registers.
    int size = type_size(lhs->type);
    VReg *tmp = add_new_reg(lhs->type);
    new_ir_mov(tmp, lhs_reg, size);
    new_ir_cmp(tmp, rhs_reg, size);
  }

  return cond;
}

void gen_cond_jmp(Expr *cond, bool tf, BB *bb) {
  // Local optimization: if `cond` is compare expression, then
  // jump using flags after CMP directly.
  switch (cond->kind) {
  case EX_NUM:
    if (cond->num.ival == 0)
      tf = !tf;
    if (tf)
      new_ir_jmp(COND_ANY, bb);
    return;

  case EX_EQ:
  case EX_NE:
    {
      enum ConditionKind kind = gen_compare_expr(cond->kind, cond->bop.lhs, cond->bop.rhs);
      if (kind != COND_EQ)
        tf = !tf;
      if (tf)
        new_ir_jmp(COND_EQ, bb);
      else
        new_ir_jmp(COND_NE, bb);
      return;
    }
  case EX_LT:
  case EX_GT:
  case EX_LE:
  case EX_GE:
    {
      enum ConditionKind kind = gen_compare_expr(cond->kind, cond->bop.lhs, cond->bop.rhs);
      switch (kind) {
      case COND_LT:
      case COND_GE:
        if (kind != COND_LT)
          tf = !tf;
        if (tf)
          new_ir_jmp(COND_LT, bb);
        else
          new_ir_jmp(COND_GE, bb);
        break;
      case COND_GT:
      case COND_LE:
        if (kind != COND_GT)
          tf = !tf;
        if (tf)
          new_ir_jmp(COND_GT, bb);
        else
          new_ir_jmp(COND_LE, bb);
        break;
      default:  assert(false); break;
      }
    }
    return;
  case EX_NOT:
    gen_cond_jmp(cond->unary.sub, !tf, bb);
    return;
  case EX_LOGAND:
    if (!tf) {
      BB *bb1 = bb_split(curbb);
      BB *bb2 = bb_split(bb1);
      gen_cond_jmp(cond->bop.lhs, false, bb);
      set_curbb(bb1);
      gen_cond_jmp(cond->bop.rhs, false, bb);
      set_curbb(bb2);
    } else {
      BB *bb1 = bb_split(curbb);
      BB *bb2 = bb_split(bb1);
      gen_cond_jmp(cond->bop.lhs, false, bb2);
      set_curbb(bb1);
      gen_cond_jmp(cond->bop.rhs, true, bb);
      set_curbb(bb2);
    }
    return;
  case EX_LOGIOR:
    if (tf) {
      BB *bb1 = bb_split(curbb);
      BB *bb2 = bb_split(bb1);
      gen_cond_jmp(cond->bop.lhs, true, bb);
      set_curbb(bb1);
      gen_cond_jmp(cond->bop.rhs, true, bb);
      set_curbb(bb2);
    } else {
      BB *bb1 = bb_split(curbb);
      BB *bb2 = bb_split(bb1);
      gen_cond_jmp(cond->bop.lhs, true, bb2);
      set_curbb(bb1);
      gen_cond_jmp(cond->bop.rhs, false, bb);
      set_curbb(bb2);
    }
    return;
  default:
    break;
  }

  VReg *reg = gen_expr(cond);
  gen_test_opcode(reg, cond->type);
  new_ir_jmp(tf ? COND_NE : COND_EQ, bb);
}

static VReg *gen_cast(VReg *reg, const Type *ltype, const Type *rtype) {
  size_t dst_size = type_size(ltype);
  size_t src_size;
  if (rtype->kind == TY_ARRAY)
    src_size = WORD_SIZE;
  else
    src_size = type_size(rtype);
  if (dst_size == src_size)
    return reg;
  return new_ir_cast(reg, ltype, src_size);
}

static VReg *gen_lval(Expr *expr) {
  switch (expr->kind) {
  case EX_VARREF:
    if (expr->varref.scope == NULL) {
      const VarInfo *varinfo = find_global(expr->varref.ident);
      assert(varinfo != NULL);
      return new_ir_iofs(expr->varref.ident, (varinfo->flag & VF_STATIC) == 0);
    } else {
      Scope *scope = expr->varref.scope;
      const VarInfo *varinfo = scope_find(&scope, expr->varref.ident);
      assert(varinfo != NULL);
      assert(!(varinfo->flag & VF_STATIC));
      if (varinfo->flag & VF_EXTERN)
        return new_ir_iofs(expr->varref.ident, true);
      else
        return new_ir_bofs(varinfo->reg);
    }
  case EX_DEREF:
    return gen_expr(expr->unary.sub);
  case EX_MEMBER:
    {
      const Type *type = expr->member.target->type;
      if (type->kind == TY_PTR || type->kind == TY_ARRAY)
        type = type->pa.ptrof;
      assert(type->kind == TY_STRUCT);
      calc_struct_size(type->struct_.info);
      const Vector *members = type->struct_.info->members;
      const VarInfo *member = members->data[expr->member.index];

      VReg *reg;
      if (expr->member.target->type->kind == TY_PTR)
        reg = gen_expr(expr->member.target);
      else
        reg = gen_lval(expr->member.target);
      if (member->struct_.offset == 0)
        return reg;
      VReg *imm = new_ir_imm(member->struct_.offset, &tySize);
      VReg *result = new_ir_bop(IR_ADD, reg, imm, &tySize);
      return result;
    }
  default:
    error("No lvalue: %d", expr->kind);
    break;
  }
  return NULL;
}

static VReg *gen_varref(Expr *expr) {
  switch (expr->type->kind) {
  case TY_NUM:
  case TY_PTR:
    {
      Scope *scope = expr->varref.scope;
      const VarInfo *varinfo = scope_find(&scope, expr->varref.ident);
      if (varinfo != NULL && !(varinfo->flag & (VF_STATIC | VF_EXTERN))) {
        assert(varinfo->reg != NULL);
        return varinfo->reg;
      }

      VReg *reg = gen_lval(expr);
      VReg *result = new_ir_unary(IR_LOAD, reg, expr->type);
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
  BB *tbb = bb_split(curbb);
  BB *fbb = bb_split(tbb);
  BB *nbb = bb_split(fbb);

  VReg *result = add_new_reg(expr->type);
  gen_cond_jmp(expr->ternary.cond, false, fbb);

  set_curbb(tbb);
  VReg *tval = gen_expr(expr->ternary.tval);
  new_ir_mov(result, tval, type_size(expr->ternary.tval->type));
  new_ir_jmp(COND_ANY, nbb);

  set_curbb(fbb);
  VReg *fval = gen_expr(expr->ternary.fval);
  new_ir_mov(result, fval, type_size(expr->ternary.fval->type));

  set_curbb(nbb);
  return result;
}

static VReg *gen_funcall(Expr *expr) {
  Expr *func = expr->funcall.func;
  Vector *args = expr->funcall.args;
  int arg_count = args != NULL ? args->len : 0;

  bool *stack_aligned = malloc(sizeof(bool));
  *stack_aligned = false;

  new_ir_precall(arg_count, stack_aligned);

  const VarInfo *varinfo = NULL;
  bool global = false;
  if (func->kind == EX_VARREF) {
    if (func->varref.scope == NULL) {
      varinfo = find_global(func->varref.ident);
      assert(varinfo != NULL);
      global = (varinfo->flag & VF_STATIC) == 0;
    } else {
      Scope *scope = func->varref.scope;
      varinfo = scope_find(&scope, func->varref.ident);
      assert(varinfo != NULL);
      global = (varinfo->flag & VF_EXTERN) != 0;
    }
  }

  if (args != NULL) {
    if (arg_count > MAX_REG_ARGS) {
      bool vaargs = false;
      if (func->kind == EX_VARREF && func->varref.scope == NULL) {
        vaargs = varinfo->type->func.vaargs;
      } else {
        // TODO:
      }

      if (vaargs)
        error("Param count exceeds %d (%d)", MAX_REG_ARGS, arg_count);
    }

    for (int i = arg_count; --i >= 0; ) {
      Expr *arg = args->data[i];
      VReg *reg = gen_expr(arg);
      new_ir_pusharg(reg);
    }
  }

  VReg *result_reg = NULL;
  if (func->kind == EX_VARREF &&
      (global || varinfo->flag & VF_EXTERN)) {
    result_reg = new_ir_call(func->varref.ident, global, NULL, arg_count,
                             func->type->func.ret, stack_aligned);
  } else {
    VReg *freg = gen_expr(func);
    result_reg = new_ir_call(NULL, false, freg, arg_count,
                             func->type->func.ret, stack_aligned);
  }

  return result_reg;
}

VReg *gen_arith(enum ExprKind kind, const Type *type, VReg *lhs, VReg *rhs) {
  switch (kind) {
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
    {
      VReg *result = new_ir_bop(kind + (IR_ADD - EX_ADD), lhs, rhs, type);
      return result;
    }

  default:
    assert(false);
    return NULL;
  }
}

VReg *gen_expr(Expr *expr) {
  switch (expr->kind) {
  case EX_NUM:
    assert(expr->type->kind == TY_NUM);
    return new_ir_imm(expr->num.ival, expr->type);

  case EX_STR:
    {
      Initializer *init = malloc(sizeof(*init));
      init->kind = vSingle;
      init->single = expr;

      Type* strtype = arrayof(&tyChar, expr->str.size);
      const VarInfo *varinfo = str_to_char_array(strtype, init);
      return new_ir_iofs(varinfo->name, false);
    }

  case EX_SIZEOF:
    return new_ir_imm(type_size(expr->sizeof_.type), expr->type);

  case EX_VARREF:
    return gen_varref(expr);

  case EX_REF:
    {
      Expr *sub = expr->unary.sub;
      if (sub->kind == EX_VARREF && sub->varref.scope != NULL) {
        Scope *scope = sub->varref.scope;
        const VarInfo *varinfo = scope_find(&scope, sub->varref.ident);
        assert(varinfo != NULL);
        if (!(varinfo->flag & (VF_STATIC | VF_EXTERN))) {
          vreg_spill(varinfo->reg);
        }
      }
      return gen_lval(sub);
    }

  case EX_DEREF:
    {
      VReg *reg = gen_expr(expr->unary.sub);
      VReg *result;
      switch (expr->type->kind) {
      case TY_NUM:
      case TY_PTR:
        result = new_ir_unary(IR_LOAD, reg, expr->type);
        return result;

      default:
        assert(false);
        // Fallthrough to suppress compile error.
      case TY_ARRAY:
      case TY_STRUCT:
        // array and struct values are handled as a pointer.
        return reg;
      }
    }

  case EX_MEMBER:
    {
      VReg *reg = gen_lval(expr);
      VReg *result;
      switch (expr->type->kind) {
      case TY_NUM:
      case TY_PTR:
        result = new_ir_unary(IR_LOAD, reg, expr->type);
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
    {
      VReg *result = NULL;
      Vector *list = expr->comma.list;
      for (int i = 0, len = list->len; i < len; ++i) {
        result = gen_expr(list->data[i]);
      }
      return result;
    }

  case EX_TERNARY:
    return gen_ternary(expr);

  case EX_CAST:
    if (expr->unary.sub->kind == EX_NUM) {
      assert(expr->unary.sub->type->kind == TY_NUM);
      intptr_t value = expr->unary.sub->num.ival;
      switch (expr->unary.sub->type->num.kind) {
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

      return new_ir_imm(value, expr->type);
    } else {
      VReg *reg = gen_expr(expr->unary.sub);
      return gen_cast(reg, expr->type, expr->unary.sub->type);
    }

  case EX_ASSIGN:
    {
      VReg *src = gen_expr(expr->bop.rhs);
      if (expr->bop.lhs->kind == EX_VARREF) {
        Expr *lhs = expr->bop.lhs;
        switch (lhs->type->kind) {
        case TY_NUM:
        case TY_PTR:
          {
            Scope *scope = lhs->varref.scope;
            const VarInfo *varinfo = scope_find(&scope, lhs->varref.ident);
            if (varinfo != NULL && !(varinfo->flag & (VF_STATIC | VF_EXTERN))) {
              assert(varinfo->reg != NULL);
              new_ir_mov(varinfo->reg, src, type_size(lhs->type));
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
      case TY_NUM:
      case TY_PTR:
#if 0
        new_ir_store(dst, tmp, type_size(expr->type));
#else
        // To avoid both spilled registers, add temporary register.
        {
          VReg *tmp = add_new_reg(expr->type);
          new_ir_mov(tmp, src, type_size(expr->type));
          new_ir_store(dst, tmp, type_size(expr->type));
        }
#endif
        break;
      case TY_STRUCT:
        new_ir_memcpy(dst, src, expr->type->struct_.info->size);
        break;
      }
      return src;
    }

  case EX_ASSIGN_WITH:
    {
      Expr *sub = expr->unary.sub;
      if (sub->bop.lhs->kind == EX_VARREF && sub->bop.lhs->varref.scope != NULL) {
        VReg *lhs = gen_expr(sub->bop.lhs);
        VReg *rhs = gen_expr(sub->bop.rhs);
        VReg *result = gen_arith(sub->kind, sub->type, lhs, rhs);
        new_ir_mov(lhs, result, type_size(sub->bop.lhs->type));
        return result;
      } else {
        VReg *lval = gen_lval(sub->bop.lhs);
        VReg *rhs = gen_expr(sub->bop.rhs);
        VReg *lhs = new_ir_unary(IR_LOAD, lval, sub->bop.lhs->type);
        VReg *result = gen_arith(sub->kind, sub->type, lhs, rhs);
        VReg *cast = gen_cast(result, expr->type, sub->type);
        new_ir_store(lval, cast, type_size(expr->type));
        return result;
      }
    }

  case EX_PREINC:
  case EX_PREDEC:
    {
      size_t value = 1;
      if (expr->type->kind == TY_PTR)
        value = type_size(expr->type->pa.ptrof);
      int size = type_size(expr->type);

      Expr *sub = expr->unary.sub;
      if (sub->kind == EX_VARREF) {
        Scope *scope = sub->varref.scope;
        const VarInfo *varinfo = scope_find(&scope, sub->varref.ident);
        if (varinfo != NULL && !(varinfo->flag & (VF_STATIC | VF_EXTERN))) {
          VReg *num = new_ir_imm(value, expr->type);
          VReg *result = new_ir_bop(expr->kind == EX_PREINC ? IR_ADD : IR_SUB,
                                    varinfo->reg, num, expr->type);
          new_ir_mov(varinfo->reg, result, size);
          return result;
        }
      }

      VReg *lval = gen_lval(sub);
      new_ir_incdec(expr->kind == EX_PREINC ? IR_INC : IR_DEC,
                    lval, size, value);
      VReg *result = new_ir_unary(IR_LOAD, lval, expr->type);
      return result;
    }

  case EX_POSTINC:
  case EX_POSTDEC:
    {
      size_t value = 1;
      if (expr->type->kind == TY_PTR)
        value = type_size(expr->type->pa.ptrof);
      int size = type_size(expr->type);

      Expr *sub = expr->unary.sub;
      if (sub->kind == EX_VARREF) {
        Scope *scope = sub->varref.scope;
        const VarInfo *varinfo = scope_find(&scope, sub->varref.ident);
        if (varinfo != NULL && !(varinfo->flag & (VF_STATIC | VF_EXTERN))) {
          VReg *org_val = add_new_reg(sub->type);
          new_ir_mov(org_val, varinfo->reg, size);
          VReg *num = new_ir_imm(value, expr->type);
          VReg *result = new_ir_bop(expr->kind == EX_POSTINC ? IR_ADD : IR_SUB,
                                    varinfo->reg, num, expr->type);
          new_ir_mov(varinfo->reg, result, size);
          return org_val;
        }
      }

      VReg *lval = gen_lval(expr->unary.sub);
      VReg *result = new_ir_unary(IR_LOAD, lval, expr->type);
      new_ir_incdec(expr->kind == EX_POSTINC ? IR_INC : IR_DEC,
                    lval, size, value);
      return result;
    }

  case EX_FUNCALL:
    return gen_funcall(expr);

  case EX_NEG:
    {
      VReg *reg = gen_expr(expr->unary.sub);
      VReg *result = new_ir_unary(IR_NEG, reg, expr->type);
      return result;
    }

  case EX_NOT:
    {
      VReg *reg = gen_expr(expr->unary.sub);
      VReg *result;
      switch (expr->unary.sub->type->kind) {
      case TY_NUM: case TY_PTR:
        result = new_ir_unary(IR_NOT, reg, expr->type);
        break;
      default:
        assert(false);
        // Fallthrough to suppress compile error
      case TY_ARRAY: case TY_FUNC:
        // Array is handled as a pointer.
        result = new_ir_unary(IR_NOT, reg, expr->type);
        break;
      }
      return result;
    }

  case EX_BITNOT:
    {
      VReg *reg = gen_expr(expr->unary.sub);
      VReg *result = new_ir_unary(IR_BITNOT, reg, expr->type);
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
      return new_ir_set(cond);
    }

  case EX_LOGAND:
    {
      BB *bb1 = bb_split(curbb);
      BB *bb2 = bb_split(bb1);
      BB *false_bb = bb_split(bb2);
      BB *next_bb = bb_split(false_bb);
      gen_cond_jmp(expr->bop.lhs, false, false_bb);
      set_curbb(bb1);
      gen_cond_jmp(expr->bop.rhs, false, false_bb);
      set_curbb(bb2);
      VReg *result = new_ir_imm(true, &tyBool);
      new_ir_jmp(COND_ANY, next_bb);
      set_curbb(false_bb);
      VReg *result2 = new_ir_imm(false, &tyBool);
      new_ir_mov(result, result2, type_size(&tyBool));
      set_curbb(next_bb);
      return result;
    }

  case EX_LOGIOR:
    {
      BB *bb1 = bb_split(curbb);
      BB *bb2 = bb_split(bb1);
      BB *true_bb = bb_split(bb2);
      BB *next_bb = bb_split(true_bb);
      gen_cond_jmp(expr->bop.lhs, true, true_bb);
      set_curbb(bb1);
      gen_cond_jmp(expr->bop.rhs, true, true_bb);
      set_curbb(bb2);
      VReg *result = new_ir_imm(false, &tyBool);
      new_ir_jmp(COND_ANY, next_bb);
      set_curbb(true_bb);
      VReg *result2 = new_ir_imm(true, &tyBool);
      new_ir_mov(result, result2, type_size(&tyBool));
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

  default:
    fprintf(stderr, "Expr kind=%d, ", expr->kind);
    assert(!"Unhandled in gen_expr");
    break;
  }

  return NULL;
}
#include "ir.h"

#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>

#include "codegen.h"
#include "regalloc.h"
#include "type.h"
#include "util.h"
#include "var.h"
#include "x86_64.h"

static enum ConditionKind invert_cond(enum ConditionKind cond) {
  assert(COND_EQ <= cond && cond <= COND_GT);
  if (cond <= COND_NE)
    return COND_NE + COND_EQ - cond;
  return COND_LT + ((cond - COND_LT) ^ 2);
}

// Virtual register

VReg *new_vreg(int vreg_no, const Type *type) {
  VReg *vreg = malloc(sizeof(*vreg));
  vreg->v = vreg_no;
  vreg->r = -1;
  vreg->type = type;
  vreg->offset = 0;
  return vreg;
}

void vreg_spill(VReg *vreg) {
  vreg->r = SPILLED_REG_NO;
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

#define CALLEE_SAVE_REG_COUNT  (5)
static struct {
  const char * reg;
  short bit;
} const kCalleeSaveRegs[] = {
  {RBX, 1 << 0},
  {R12, 1 << 3},
  {R13, 1 << 4},
  {R14, 1 << 5},
  {R15, 1 << 6},
};

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

VReg *new_ir_imm(intptr_t value, const Type *type) {
  IR *ir = new_ir(IR_IMM);
  ir->value = value;
  ir->size = type_size(type);
  return ir->dst = reg_alloc_spawn(curra, type);
}

VReg *new_ir_bop(enum IrKind kind, VReg *opr1, VReg *opr2, const Type *type) {
  IR *ir = new_ir(kind);
  ir->opr1 = opr1;
  ir->opr2 = opr2;
  ir->size = type_size(type);
  return ir->dst = reg_alloc_spawn(curra, type);
}

VReg *new_ir_unary(enum IrKind kind, VReg *opr, const Type *type) {
  IR *ir = new_ir(kind);
  ir->opr1 = opr;
  ir->size = type_size(type);
  return ir->dst = reg_alloc_spawn(curra, type);
}

VReg *new_ir_bofs(VReg *src) {
  IR *ir = new_ir(IR_BOFS);
  ir->opr1 = src;
  ir->size = WORD_SIZE;
  return ir->dst = reg_alloc_spawn(curra, &tyVoidPtr);
}

VReg *new_ir_iofs(const char *label, bool global) {
  IR *ir = new_ir(IR_IOFS);
  ir->iofs.label = label;
  ir->iofs.global = global;
  ir->size = WORD_SIZE;
  return ir->dst = reg_alloc_spawn(curra, &tyVoidPtr);
}

void new_ir_store(VReg *dst, VReg *src, int size) {
  IR *ir = new_ir(IR_STORE);
  ir->opr1 = src;
  ir->size = size;
  ir->opr2 = dst;  // `dst` is used by indirect, so it is not actually `dst`.
}

void new_ir_memcpy(VReg *dst, VReg *src, int size) {
  IR *ir = new_ir(IR_MEMCPY);
  ir->opr1 = src;
  ir->opr2 = dst;
  ir->size = size;
}


void new_ir_cmp(VReg *opr1, VReg *opr2, int size) {
  IR *ir = new_ir(IR_CMP);
  ir->opr1 = opr1;
  ir->opr2 = opr2;
  ir->size = size;
}

void new_ir_test(VReg *reg, int size) {
  IR *ir = new_ir(IR_TEST);
  ir->opr1 = reg;
  ir->size = size;
}

void new_ir_incdec(enum IrKind kind, VReg *reg, int size, intptr_t value) {
  IR *ir = new_ir(kind);
  ir->opr1 = reg;
  ir->size = size;
  ir->value = value;
}

VReg *new_ir_set(enum ConditionKind cond) {
  IR *ir = new_ir(IR_SET);
  ir->set.cond = cond;
  return ir->dst = reg_alloc_spawn(curra, &tyBool);
}

void new_ir_jmp(enum ConditionKind cond, BB *bb) {
  IR *ir = new_ir(IR_JMP);
  ir->jmp.bb = bb;
  ir->jmp.cond = cond;
}

void new_ir_pusharg(VReg *vreg) {
  IR *ir = new_ir(IR_PUSHARG);
  ir->opr1 = vreg;
  ir->size = WORD_SIZE;
}

void new_ir_precall(int arg_count, bool *stack_aligned) {
  IR *ir = new_ir(IR_PRECALL);
  ir->call.stack_aligned = stack_aligned;
  ir->call.arg_count = arg_count;
}

VReg *new_ir_call(const char *label, bool global, VReg *freg, int arg_count, const Type *result_type, bool *stack_aligned) {
  IR *ir = new_ir(IR_CALL);
  ir->call.label = label;
  ir->call.global = global;
  ir->opr1 = freg;
  ir->call.stack_aligned = stack_aligned;
  ir->call.arg_count = arg_count;
  ir->size = type_size(result_type);
  return ir->dst = reg_alloc_spawn(curra, result_type);
}

void new_ir_addsp(int value) {
  IR *ir = new_ir(IR_ADDSP);
  ir->value = value;
}

VReg *new_ir_cast(VReg *vreg, const Type *dsttype, int srcsize) {
  IR *ir = new_ir(IR_CAST);
  ir->opr1 = vreg;
  ir->size = type_size(dsttype);
  ir->cast.srcsize = srcsize;
  return ir->dst = reg_alloc_spawn(curra, dsttype);
}

void new_ir_mov(VReg *dst, VReg *src, int size) {
  IR *ir = new_ir(IR_MOV);
  ir->dst = dst;
  ir->opr1 = src;
  ir->size = size;
}

void new_ir_clear(VReg *reg, size_t size) {
  IR *ir = new_ir(IR_CLEAR);
  ir->size = size;
  ir->opr1 = reg;
}

void new_ir_result(VReg *reg, int size) {
  IR *ir = new_ir(IR_RESULT);
  ir->opr1 = reg;
  ir->size = size;
}

void new_ir_asm(const char *asm_) {
  IR *ir = new_ir(IR_ASM);
  ir->asm_.str = asm_;
}

IR *new_ir_load_spilled(int offset, int size) {
  IR *ir = new_ir(IR_LOAD_SPILLED);
  ir->value = offset;
  ir->size = size;
  return ir;
}

IR *new_ir_store_spilled(int offset, int size) {
  IR *ir = new_ir(IR_STORE_SPILLED);
  ir->value = offset;
  ir->size = size;
  return ir;
}

static void ir_memcpy(int dst_reg, int src_reg, ssize_t size) {
  const char *dst = kReg64s[dst_reg];
  const char *src = kReg64s[src_reg];

  // Break %rcx, %dl
  switch (size) {
  case 1:
    MOV(INDIRECT(src), DL);
    MOV(DL, INDIRECT(dst));
    break;
  case 2:
    MOV(INDIRECT(src), DX);
    MOV(DX, INDIRECT(dst));
    break;
  case 4:
    MOV(INDIRECT(src), EDX);
    MOV(EDX, INDIRECT(dst));
    break;
  case 8:
    MOV(INDIRECT(src), RDX);
    MOV(RDX, INDIRECT(dst));
    break;
  default:
    {
      const char * label = alloc_label();
      PUSH(src);
      MOV(IM(size), RCX);
      EMIT_LABEL(label);
      MOV(INDIRECT(src), DL);
      MOV(DL, INDIRECT(dst));
      INC(src);
      INC(dst);
      DEC(RCX);
      JNE(label);
      POP(src);
    }
    break;
  }
}

static void ir_out(const IR *ir) {
  switch (ir->kind) {
  case IR_IMM:
    {
      intptr_t value = ir->value;
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *dst = regs[ir->dst->r];
      if (value == 0)
        XOR(dst, dst);
      else
        MOV(IM(value), dst);
      break;
    }
    break;

  case IR_BOFS:
    LEA(OFFSET_INDIRECT(ir->opr1->offset, RBP), kReg64s[ir->dst->r]);
    break;

  case IR_IOFS:
    {
      const char *label = ir->iofs.label;
      if (ir->iofs.global)
        label = MANGLE(label);
      LEA(LABEL_INDIRECT(label, RIP), kReg64s[ir->dst->r]);
    }
    break;

  case IR_LOAD:
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(INDIRECT(kReg64s[ir->opr1->r]), regs[ir->dst->r]);
    }
    break;

  case IR_STORE:
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(regs[ir->opr1->r], INDIRECT(kReg64s[ir->opr2->r]));
    }
    break;

  case IR_MEMCPY:
    ir_memcpy(ir->opr2->r, ir->opr1->r, ir->size);
    break;

  case IR_ADD:
    {
      assert(ir->dst->r == ir->opr1->r);
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      ADD(regs[ir->opr2->r], regs[ir->dst->r]);
    }
    break;

  case IR_SUB:
    {
      assert(ir->dst->r == ir->opr1->r);
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      SUB(regs[ir->opr2->r], regs[ir->dst->r]);
    }
    break;

  case IR_MUL:
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *a = kRegATable[pow];
      MOV(regs[ir->opr1->r], a);
      MUL(regs[ir->opr2->r]);
      MOV(a, regs[ir->dst->r]);
    }
    break;

  case IR_DIV:
    if (ir->size == 1) {
      MOVSX(kReg8s[ir->opr1->r], AX);
      IDIV(kReg8s[ir->opr2->r]);
      MOV(AL, kReg8s[ir->dst->r]);
    } else {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *a = kRegATable[pow];
      MOV(regs[ir->opr1->r], a);
      switch (pow) {
      case 1:  CWTL(); break;
      case 2:  CLTD(); break;
      case 3:  CQTO(); break;
      default: assert(false); break;
      }
      IDIV(regs[ir->opr2->r]);
      MOV(a, regs[ir->dst->r]);
    }
    break;

  case IR_MOD:
    if (ir->size == 1) {
      MOVSX(kReg8s[ir->opr1->r], AX);
      IDIV(kReg8s[ir->opr2->r]);
      MOV(AH, kReg8s[ir->dst->r]);
    } else {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *a = kRegATable[pow];
      const char *d = kRegDTable[pow];
      MOV(regs[ir->opr1->r], a);
      switch (pow) {
      case 1:  CWTL(); break;
      case 2:  CLTD(); break;
      case 3:  CQTO(); break;
      default: assert(false); break;
      }
      IDIV(regs[ir->opr2->r]);
      MOV(d, regs[ir->dst->r]);
    }
    break;

  case IR_BITAND:
    {
      assert(ir->dst->r == ir->opr1->r);
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      AND(regs[ir->opr2->r], regs[ir->dst->r]);
    }
    break;

  case IR_BITOR:
    {
      assert(ir->dst->r == ir->opr1->r);
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      OR(regs[ir->opr2->r], regs[ir->dst->r]);
    }
    break;

  case IR_BITXOR:
    {
      assert(ir->dst->r == ir->opr1->r);
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      XOR(regs[ir->opr2->r], regs[ir->dst->r]);
    }
    break;

  case IR_LSHIFT:
    {
      assert(ir->dst->r == ir->opr1->r);
      MOV(kReg8s[ir->opr2->r], CL);
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      SHL(CL, regs[ir->dst->r]);
    }
    break;
  case IR_RSHIFT:
    {
      assert(ir->dst->r == ir->opr1->r);
      MOV(kReg8s[ir->opr2->r], CL);
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      SHR(CL, regs[ir->dst->r]);
    }
    break;

  case IR_CMP:
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      CMP(regs[ir->opr2->r], regs[ir->opr1->r]);
    }
    break;

  case IR_INC:
    {
      const char *reg = INDIRECT(kReg64s[ir->opr1->r]);
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
      const char *reg = INDIRECT(kReg64s[ir->opr1->r]);
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
      assert(ir->dst->r == ir->opr1->r);
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      NEG(regs[ir->dst->r]);
    }
    break;

  case IR_NOT:
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *opr1 = regs[ir->opr1->r];
      TEST(opr1, opr1);
      const char *dst8 = kReg8s[ir->dst->r];
      SETE(dst8);
      MOVSX(dst8, kReg32s[ir->dst->r]);
    }
    break;

  case IR_BITNOT:
    {
      assert(ir->dst->r == ir->opr1->r);
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      NOT(regs[ir->dst->r]);
    }
    break;

  case IR_SET:
    {
      const char *dst = kReg8s[ir->dst->r];
      switch (ir->set.cond) {
      case COND_EQ:  SETE(dst); break;
      case COND_NE:  SETNE(dst); break;
      case COND_LT:  SETL(dst); break;
      case COND_GT:  SETG(dst); break;
      case COND_LE:  SETLE(dst); break;
      case COND_GE:  SETGE(dst); break;
      default: assert(false); break;
      }
      MOVSX(dst, kReg32s[ir->dst->r]);  // Assume bool is 4 byte.
    }
    break;

  case IR_TEST:
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      const char *opr1 = regs[ir->opr1->r];
      TEST(opr1, opr1);
    }
    break;

  case IR_JMP:
    switch (ir->jmp.cond) {
    case COND_ANY:  JMP(ir->jmp.bb->label); break;
    case COND_EQ:   JE(ir->jmp.bb->label); break;
    case COND_NE:   JNE(ir->jmp.bb->label); break;
    case COND_LT:   JL(ir->jmp.bb->label); break;
    case COND_GT:   JG(ir->jmp.bb->label); break;
    case COND_LE:   JLE(ir->jmp.bb->label); break;
    case COND_GE:   JGE(ir->jmp.bb->label); break;
    default:  assert(false); break;
    }
    break;

  case IR_PRECALL:
    {
      // Caller save.
      PUSH(R10); PUSH_STACK_POS();
      PUSH(R11); PUSH_STACK_POS();

      int stack_args = MAX(ir->call.arg_count - MAX_REG_ARGS, 0);
      bool align_stack = ((stackpos + stack_args * WORD_SIZE) & 15) != 0;
      if (align_stack) {
        SUB(IM(8), RSP);
        stackpos += 8;
      }
      *ir->call.stack_aligned = align_stack;
    }
    break;

  case IR_PUSHARG:
    PUSH(kReg64s[ir->opr1->r]); PUSH_STACK_POS();
    break;

  case IR_CALL:
    {
      static const char *kArgReg64s[] = {RDI, RSI, RDX, RCX, R8, R9};

      // Pop register arguments.
      int reg_args = MIN((int)ir->call.arg_count, MAX_REG_ARGS);
      for (int i = 0; i < reg_args; ++i) {
        POP(kArgReg64s[i]); POP_STACK_POS();
      }

      if (ir->call.label != NULL) {
        if (ir->call.global)
          CALL(MANGLE(ir->call.label));
        else
          CALL(ir->call.label);
      } else {
        CALL(fmt("*%s", kReg64s[ir->opr1->r]));
      }

      int stack_args = MAX(ir->call.arg_count - MAX_REG_ARGS, 0);
      bool align_stack = *ir->call.stack_aligned;
      if (stack_args > 0 || align_stack) {
        int add = stack_args * WORD_SIZE + (align_stack ? 8 : 0);
        ADD(IM(add), RSP);
        stackpos -= add;
      }

      // Resore caller save registers.
      POP(R11); POP_STACK_POS();
      POP(R10); POP_STACK_POS();

      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(kRegATable[pow], regs[ir->dst->r]);
    }
    break;

  case IR_ADDSP:
    if (ir->value > 0)
      ADD(IM(ir->value), RSP);
    else
      SUB(IM(-ir->value), RSP);
    stackpos -= ir->value;
    break;

  case IR_CAST:
    if (ir->size <= ir->cast.srcsize) {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(regs[ir->opr1->r], regs[ir->dst->r]);
    } else {
      assert(0 <= ir->cast.srcsize && ir->cast.srcsize < kPow2TableSize);
      int pows = kPow2Table[ir->cast.srcsize];
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int powd = kPow2Table[ir->size];
      assert(0 <= pows && pows < 4);
      assert(0 <= powd && powd < 4);
      MOVSX(kRegSizeTable[pows][ir->opr1->r], kRegSizeTable[powd][ir->dst->r]); break;
    }
    break;

  case IR_MOV:
    if (ir->opr1->r != ir->dst->r) {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(regs[ir->opr1->r], regs[ir->dst->r]);
    }
    break;

  case IR_CLEAR:
    {
      const char *loop = alloc_label();
      MOV(kReg64s[ir->opr1->r], RSI);
      MOV(IM(ir->size), EDI);
      XOR(AL, AL);
      EMIT_LABEL(loop);
      MOV(AL, INDIRECT(RSI));
      INC(RSI);
      DEC(EDI);
      JNE(loop);
    }
    break;

  case IR_RESULT:
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(regs[ir->opr1->r], kRegATable[pow]);
    }
    break;

  case IR_ASM:
    EMIT_ASM0(ir->asm_.str);
    break;

  case IR_LOAD_SPILLED:
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(OFFSET_INDIRECT(ir->value, RBP), regs[SPILLED_REG_NO]);
    }
    break;

  case IR_STORE_SPILLED:
    {
      assert(0 <= ir->size && ir->size < kPow2TableSize);
      int pow = kPow2Table[ir->size];
      assert(0 <= pow && pow < 4);
      const char **regs = kRegSizeTable[pow];
      MOV(regs[SPILLED_REG_NO], OFFSET_INDIRECT(ir->value, RBP));
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

BB *bb_split(BB *bb) {
  BB *cc = new_bb();
  cc->next = bb->next;
  bb->next = cc;
  return cc;
}

void bb_insert(BB *bb, BB *cc) {
  cc->next = bb->next;
  bb->next = cc;
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
      } else if (bb->irs->len == 1 && (ir = is_last_any_jmp(bb)) != NULL) {  // jmp only.
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

void push_callee_save_regs(Function *func) {
  short used = func->used_reg_bits;
  for (int i = 0; i < CALLEE_SAVE_REG_COUNT; ++i) {
    if (used & kCalleeSaveRegs[i].bit) {
      PUSH(kCalleeSaveRegs[i].reg); PUSH_STACK_POS();
    }
  }
}

void pop_callee_save_regs(Function *func) {
  short used = func->used_reg_bits;
  for (int i = CALLEE_SAVE_REG_COUNT; --i >= 0; ) {
    if (used & kCalleeSaveRegs[i].bit) {
      POP(kCalleeSaveRegs[i].reg); POP_STACK_POS();
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

    EMIT_LABEL(bb->label);
    for (int j = 0; j < bb->irs->len; ++j) {
      IR *ir = bb->irs->data[j];
      ir_out(ir);
    }
  }
}

#if !defined(SELF_HOSTING)
static void dump_ir(FILE *fp, IR *ir) {
  static char *kSize[] = {"0", "b", "w", "3", "d", "5", "6", "7", ""};
  static char *kCond[] = {"MP", "EQ", "NE", "LT", "LE", "GE", "GT"};

  int dst = ir->dst != NULL ? ir->dst->r : -1;
  int opr1 = ir->opr1 != NULL ? ir->opr1->r : -1;
  int opr2 = ir->opr2 != NULL ? ir->opr2->r : -1;

  switch (ir->kind) {
  case IR_IMM:    fprintf(fp, "\tIMM\tR%d%s = %"PRIdPTR"\n", dst, kSize[ir->size], ir->value); break;
  case IR_BOFS:   fprintf(fp, "\tBOFS\tR%d = &[rbp %c %d]\n", dst, ir->opr1->offset > 0 ? '+' : '-', ir->opr1->offset > 0 ? ir->opr1->offset : -ir->opr1->offset); break;
  case IR_IOFS:   fprintf(fp, "\tIOFS\tR%d = &%s\n", dst, ir->iofs.label); break;
  case IR_LOAD:   fprintf(fp, "\tLOAD\tR%d%s = (R%d)\n", dst, kSize[ir->size], opr1); break;
  case IR_STORE:  fprintf(fp, "\tSTORE\t(R%d) = R%d%s\n", opr2, opr1, kSize[ir->size]); break;
  case IR_MEMCPY: fprintf(fp, "\tMEMCPY(dst=R%d, src=R%d, size=%d)\n", opr2, opr1, ir->size); break;
  case IR_ADD:    fprintf(fp, "\tADD\tR%d%s = R%d%s + R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_SUB:    fprintf(fp, "\tSUB\tR%d%s = R%d%s - R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_MUL:    fprintf(fp, "\tMUL\tR%d%s = R%d%s * R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_DIV:    fprintf(fp, "\tDIV\tR%d%s = R%d%s / R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_MOD:    fprintf(fp, "\tMOD\tR%d%s = R%d%s %% R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_BITAND: fprintf(fp, "\tBITAND\tR%d%s = R%d%s & R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_BITOR:  fprintf(fp, "\tBITOR\tR%d%s = R%d%s | R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_BITXOR: fprintf(fp, "\tBITXOR\tR%d%s = R%d%s ^ R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_LSHIFT: fprintf(fp, "\tLSHIFT\tR%d%s = R%d%s << R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_RSHIFT: fprintf(fp, "\tRSHIFT\tR%d%s = R%d%s >> R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_CMP:    fprintf(fp, "\tCMP\tR%d%s - R%d%s\n", opr1, kSize[ir->size], opr2, kSize[ir->size]); break;
  case IR_INC:    fprintf(fp, "\tINC\t(R%d)%s += %"PRIdPTR"\n", opr1, kSize[ir->size], ir->value); break;
  case IR_DEC:    fprintf(fp, "\tDEC\t(R%d)%s -= %"PRIdPTR"\n", opr1, kSize[ir->size], ir->value); break;
  case IR_NEG:    fprintf(fp, "\tNEG\tR%d%s = -R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size]); break;
  case IR_NOT:    fprintf(fp, "\tNOT\tR%d%s = !R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size]); break;
  case IR_BITNOT: fprintf(fp, "\tBIT\tR%d%s = -R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size]); break;
  case IR_SET:    fprintf(fp, "\tSET\tR%d%s = %s\n", dst, kSize[4], kCond[ir->set.cond]); break;
  case IR_TEST:   fprintf(fp, "\tTEST\tR%d%s\n", opr1, kSize[ir->size]); break;
  case IR_JMP:    fprintf(fp, "\tJ%s\t%s\n", kCond[ir->jmp.cond], ir->jmp.bb->label); break;
  case IR_PRECALL: fprintf(fp, "\tPRECALL\n"); break;
  case IR_PUSHARG: fprintf(fp, "\tPUSHARG\tR%d\n", opr1); break;
  case IR_CALL:
    if (ir->call.label != NULL)
      fprintf(fp, "\tCALL\tR%d%s = call %s\n", dst, kSize[ir->size], ir->call.label);
    else
      fprintf(fp, "\tCALL\tR%d%s = *R%d\n", dst, kSize[ir->size], opr1);
    break;
  case IR_ADDSP:  fprintf(fp, "\tADDSP\t%"PRIdPTR"\n", ir->value); break;
  case IR_CAST:   fprintf(fp, "\tCAST\tR%d%s = R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->cast.srcsize]); break;
  case IR_MOV:    fprintf(fp, "\tMOV\tR%d%s = R%d%s\n", dst, kSize[ir->size], opr1, kSize[ir->size]); break;
  case IR_CLEAR:  fprintf(fp, "\tCLEAR\tR%d, %d\n", opr1, ir->size); break;
  case IR_RESULT: fprintf(fp, "\tRESULT\tR%d%s\n", opr1, kSize[ir->size]); break;
  case IR_ASM:    fprintf(fp, "\tASM \"%s\"\n", ir->asm_.str); break;
  case IR_LOAD_SPILLED:   fprintf(fp, "\tLOAD_SPILLED %d(%s)\n", (int)ir->value, kSize[ir->size]); break;
  case IR_STORE_SPILLED:  fprintf(fp, "\tSTORE_SPILLED %d(%s)\n", (int)ir->value, kSize[ir->size]); break;

  default: assert(false); break;
  }
}

void dump_func_ir(Function *func) {
  FILE *fp = stdout;

  BBContainer *bbcon = func->bbcon;
  assert(bbcon != NULL);

  fprintf(fp, "### %s\n\n", func->name);

  fprintf(fp, "params and locals:\n");
  for (int i = 0; i < func->all_scopes->len; ++i) {
    Scope *scope = func->all_scopes->data[i];
    if (scope->vars == NULL)
      continue;
    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      fprintf(fp, "  V%3d: %s\n", varinfo->reg->v, varinfo->name);
    }
  }

#if 0
  RegAlloc *ra = func->ra;
  fprintf(fp, "VREG: #%d\n", ra->regno);
  LiveInterval **sorted_intervals = func->ra->sorted_intervals;
  for (int i = 0; i < ra->regno; ++i) {
    LiveInterval *li = sorted_intervals[i];
    VReg *vreg = ra->vregs->data[li->vreg];
    if (!li->spill) {
      fprintf(fp, "  V%3d: live %3d - %3d, => R%3d\n", li->vreg, li->start, li->end, li->rreg);
    } else {
      fprintf(fp, "  V%3d: live %3d - %3d (spilled, offset=%d)\n", li->vreg, li->start, li->end, vreg->offset);
    }
  }
#endif

  fprintf(fp, "BB: #%d\n", bbcon->bbs->len);
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    fprintf(fp, "// BB %d\n", i);
    fprintf(fp, "%s:\n", bb->label);
    for (int j = 0; j < bb->irs->len; ++j) {
      IR *ir = bb->irs->data[j];
      dump_ir(fp, ir);
    }
  }
  fprintf(fp, "\n");
}
#endif
#include "regalloc.h"

#include <assert.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "codegen.h"  // WORD_SIZE
#include "ir.h"
#include "util.h"
#include "var.h"

// Register allocator

RegAlloc *new_reg_alloc(void) {
  RegAlloc *ra = malloc(sizeof(*ra));
  ra->vregs = new_vector();
  ra->regno = 0;
  vec_clear(ra->vregs);
  return ra;
}

VReg *reg_alloc_spawn(RegAlloc *ra, const Type *type) {
  VReg *vreg = new_vreg(ra->regno++, type);
  vec_push(ra->vregs, vreg);
  return vreg;
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
    case IR_MOD:
    case IR_BITAND:
    case IR_BITOR:
    case IR_BITXOR:
    case IR_LSHIFT:
    case IR_RSHIFT:
    case IR_NEG:  // unary ops
    case IR_BITNOT:
      {
        IR *ir2 = malloc(sizeof(*ir));
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

typedef struct LiveInterval {
  int vreg;
  int rreg;
  int start;
  int end;
  bool spill;
} LiveInterval;

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

static int sort_live_interval(LiveInterval **pa, LiveInterval **pb) {
  LiveInterval *a = *pa, *b = *pb;
  int d = a->start - b->start;
  if (d == 0)
    d = b->end - a->start;
  return d;
}

static void split_at_interval(LiveInterval **active, int active_count, LiveInterval *li) {
  assert(active_count > 0);
  LiveInterval *spill = active[active_count - 1];
  if (spill->end > li->end) {
    li->rreg = spill->rreg;
    spill->rreg = SPILLED_REG_NO;
    spill->spill = true;
    insert_active(active, active_count - 1, li);
  } else {
    li->rreg = SPILLED_REG_NO;
    li->spill = true;
  }
}

static void expire_old_intervals(LiveInterval **active, int *pactive_count, short *pusing_bits, int start) {
  int active_count = *pactive_count;
  int j;
  short using_bits = *pusing_bits;
  for (j = 0; j < active_count; ++j) {
    LiveInterval *li = active[j];
    if (li->end > start)
      break;
    using_bits &= ~((short)1 << li->rreg);
  }
  remove_active(active, active_count, 0, j);
  *pactive_count = active_count - j;
  *pusing_bits = using_bits;
}

static LiveInterval **check_live_interval(BBContainer *bbcon, int vreg_count, LiveInterval **pintervals) {
  LiveInterval *intervals = malloc(sizeof(LiveInterval) * vreg_count);
  for (int i = 0; i < vreg_count; ++i) {
    LiveInterval *li = &intervals[i];
    li->vreg = i;
    li->rreg = -1;
    li->start = li->end = -1;
    li->spill = false;
  }

  int nip = 0;
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    for (int j = 0; j < bb->irs->len; ++j, ++nip) {
      IR *ir = bb->irs->data[j];
      VReg *regs[] = {ir->dst, ir->opr1, ir->opr2};
      for (int k = 0; k < 3; ++k) {
        VReg *reg = regs[k];
        if (reg == NULL)
          continue;
        LiveInterval *li = &intervals[reg->v];
        if (li->start < 0)
          li->start = nip;
        if (li->end < nip)
          li->end = nip;
      }
    }

    for (int j = 0; j < bb->out_regs->len; ++j) {
      VReg *reg = bb->out_regs->data[j];
      LiveInterval *li = &intervals[reg->v];
      if (li->start < 0)
        li->start = nip;
      if (li->end < nip)
        li->end = nip;
    }
  }

  // Sort by start, end
  LiveInterval **sorted_intervals = malloc(sizeof(LiveInterval*) * vreg_count);
  for (int i = 0; i < vreg_count; ++i)
    sorted_intervals[i] = &intervals[i];
  myqsort(sorted_intervals, vreg_count, sizeof(LiveInterval*), (int (*)(const void*, const void*))sort_live_interval);

  *pintervals = intervals;
  return sorted_intervals;
}

static short linear_scan_register_allocation(LiveInterval **sorted_intervals, int vreg_count) {
  LiveInterval *active[REG_COUNT];
  int active_count = 0;
  short using_bits = 0;
  short used_bits = 0;

  for (int i = 0; i < vreg_count; ++i) {
    LiveInterval *li = sorted_intervals[i];
    if (li->spill)
      continue;
    expire_old_intervals(active, &active_count, &using_bits, li->start);
    if (active_count >= REG_COUNT) {
      split_at_interval(active, active_count, li);
    } else {
      int regno = -1;
      for (int j = 0; j < REG_COUNT; ++j) {
        if (!(using_bits & (1 << j))) {
          regno = j;
          break;
        }
      }
      assert(regno >= 0);
      li->rreg = regno;
      using_bits |= 1 << regno;

      insert_active(active, active_count, li);
      ++active_count;
    }
    used_bits |= using_bits;
  }
  return used_bits;
}

static int insert_load_store_spilled(BBContainer *bbcon, Vector *vregs) {
  int inserted = 0;
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    Vector *irs = bb->irs;
    for (int j = 0; j < irs->len; ++j) {
      IR *ir = irs->data[j];

      switch (ir->kind) {
      case IR_IMM:
      case IR_BOFS:
      case IR_IOFS:
      case IR_MOV:
      case IR_ADD:  // binops
      case IR_SUB:
      case IR_MUL:
      case IR_DIV:
      case IR_MOD:
      case IR_BITAND:
      case IR_BITOR:
      case IR_BITXOR:
      case IR_LSHIFT:
      case IR_RSHIFT:
      case IR_CMP:
      case IR_NEG:  // unary ops
      case IR_NOT:
      case IR_BITNOT:
      case IR_SET:
      case IR_TEST:
      case IR_PUSHARG:
      case IR_CALL:
      case IR_CAST:
      case IR_RESULT:
        if (ir->opr1 != NULL && ir->opr1->r == SPILLED_REG_NO) {
          vec_insert(irs, j,
                     new_ir_load_spilled(((VReg*)vregs->data[ir->opr1->v])->offset, ir->size));
          ++j;
          inserted |= 1;
        }

        if (ir->opr2 != NULL && ir->opr2->r == SPILLED_REG_NO) {
          vec_insert(irs, j,
                     new_ir_load_spilled(((VReg*)vregs->data[ir->opr2->v])->offset, ir->size));
          ++j;
          inserted |= 2;
        }

        if (ir->dst != NULL && ir->dst->r == SPILLED_REG_NO) {
          ++j;
          vec_insert(irs, j,
                     new_ir_store_spilled(((VReg*)vregs->data[ir->dst->v])->offset, ir->size));
          inserted |= 4;
        }
        break;

      case IR_LOAD:
      case IR_STORE:
      case IR_MEMCPY:
        if (ir->opr1 != NULL && ir->opr1->r == SPILLED_REG_NO) {
          vec_insert(irs, j,
                     new_ir_load_spilled(((VReg*)vregs->data[ir->opr1->v])->offset, WORD_SIZE));
          ++j;
          inserted |= 1;
        }

        if (ir->opr2 != NULL && ir->opr2->r == SPILLED_REG_NO) {
          vec_insert(irs, j,
                     new_ir_load_spilled(((VReg*)vregs->data[ir->opr2->v])->offset, WORD_SIZE));
          ++j;
          inserted |= 2;
        }

        if (ir->dst != NULL && ir->dst->r == SPILLED_REG_NO) {
          ++j;
          vec_insert(irs, j,
                     new_ir_store_spilled(((VReg*)vregs->data[ir->dst->v])->offset, ir->size));
          inserted |= 4;
        }
        break;

      default:
        break;
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
        if (reg == NULL)
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

size_t alloc_real_registers(Function *func) {
  BBContainer *bbcon = func->bbcon;
  for (int i = 0; i < bbcon->bbs->len; ++i) {
    BB *bb = bbcon->bbs->data[i];
    three_to_two(bb);
  }

  analyze_reg_flow(bbcon);

  int vreg_count = func->ra->regno;
  LiveInterval *intervals;
  LiveInterval **sorted_intervals = check_live_interval(bbcon, vreg_count, &intervals);

  for (int i = 0; i < vreg_count; ++i) {
    LiveInterval *li = &intervals[i];
    VReg *vreg = func->ra->vregs->data[i];
    if (vreg->r == SPILLED_REG_NO) {
      li->spill = true;
      li->rreg = vreg->r;
    }
  }

  if (func->params != NULL) {
    // Make function parameters all spilled.
    for (int i = 0; i < func->params->len; ++i) {
      VarInfo *varinfo = func->params->data[i];

      LiveInterval *li = &intervals[varinfo->reg->v];
      li->start = 0;
      li->spill = true;
      li->rreg = SPILLED_REG_NO;
    }
  }

  func->used_reg_bits = linear_scan_register_allocation(sorted_intervals, vreg_count);

  // Map vreg to rreg.
  for (int i = 0; i < vreg_count; ++i) {
    VReg *vreg = func->ra->vregs->data[i];
    vreg->r = intervals[vreg->v].rreg;
  }

  // Allocated spilled virtual registers onto stack.
  size_t frame_size = 0;
  for (int i = 0; i < vreg_count; ++i) {
    LiveInterval *li = sorted_intervals[i];
    if (!li->spill)
      continue;
    VReg *vreg = func->ra->vregs->data[li->vreg];
    if (vreg->offset != 0) {  // Variadic function parameter or stack parameter.
      if (-vreg->offset > (int)frame_size)
        frame_size = -vreg->offset;
      continue;
    }

    int size, align;
    const Type *type = vreg->type;
    assert(type != NULL);
    size = type_size(type);
    align = align_size(type);
    if (size < 1)
      size = 1;

    frame_size = ALIGN(frame_size + size, align);
    vreg->offset = -frame_size;
  }

  int inserted = insert_load_store_spilled(bbcon, func->ra->vregs);
  if (inserted != 0)
    func->used_reg_bits |= 1 << SPILLED_REG_NO;

  func->ra->sorted_intervals = sorted_intervals;

  return ALIGN(frame_size, 16);
}
#include "emit.h"

#include <assert.h>
#include <inttypes.h>  // PRIdPTR
#include <stdarg.h>
#include <stdint.h>  // intptr_t

#include "x86_64.h"

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

char *num(intptr_t x) {
  return fmt("%"PRIdPTR, x);
}

char *im(intptr_t x) {
  return fmt("$%"PRIdPTR, x);
}

char *indirect(const char *reg) {
  return fmt("(%s)", reg);
}

char *offset_indirect(int offset, const char *reg) {
  return fmt("%d(%s)", offset, reg);
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
  if (align <= 0)
    return;

#ifdef __APPLE__
  // On Apple platform,
  // .align directive is actually .p2align,
  // so it has to find power of 2.
#define IS_POWER_OF_2(x)  (x > 0 && (x & (x - 1)) == 0)
  assert(IS_POWER_OF_2(align));
  int bit, x = align;
  for (bit = 0;; ++bit) {
    x >>= 1;
    if (x <= 0)
      break;
  }
  _P2ALIGN(NUM(bit));
#else
  _ALIGN(NUM(align));
#endif
}

void init_emit(FILE *fp) {
  emit_fp = fp;
}
#include <assert.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "codegen.h"
#include "emit.h"
#include "lexer.h"
#include "parser.h"
#include "sema.h"
#include "type.h"
#include "util.h"
#include "var.h"

#include "ir.h"

////////////////////////////////////////////////

#if !defined(SELF_HOSTING)
static void do_dump_ir(Node *node) {
  switch (node->kind) {
  case ND_TOPLEVEL:
    for (int i = 0, len = node->toplevel.nodes->len; i < len; ++i) {
      Node *child = node->toplevel.nodes->data[i];
      if (child == NULL)
        continue;
      do_dump_ir(child);
    }
    break;
  case ND_DEFUN:
    dump_func_ir(node->defun->func);
    break;
  case ND_VARDECL:
    break;

  default:
    assert(false);
    break;
  }
}
#endif

////////////////////////////////////////////////

static void init_compiler(FILE *ofp) {
  init_emit(ofp);
  enum_map = new_map();
  enum_value_map = new_map();
  struct_map = new_map();
  typedef_map = new_map();
  gvar_map = new_map();
}

static Vector *compile1(FILE *ifp, const char *filename, Vector *nodes) {
  init_lexer(ifp, filename);
  return parse_program(nodes);
}

static Node *compile2(Vector *nodes) {
  Node *top = sema(new_top_node(nodes));
  gen(top);
  return top;
}

static const char LOCAL_LABEL_PREFIX[] = "--local-label-prefix=";

int main(int argc, char* argv[]) {
  int iarg;
  bool dump_ir = false;

  for (iarg = 1; iarg < argc; ++iarg) {
    if (*argv[iarg] != '-')
      break;
    if (strncmp(argv[iarg], LOCAL_LABEL_PREFIX, sizeof(LOCAL_LABEL_PREFIX) - 1) == 0) {
      set_local_label_prefix(&argv[iarg][sizeof(LOCAL_LABEL_PREFIX) - 1]);
    }
    if (strcmp(argv[iarg], "--dump-ir") == 0)
      dump_ir = true;
  }

  // Compile.
  init_compiler(stdout);

  Vector *nodes = NULL;
  if (iarg < argc) {
    for (int i = iarg; i < argc; ++i) {
      const char *filename = argv[i];
      FILE *ifp = fopen(filename, "r");
      if (ifp == NULL)
        error("Cannot open file: %s\n", filename);
      nodes = compile1(ifp, filename, nodes);
    }
  } else {
    nodes = compile1(stdin, "*stdin*", nodes);
  }
  Node *root = compile2(nodes);

  if (!dump_ir) {
    emit_code(root);
  } else {
#if !defined(SELF_HOSTING)
    do_dump_ir(root);
#endif
  }

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
