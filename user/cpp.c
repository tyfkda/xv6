#include "assert.h"
#include "ctype.h"
#include "libgen.h"  // dirname
#include "stdarg.h"
#include "stdbool.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "unistd.h"

#include "expr.h"
#include "lexer.h"
#include "type.h"
#include "util.h"

char *abspath_cwd(const char *dir, const char *path) {
  char *cwd = getcwd(NULL, 0);
  char *root = abspath(cwd, dir);
  free(cwd);
  return abspath(root, path);
}

enum SegmentType {
  ST_TEXT,
  ST_PARAM,
};

typedef struct {
  enum SegmentType type;
  union {
    const char *text;
    int param;
  };
} Segment;

typedef struct {
  Vector *params;  // <const char*>
  bool va_args;
  Vector *segments;  // <Segment*>
} Macro;

Macro *new_macro(Vector *params, bool va_args, Vector *segments) {
  Macro *macro = malloc(sizeof(*macro));
  macro->params = params;
  macro->va_args = va_args;
  macro->segments = segments;
  return macro;
}

Macro *new_macro_single(const char *text) {
  Vector *segments = new_vector();
  Segment *seg = malloc(sizeof(*seg));
  seg->type = ST_TEXT;
  seg->text = text;
  vec_push(segments, seg);
  return new_macro(NULL, false, segments);
}

const char *skip_whitespaces(const char *s) {
  while (isspace(*s))
    ++s;
  return s;
}

const char *keyword(const char *s, const char *word) {
  size_t len = strlen(word);
  if (strncmp(s, word, len) != 0 || (s[len] != '\0' && !isspace(s[len])))
    return NULL;
  return skip_whitespaces(s + (len + 1));
}

const char *find_directive(const char *line) {
  const char *p = skip_whitespaces(line);
  if (*p != '#')
    return NULL;
  return skip_whitespaces(p + 1);
}

Map *macro_map;  // <Macro*>

Vector *sys_inc_paths;  // <const char*>
Vector *pragma_once_files;  // <const char*>

typedef struct {
  const char *filename;
  FILE *fp;
  int lineno;
} Stream;

static Stream *s_stream;

bool registered_pragma_once(const char *filename) {
  for (int i = 0, len = pragma_once_files->len; i < len; ++i) {
    const char *fn = pragma_once_files->data[i];
    if (strcmp(fn, filename) == 0)
      return true;
  }
  return false;
}

void register_pragma_once(const char *filename) {
  vec_push(pragma_once_files, filename);
}

int pp(FILE *fp, const char *filename);

void handle_include(const char *p, const char *srcname) {
  char close;
  bool sys = false;
  switch (*p++) {
  case '"':
    close = '"';
    break;
  case '<':
    close = '>';
    sys = true;
    break;
  default:
    error("syntax error");
    return;
  }

  const char *q;
  for (q = p; *q != close; ++q) {
    if (*q == '\0')
      error("not closed");
  }

  char *path = strndup_(p, q - p);
  char *fn = NULL;
  FILE *fp = NULL;
  // Search from current directory.
  if (!sys) {
    fn = abspath_cwd(dirname(strdup_(srcname)), path);
    fp = fopen(fn, "r");
  }
  if (fp == NULL) {
    // Search from system include directries.
    for (int i = 0; i < sys_inc_paths->len; ++i) {
      fn = abspath_cwd(sys_inc_paths->data[i], path);
      fp = fopen(fn, "r");
      if (fp != NULL)
        break;
    }
    if (fp == NULL) {
      error("Cannot open file: %s", path);
      return;
    }
  }

  if (registered_pragma_once(fn))
    return;

  printf("# 1 \"%s\" 1\n", fn);
  int lineno = pp(fp, fn);
  printf("# %d \"%s\" 2\n", lineno, fn);
  fclose(fp);
}

void handle_pragma(const char *p, const char *filename) {
  char *name = read_ident(&p);
  if (strcmp(name, "once") == 0) {
    if (!registered_pragma_once(filename))
      register_pragma_once(filename);
  } else {
    fprintf(stderr, "Warning: unhandled #pragma: %s\n", p);
  }
}

Vector *parse_macro_body(const char *p, const Vector *params, bool va_args, Stream *stream) {
  Vector *segments = new_vector();
  init_lexer_string(p, stream->filename, stream->lineno);
  int param_len = params != NULL ? params->len : 0;
  StringBuffer sb;
  sb_init(&sb);
  for (;;) {
    Token *tok;
    if ((tok = consume(TK_IDENT)) != NULL) {
      int index = -1;
      if (va_args && strcmp(tok->ident, "__VA_ARGS__") == 0) {
        index = param_len;
      } else {
        for (int i = 0; i < param_len; ++i) {
          if (strcmp(tok->ident, params->data[i]) == 0) {
            index = i;
            break;
          }
        }
      }
      if (index >= 0) {
        if (!sb_empty(&sb)) {
          Segment *seg = malloc(sizeof(*seg));
          seg->type = ST_TEXT;
          seg->text = sb_to_string(&sb);
          vec_push(segments, seg);
          sb_clear(&sb);
        }

        Segment *seg2 = malloc(sizeof(*seg2));
        seg2->type = ST_PARAM;
        seg2->param = index;
        vec_push(segments, seg2);

        continue;
      }
    } else {
      tok = consume(-1);
      if (tok->kind == TK_EOF)
        break;
    }

    if (!sb_empty(&sb))
      sb_append(&sb, " ", NULL);
    sb_append(&sb, tok->begin, tok->end);
  }

  if (!sb_empty(&sb)) {
    Segment *seg = malloc(sizeof(*seg));
    seg->type = ST_TEXT;
    seg->text = sb_to_string(&sb);
    vec_push(segments, seg);
  }
  return segments;
}

void handle_define(const char *p, Stream *stream) {
  char *name = read_ident(&p);
  if (name == NULL)
    error("`ident' expected");

  Vector *params = NULL;
  bool va_args = false;
  if (*p == '(') {
    // Macro with parameter.
    params = new_vector();
    init_lexer_string(p + 1, stream->filename, stream->lineno);
    if (!consume(TK_RPAR)) {
      for (;;) {
        Token *tok;
        if ((tok = consume(TK_DOTDOTDOT)) != NULL) {
          va_args = true;
          if (!consume(TK_RPAR))
            parse_error(NULL, "`)' expected");
          break;
        } else if ((tok = consume(TK_IDENT)) != NULL) {
          vec_push(params, tok->ident);
          if (consume(TK_RPAR))
            break;
          if (!consume(TK_COMMA))
            parse_error(NULL, "`,' or `)' expected");
        } else {
          parse_error(NULL, "`ident' expected");
        }
      }
    }
    p = get_lex_p();
  }

  Vector *segments = NULL;
  p = skip_whitespaces(p);
  if (*p != '\0') {
    segments = parse_macro_body(skip_whitespaces(p), params, va_args, stream);
  }
  map_put(macro_map, name, new_macro(params, va_args, segments));
}

void handle_undef(const char *p) {
  char *name = read_ident(&p);
  if (name == NULL)
    error("`ident' expected");

  map_remove(macro_map, name);
}

Token *consume2(enum TokenKind kind) {
  Token *tok;
  for (;;) {
    tok = consume(kind);
    if (tok == NULL || tok->kind != TK_EOF)
      return tok;

    char *line = NULL;
    size_t capa = 0;
    ssize_t len = getline_(&line, &capa, s_stream->fp, 0);
    if (len == EOF)
      return tok;  // EOF
    ++s_stream->lineno;
    init_lexer_string(line, s_stream->filename, s_stream->lineno);
  }
}

void expand(Macro *macro, const char *name) {
  Vector *args = NULL;
  if (macro->params != NULL) {
    if (!consume2(TK_LPAR))
      parse_error(NULL, "`(' expected for macro `%s'", name);
    args = new_vector();
    StringBuffer sb;
    sb_init(&sb);
    if (!consume2(TK_RPAR)) {
      int paren = 0;
      for (;;) {
        if (consume2(TK_EOF))
          parse_error(NULL, "`)' expected");

        Token *tok;
        if ((tok = consume2(TK_COMMA)) != NULL || (tok = consume2(TK_RPAR)) != NULL)  {
          if (paren > 0) {
            sb_append(&sb, tok->begin, tok->end);
            if (tok->kind == TK_RPAR)
              --paren;
            continue;
          }
          if (sb_empty(&sb))
            parse_error(tok, "expression expected");

          vec_push(args, sb_to_string(&sb));
          sb_clear(&sb);

          if (tok->kind == TK_RPAR)
            break;
          continue;
        }
        tok = consume2(-1);
        if (tok->kind == TK_LPAR)
          ++paren;
        if (!sb_empty(&sb))
          sb_append(&sb, " ", NULL);
        sb_append(&sb, tok->begin, tok->end);
      }
    }

    if ((!macro->va_args && args->len != macro->params->len) ||
        (macro->va_args && args->len <= macro->params->len)) {
      const char *cmp = args->len < macro->params->len ? "less" : "few";
      parse_error(NULL, "Too %s arguments for macro `%s'", cmp, name);
    }
  }

  // __VA_ARGS__
  if (macro->va_args) {
    // Concat.
    StringBuffer sb;
    sb_init(&sb);
    int plen = macro->params->len;
    for (int i = plen; i < args->len; ++i) {
      if (i > plen)
        sb_append(&sb, ",", NULL);
      sb_append(&sb, (char*)args->data[i], NULL);
    }
    char *vaargs = sb_to_string(&sb);

    if (args->len <= plen)
      vec_push(args, vaargs);
    else
      args->data[plen] = vaargs;
  }

  StringBuffer sb;
  sb_init(&sb);
  if (macro->segments != NULL) {
    for (int i = 0; i < macro->segments->len; ++i) {
      Segment *seg = macro->segments->data[i];
      switch (seg->type) {
      case ST_TEXT:
        sb_append(&sb, seg->text, NULL);
        break;
      case ST_PARAM:
        sb_append(&sb, (char*)args->data[seg->param], NULL);
        break;
      default:
        break;
      }
    }
  }
  sb_append(&sb, get_lex_p(), NULL);

  init_lexer_string(sb_to_string(&sb), NULL, -1);
}

bool handle_block_comment(const char *begin, const char **pp, Stream *stream) {
  const char *p = skip_whitespaces(*pp);
  if (*p != '/' || p[1] != '*')
    return false;

  p += 2;
  for (;;) {
    if (*p == '\0') {
      fwrite(begin, p - begin, 1, stdout);
      fputc('\n', stdout);

      char *line = NULL;
      size_t capa = 0;
      ssize_t len = getline_(&line, &capa, stream->fp, 0);
      if (len == EOF) {
        *pp = p;
        return true;
      }
      ++stream->lineno;
      begin = p = line;
      continue;
    }

    if (*p == '*' && p[1] == '/') {
      p += 2;
      fwrite(begin, p - begin, 1, stdout);
      *pp = p;
      return true;
    }

    ++p;
  }
}

void process_line(const char *line, Stream *stream) {
  init_lexer_string(line, stream->filename, stream->lineno);

  const char *begin = get_lex_p();
  for (;;) {
    const char *p = get_lex_p();
    if (p != NULL) {
      if (handle_block_comment(begin, &p, stream)) {
        begin = p;
        init_lexer_string(begin, stream->filename, stream->lineno);
      }
    }

    if (consume(TK_EOF))
      break;

    Token *ident = consume(TK_IDENT);
    Macro *macro;
    if (ident != NULL && (macro = map_get(macro_map, ident->ident)) != NULL) {
      if (ident->begin != begin)
        fwrite(begin, ident->begin - begin, 1, stdout);

      s_stream = stream;
      expand(macro, ident->ident);
      begin = get_lex_p();
      continue;
    }

    consume(-1);
  }

  printf("%s\n", begin);
}

bool handle_ifdef(const char *p) {
  char *name = read_ident(&p);
  if (name == NULL)
    error("`ident' expected");
  return map_get(macro_map, name) != NULL;
}

intptr_t reduce(Expr *expr) {
  switch (expr->kind) {
  case EX_NUM:
    switch (expr->type->num.kind) {
    case NUM_CHAR:
    case NUM_SHORT:
    case NUM_INT:
    case NUM_LONG:
      return expr->num.ival;
    default: assert(false); break;
    }
    break;
  case EX_FUNCALL:
    {
      const Expr *func = expr->funcall.func;
      const Vector *args = expr->funcall.args;
      if (func->kind == EX_VARREF &&
          strcmp(func->varref.ident, "defined") == 0 &&
          args != NULL && args->len == 1 &&
          ((Expr*)args->data[0])->kind == EX_VARREF) {  // defined(IDENT)
        Expr *arg = (Expr*)args->data[0];
        void *dummy = 0;
        return map_try_get(macro_map, arg->varref.ident, &dummy) ? 1 : 0;
      }
    }
    break;
  case EX_NOT:
    return reduce(expr->unary.sub) ? 0 : 1;
  case EX_LOGAND:
    return reduce(expr->bop.lhs) && reduce(expr->bop.rhs);
  case EX_LOGIOR:
    return reduce(expr->bop.lhs) || reduce(expr->bop.rhs);
  default:
    break;
  }
  error("expression not handled in preprocessor: type=%d", expr->kind);
  return 0;
}

bool handle_if(const char *p, Stream *stream) {
  init_lexer_string(p, stream->filename, stream->lineno);
  Expr *expr = parse_expr();
  return reduce(expr) != 0;
}

#define CF_ENABLE         (1 << 0)
#define CF_SATISFY_SHIFT  (1)
#define CF_SATISFY_MASK   (3 << CF_SATISFY_SHIFT)

intptr_t cond_value(bool enable, int satisfy) {
  return (enable ? CF_ENABLE : 0) | (satisfy << CF_SATISFY_SHIFT);
}

static void define_file_macro(const char *filename) {
  size_t len = strlen(filename);
  char *buf = malloc(len + 2 + 1);
  snprintf(buf, len + 2 + 1, "\"%s\"", filename);
  map_put(macro_map, "__FILE__", new_macro_single(buf));
}

int pp(FILE *fp, const char *filename) {
  Vector *condstack = new_vector();
  bool enable = true;
  int satisfy = 0;  // #if condition: 0=not satisfied, 1=satisfied, 2=else
  //char linenobuf[sizeof(int) * 3 + 1];  // Buffer for __LINE__
  char linenobuf[32];  // Buffer for __LINE__

  Macro *old_file_macro = map_get(macro_map, "__FILE__");
  Macro *old_line_macro = map_get(macro_map, "__LINE__");

  define_file_macro(filename);
  map_put(macro_map, "__LINE__", new_macro_single(linenobuf));

  Stream stream;
  stream.filename = filename;
  stream.fp = fp;

  for (stream.lineno = 1;; ++stream.lineno) {
    char *line = NULL;
    size_t capa = 0;
    ssize_t len = getline_(&line, &capa, fp, 0);
    if (len == EOF)
      break;

    snprintf(linenobuf, sizeof(linenobuf), "%d", stream.lineno);

    while (len > 0 && line[len - 1] == '\\') {  // Continue line.
      ++stream.lineno;
      len = getline_(&line, &capa, fp, len - 1);  // -1 for overwrite on '\'
    }

    // Find '#'
    const char *directive = find_directive(line);
    if (directive == NULL) {
      if (enable)
        process_line(line, &stream);
      else
        printf("\n");
      continue;
    }
    printf("\n");

    const char *next;
    if ((next = keyword(directive, "ifdef")) != NULL) {
      vec_push(condstack, (void*)cond_value(enable, satisfy));
      bool defined = handle_ifdef(next);
      satisfy = defined ? 1 : 0;
      enable = enable && satisfy == 1;
    } else if ((next = keyword(directive, "ifndef")) != NULL) {
      vec_push(condstack, (void*)cond_value(enable, satisfy));
      bool defined = handle_ifdef(next);
      satisfy = defined ? 0 : 1;
      enable = enable && satisfy == 1;
    } else if ((next = keyword(directive, "if")) != NULL) {
      vec_push(condstack, (void*)cond_value(enable, satisfy));
      bool cond = handle_if(next, &stream);
      satisfy = cond ? 1 : 0;
      enable = enable && satisfy == 1;
    } else if ((next = keyword(directive, "else")) != NULL) {
      int last = condstack->len - 1;
      if (last < 0)
        error("`#else' used without `#if'");
      intptr_t flag = (intptr_t)condstack->data[last];
      if (satisfy == 2)
        error("Illegal #else");
      enable = !enable && satisfy == 0 && ((flag & CF_ENABLE) != 0);
      satisfy = 2;
    } else if ((next = keyword(directive, "elif")) != NULL) {
      int last = condstack->len - 1;
      if (last < 0)
        error("`#elif' used without `#if'");
      intptr_t flag = (intptr_t)condstack->data[last];
      if (satisfy == 2)
        error("Illegal #elif");

      bool cond = false;
      if (satisfy == 0) {
        cond = handle_if(next, &stream);
        if (cond)
          satisfy = 1;
      }

      enable = !enable && cond && ((flag & CF_ENABLE) != 0);
    } else if ((next = keyword(directive, "endif")) != NULL) {
      int len = condstack->len;
      if (len <= 0)
        error("`#endif' used without `#if'");
      --len;
      int flag = (intptr_t)condstack->data[len];
      enable = (flag & CF_ENABLE) != 0;
      satisfy = (flag & CF_SATISFY_MASK) >> CF_SATISFY_SHIFT;
      condstack->len = len;
    } else if (enable) {
      if ((next = keyword(directive, "include")) != NULL) {
        handle_include(next, filename);
        printf("# %d \"%s\" 1\n", stream.lineno + 1, filename);
      } else if ((next = keyword(directive, "define")) != NULL) {
        handle_define(next, &stream);
      } else if ((next = keyword(directive, "undef")) != NULL) {
        handle_undef(next);
      } else if ((next = keyword(directive, "pragma")) != NULL) {
        handle_pragma(next, filename);
      } else if ((next = keyword(directive, "error")) != NULL) {
        error("#error: %s", next);
      } else {
        error("unknown directive: %s", directive);
      }
    }
  }

  if (condstack->len > 0)
    error("#if not closed");

  map_put(macro_map, "__FILE__", old_file_macro);
  map_put(macro_map, "__LINE__", old_line_macro);

  return stream.lineno;
}

static void define_macro(const char *arg) {
  char *p = strchr(arg, '=');
  if (p == NULL) {
    map_put(macro_map, arg, new_macro(NULL, false, NULL));
  } else {
    char *name = strndup_(arg, p - arg);
    map_put(macro_map, name, new_macro_single(p + 1));
  }
}

int main(int argc, char* argv[]) {
  macro_map = new_map();
  sys_inc_paths = new_vector();
  pragma_once_files = new_vector();

  // Predefeined macros.
  map_put(macro_map, "__XCC", new_macro(NULL, false, NULL));
#if defined(__XV6)
  map_put(macro_map, "__XV6", new_macro(NULL, false, NULL));
#elif defined(__linux__)
  map_put(macro_map, "__linux__", new_macro(NULL, false, NULL));
#elif defined(__APPLE__)
  map_put(macro_map, "__APPLE__", new_macro(NULL, false, NULL));
#endif

  int i = 1;
  for (; i < argc; ++i) {
    if (*argv[i] != '-')
      break;
    if (strncmp(argv[i], "-I", 2) == 0) {
      vec_push(sys_inc_paths, strdup_(argv[i] + 2));
    }
    if (strncmp(argv[i], "-D", 2) == 0)
      define_macro(argv[i] + 2);
  }

  if (i < argc) {
    for (; i < argc; ++i) {
      const char *filename = argv[i];
      FILE *fp = fopen(filename, "r");
      if (fp == NULL)
        error("Cannot open file: %s\n", filename);
      printf("# 1 \"%s\" 1\n", filename);
      pp(fp, filename);
      fclose(fp);
    }
  } else {
    pp(stdin, "*stdin*");
  }
  return 0;
}
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
