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

char *append(char *str, const char *begin, const char *end) {
  size_t add;
  if (end == NULL) {
    add = strlen(begin);
    end = begin + add;
  } else {
    add = end - begin;
  }
  if (add == 0)
    return str;
  size_t len = str != NULL ? strlen(str) : 0;
  char *newstr = realloc(str, len + add + 1);
  memcpy(newstr + len, begin, add);
  newstr[len + add] = '\0';
  return newstr;
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
  } u;
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
  seg->u.text = text;
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
  char *text = NULL;
  for (;;) {
    Token *tok;
    if ((tok = consume(TK_IDENT)) != NULL) {
      int index = -1;
      if (va_args && strcmp(tok->u.ident, "__VA_ARGS__") == 0) {
        index = param_len;
      } else {
        for (int i = 0; i < param_len; ++i) {
          if (strcmp(tok->u.ident, params->data[i]) == 0) {
            index = i;
            break;
          }
        }
      }
      if (index >= 0) {
        if (text != NULL) {
          Segment *seg = malloc(sizeof(*seg));
          seg->type = ST_TEXT;
          seg->u.text = text;
          vec_push(segments, seg);
        }

        Segment *seg2 = malloc(sizeof(*seg2));
        seg2->type = ST_PARAM;
        seg2->u.param = index;
        vec_push(segments, seg2);

        text = NULL;
        continue;
      }
    } else {
      tok = consume(-1);
      if (tok->type == TK_EOF)
        break;
    }
    text = append(text, tok->begin, tok->end);
    text = append(text, " ", NULL);
  }

  if (text != NULL) {
    Segment *seg = malloc(sizeof(*seg));
    seg->type = ST_TEXT;
    seg->u.text = text;
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
          vec_push(params, tok->u.ident);
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

Token *consume2(enum TokenType type) {
  Token *tok;
  for (;;) {
    tok = consume(type);
    if (tok == NULL || tok->type != TK_EOF)
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
    if (!consume2(TK_RPAR)) {
      int paren = 0;
      const char *begin = NULL;
      for (;;) {
        if (consume2(TK_EOF))
          parse_error(NULL, "`)' expected");

        Token *tok;
        if ((tok = consume2(TK_COMMA)) != NULL || (tok = consume2(TK_RPAR)) != NULL)  {
          if (paren > 0) {
            if (tok->type == TK_RPAR)
              --paren;
            continue;
          }
          if (begin == NULL)
            parse_error(tok, "expression expected");
          vec_push(args, strndup_(begin, tok->begin - begin));
          begin = NULL;
          if (tok->type == TK_RPAR)
            break;
          continue;
        }
        tok = consume2(-1);
        if (begin == NULL)
          begin = tok->begin;
        if (tok->type == TK_LPAR)
          ++paren;
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
    char *vaargs = NULL;
    for (int i = macro->params->len; i < args->len; ++i) {
      if (i > macro->params->len)
        vaargs = append(vaargs, ",", NULL);
      vaargs = append(vaargs, (char*)args->data[i], NULL);
    }
    if (args->len <= macro->params->len)
      vec_push(args, vaargs);
    else
      args->data[macro->params->len] = vaargs;
  }

  char *str = NULL;
  if (macro->segments != NULL) {
    str = NULL;
    for (int i = 0; i < macro->segments->len; ++i) {
      Segment *seg = macro->segments->data[i];
      switch (seg->type) {
      case ST_TEXT:
        str = append(str, seg->u.text, NULL);
        break;
      case ST_PARAM:
        str = append(str, (char*)args->data[seg->u.param], NULL);
        break;
      default:
        break;
      }
    }
  }
  str = append(str, get_lex_p(), NULL);

  init_lexer_string(str, NULL, -1);
}

void process_line(const char *line, Stream *stream) {
  init_lexer_string(line, stream->filename, stream->lineno);

  const char *begin = get_lex_p();
  for (;;) {
    if (consume(TK_EOF))
      break;

    Token *ident = consume(TK_IDENT);
    Macro *macro;
    if (ident != NULL && (macro = map_get(macro_map, ident->u.ident)) != NULL) {
      if (ident->begin != begin)
        fwrite(begin, ident->begin - begin, 1, stdout);

      s_stream = stream;
      expand(macro, ident->u.ident);
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
  switch (expr->type) {
  case EX_NUM:
    switch (expr->valType->u.num.type) {
    case NUM_CHAR:
    case NUM_SHORT:
    case NUM_INT:
    case NUM_LONG:
      return expr->u.num.ival;
    default: assert(false); break;
    }
    break;
  case EX_FUNCALL:
    {
      const Expr *func = expr->u.funcall.func;
      const Vector *args = expr->u.funcall.args;
      if (func->type == EX_VARREF &&
          strcmp(func->u.varref.ident, "defined") == 0 &&
          args != NULL && args->len == 1 &&
          ((Expr*)args->data[0])->type == EX_VARREF) {  // defined(IDENT)
        Expr *arg = (Expr*)args->data[0];
        void *dummy = 0;
        return map_try_get(macro_map, arg->u.varref.ident, &dummy) ? 1 : 0;
      }
    }
    break;
  case EX_NOT:
    return reduce(expr->u.unary.sub) ? 0 : 1;
  case EX_LOGAND:
    return reduce(expr->u.bop.lhs) && reduce(expr->u.bop.rhs);
  case EX_LOGIOR:
    return reduce(expr->u.bop.lhs) || reduce(expr->u.bop.rhs);
  default:
    break;
  }
  error("expression not handled in preprocessor: type=%d", expr->type);
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

  switch (expr->type) {
  case EX_NUM:
  case EX_STR:
    return true;
  default:
    return false;
  }
}

void not_void(const Type *type) {
  if (type->type == TY_VOID)
    parse_error(NULL, "`void' not allowed");
}

enum ExprType flip_cmp(enum ExprType type) {
  assert(EX_EQ <= type && type <= EX_GT);
  if (type >= EX_LT)
    type = EX_GT - (type - EX_LT);
  return type;
}

//

static Expr *new_expr(enum ExprType type, const Type *valType, const Token *token) {
  Expr *expr = malloc(sizeof(*expr));
  expr->type = type;
  expr->valType = valType;
  expr->token = token;
  return expr;
}

Expr *new_expr_numlit(const Type *type, const Token *token, const Num *num) {
  assert(type->type == TY_NUM);
  Expr *expr = new_expr(EX_NUM, type, token);
  expr->u.num = *num;
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

Expr *new_expr_varref(const char *name, const Type *type, const Token *token) {
  Expr *expr = new_expr(EX_VARREF, type, token);
  expr->u.varref.ident = name;
  expr->u.varref.scope = NULL;
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

Expr *new_expr_sizeof(const Token *token, const Type *type, Expr *sub) {
  Expr *expr = new_expr(EX_SIZEOF, &tySize, token);
  expr->u.sizeof_.type = type;
  expr->u.sizeof_.sub = sub;
  return expr;
}

Expr *new_expr_cast(const Type *type, const Token *token, Expr *sub) {
  Expr *expr = new_expr(EX_CAST, type, token);
  expr->u.unary.sub = sub;
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
      type = find_typedef(ident->u.ident);
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
    return new_expr_varref(name, /*type*/NULL, ident);
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
      expr->u.unary.sub = sub;
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

bool is_im8(intptr_t x) {
  return x <= ((1L << 7) - 1) && x >= -(1L << 7);
}

bool is_im32(intptr_t x) {
  return x <= ((1L << 31) - 1) && x >= -(1L << 31);
}

// Container

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
  int d = vec->len - index;
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

Expr *make_cast(const Type *type, const Token *token, Expr *sub, bool is_explicit) {
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

  return new_expr_cast(type, token, sub);
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
    rhs = make_cast(type, rhs->token, rhs, false);
  } else {
    type = tyNumTable[rnt];
    lhs = make_cast(type, lhs->token, lhs, false);
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
                                   make_cast(&tySize, token, num, false),
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
      *pRhs = make_cast((*pLhs)->valType, (*pRhs)->token, *pRhs, false);
    else if (ltype < rtype)
      *pLhs = make_cast((*pRhs)->valType, (*pLhs)->token, *pLhs, false);
  }
  return true;
}

static bool search_from_anonymous(const Type *type, const Token *ident, Vector *stack) {
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
      vec_push(stack, (void*)(intptr_t)i);
      bool res = search_from_anonymous(info->type, ident, stack);
      if (res)
        return true;
      vec_pop(stack);
    }
  }
  return false;
}

static Expr *analyze_cmp(Expr *expr) {
  Expr *lhs = expr->u.bop.lhs, *rhs = expr->u.bop.rhs;
  if (lhs->valType->type == TY_PTR || rhs->valType->type == TY_PTR) {
    if (lhs->valType->type != TY_PTR) {
      Expr *tmp = lhs;
      lhs = rhs;
      rhs = tmp;
      expr->u.bop.lhs = lhs;
      expr->u.bop.rhs = rhs;
      expr->type = flip_cmp(expr->type);
    }
    const Type *lt = lhs->valType, *rt = rhs->valType;
    if (!can_cast(lt, rt, rhs, false))
      parse_error(expr->token, "Cannot compare pointer to other types");
    if (rt->type != TY_PTR)
      expr->u.bop.rhs = make_cast(lhs->valType, expr->token, rhs, false);
  } else {
    if (!cast_numbers(&expr->u.bop.lhs, &expr->u.bop.rhs, false))
      parse_error(expr->token, "Cannot compare except numbers");
    // cast_numbers might change lhs and rhs, so need to be updated.
    lhs = expr->u.bop.lhs;
    rhs = expr->u.bop.rhs;

    if (is_const(lhs) && !is_const(rhs)) {
      Expr *tmp = lhs;
      lhs = rhs;
      rhs = tmp;
      expr->u.bop.lhs = lhs;
      expr->u.bop.rhs = rhs;
      expr->type = flip_cmp(expr->type);
    }
  }
  return expr;
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
      Scope *scope = NULL;
      if (curscope != NULL) {
        scope = curscope;
        VarInfo *varinfo = scope_find(&scope, name);
        if (varinfo != NULL) {
          if (varinfo->flag & VF_STATIC) {
            // Replace local variable reference to global.
            name = varinfo->u.l.label;
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
      expr->valType = type;
      expr->u.varref.scope = scope;
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
      expr = analyze_cmp(expr);
      break;

    case EX_LOGAND:
    case EX_LOGIOR:
      break;

    case EX_ASSIGN:
      expr->valType = expr->u.bop.lhs->valType;
      expr->u.bop.rhs = make_cast(expr->valType, expr->token, expr->u.bop.rhs, false);
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
      if (is_const(expr->u.unary.sub)) {
        Expr *sub = expr->u.unary.sub;
        sub->u.num.ival = -sub->u.num.ival;
        return sub;
      }
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
        bool res = search_from_anonymous(targetType, ident, stack);
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
            args->data[i] = make_cast(type, arg->token, arg, false);
          } else if (vaargs && i >= paramc) {
            Expr *arg = args->data[i];
            const Type *type = arg->valType;
            if (type->type == TY_NUM && type->u.num.type < NUM_INT)  // Promote variadic argument.
              args->data[i] = make_cast(&tyInt, arg->token, arg, false);
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

  assert(expr->valType != NULL);
  return expr;
}
