#define __NO_FLONUM
 #define __NO_ELF_OBJ

#include <string.h>

#include "preprocessor.h"
#include "util.h"

int main(int argc, char *argv[]) {
  FILE *ofp = stdout;
  init_preprocessor(ofp);

  // Predefeined macros.
  define_macro_simple("__XCC");
#if defined(__XV6)
  define_macro_simple("__XV6");
#elif defined(__linux__)
  define_macro_simple("__linux__");
#elif defined(__APPLE__)
  define_macro_simple("__APPLE__");
#endif
#if defined(__NO_FLONUM)
  define_macro_simple("__NO_FLONUM");
#endif

  int iarg = 1;
  for (; iarg < argc; ++iarg) {
    char *arg = argv[iarg];
    if (*arg != '-')
      break;

    if (starts_with(arg, "-I")) {
      add_system_inc_path(argv[iarg] + 2);
    } else if (starts_with(argv[iarg], "-D")) {
      define_macro(argv[iarg] + 2);
    } else if (strcmp(arg, "--version") == 0) {
      show_version("cpp");
      return 0;
    } else {
      fprintf(stderr, "Unknown option: %s\n", arg);
      return 1;
    }
  }

  if (iarg < argc) {
    for (int i = iarg; i < argc; ++i) {
      const char *filename = argv[i];
      FILE *fp = fopen(filename, "r");
      if (fp == NULL)
        error("Cannot open file: %s\n", filename);
      fprintf(ofp, "# 1 \"%s\" 1\n", filename);
      preprocess(fp, filename);
      fclose(fp);
    }
  } else {
    preprocess(stdin, "*stdin*");
  }
  return 0;
}
#include "macro.h"

#include <assert.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "lexer.h"  // parse_error
#include "table.h"
#include "util.h"

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
  seg->kind = SK_TEXT;
  seg->text = text;
  vec_push(segments, seg);
  return new_macro(NULL, false, segments);
}

bool expand(Macro *macro, const Token *token, Vector *args, const Name *name, StringBuffer *sb) {
  if (macro->params != NULL) {
    if (args == NULL)
      return false;

    if ((!macro->va_args && args->len != macro->params->len) ||
        (macro->va_args && args->len <= macro->params->len)) {
      const char *cmp = args->len < macro->params->len ? "few" : "many";
      parse_error(token, "Too %s arguments for macro `%.*s'", cmp, name->bytes, name->chars);
    }
  } else {
    if (args != NULL) {
      parse_error(token, "Illegal argument for macro `%.*s'", name->bytes, name->chars);
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

  if (macro->segments != NULL) {
    for (int i = 0; i < macro->segments->len; ++i) {
      Segment *seg = macro->segments->data[i];
      switch (seg->kind) {
      case SK_TEXT:
        sb_append(sb, seg->text, NULL);
        break;
      case SK_PARAM:
        sb_append(sb, (char*)args->data[seg->param], NULL);
        break;
      case SK_STRINGIFY:
        {
          static const char DQUOTE[] = "\"";
          sb_append(sb, DQUOTE, NULL);
          const char *text = args->data[seg->param];
          escape_string(text, strlen(text), sb);
          sb_append(sb, DQUOTE, NULL);
        }
        break;
      default:
        assert(false);
        break;
      }
    }
  }
  return true;
}
#include "pp_parser.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "lexer.h"
#include "macro.h"
#include "table.h"
#include "type.h"
#include "util.h"

extern Table macro_table;

static PpResult pp_prim(void);
static PpResult pp_cast_expr(void);

//

static PpResult expand_ident(const Token *ident) {
  Macro *macro = table_get(&macro_table, ident->ident);
  if (macro == NULL) {
    //parse_error(ident, "`%.s' not defined", ident->ident->bytes, ident->ident->chars);
    return 0;
  }

  Vector *args = NULL;
  if (macro->params != NULL)
    args = pp_funargs(NULL);

  StringBuffer sb;
  sb_init(&sb);
  expand(macro, ident, args, ident->ident, &sb);

  const char *left = get_lex_p();
  if (left != NULL)
    sb_append(&sb, left, NULL);
  char *expanded = sb_to_string(&sb);

  set_source_string(expanded, NULL, -1);

  return pp_prim();
}

static PpResult parse_defined(void) {
  bool lpar = match(TK_LPAR) != NULL;
  Token *ident = consume(TK_IDENT, "Ident expected");
  if (lpar)
    consume(TK_RPAR, "No close paren");

  void *dummy = 0;
  return table_try_get(&macro_table, ident->ident, &dummy) ? 1 : 0;
}

static PpResult pp_prim(void) {
  Token *tok;
  if ((tok = match(TK_LPAR)) != NULL) {
    PpResult result = pp_expr();
    consume(TK_RPAR, "No close paren");
    return result;
  }

  if ((tok = match(TK_CHARLIT)) != NULL ||
      (tok = match(TK_INTLIT)) != NULL ||
      (tok = match(TK_LONGLIT)) != NULL ||
      (tok = match(TK_LLONGLIT)) != NULL ||
      (tok = match(TK_UCHARLIT)) != NULL ||
      (tok = match(TK_UINTLIT)) != NULL ||
      (tok = match(TK_ULONGLIT)) != NULL ||
      (tok = match(TK_ULLONGLIT)) != NULL) {
    return tok->fixnum;
  }
  //if ((tok = match(TK_STR)) != NULL)
  //  return new_expr_str(tok, tok->str.buf, tok->str.size);

  Token *ident = consume(TK_IDENT, "Number or Ident or open paren expected");
  if (equal_name(ident->ident, alloc_name("defined", NULL, false))) {
    return parse_defined();
  } else {
    return expand_ident(ident);
  }
}

static PpResult pp_postfix(void) {
  PpResult result = pp_prim();

  //for (;;) {
    //Token *tok;
    //if (match(TK_LPAR))
    //  expr = parse_funcall(expr);
    //else if ((tok = match(TK_LBRACKET)) != NULL)
    //  expr = parse_array_index(tok, expr);
    //else if ((tok = match(TK_INC)) != NULL)
    //  expr = new_expr_unary(EX_POSTINC, NULL, tok, expr);
    //else if ((tok = match(TK_DEC)) != NULL)
    //  expr = new_expr_unary(EX_POSTDEC, NULL, tok, expr);
    //else
      return result;
  //}
}

static PpResult pp_unary(void) {
  Token *tok;
  if ((tok = match(TK_ADD)) != NULL) {
    return pp_cast_expr();
  }

  if ((tok = match(TK_SUB)) != NULL) {
    PpResult result = pp_cast_expr();
    return -result;
  }

  if ((tok = match(TK_NOT)) != NULL) {
    PpResult result = pp_cast_expr();
    return result ? 0 : 1;
  }

  if ((tok = match(TK_TILDA)) != NULL) {
    PpResult result = pp_cast_expr();
    return ~result;
  }

  //if ((tok = match(TK_AND)) != NULL) {
  //  PpExpr *expr = pp_cast_expr();
  //  return new_expr_unary(EX_REF, NULL, tok, expr);
  //}

  //if ((tok = match(TK_MUL)) != NULL) {
  //  PpExpr *expr = pp_cast_expr();
  //  return new_expr_unary(EX_DEREF, NULL, tok, expr);
  //}

  //if ((tok = match(TK_INC)) != NULL) {
  //  PpExpr *expr = pp_unary();
  //  return new_expr_unary(EX_PREINC, NULL, tok, expr);
  //}

  //if ((tok = match(TK_DEC)) != NULL) {
  //  PpExpr *expr = pp_unary();
  //  return new_expr_unary(EX_PREDEC, NULL, tok, expr);
  //}

  return pp_postfix();
}

static PpResult pp_cast_expr(void) {
  return pp_unary();
}

static PpResult pp_mul(void) {
  PpResult result = pp_cast_expr();
  for (;;) {
    Token *tok;
    if (!(((tok = match(TK_MUL)) != NULL) ||
          ((tok = match(TK_DIV)) != NULL) ||
          ((tok = match(TK_MOD)) != NULL)))
      return result;

    PpResult rhs = pp_cast_expr();
    switch (tok->kind) {
    case TK_MUL:  result *= rhs; break;
    case TK_DIV:  result /= rhs; break;
    case TK_MOD:  result %= rhs; break;
    default:  assert(false); break;
    }
  }
}

static PpResult pp_add(void) {
  PpResult result = pp_mul();
  for (;;) {
    Token *tok;
    if (!(((tok = match(TK_ADD)) != NULL) ||
          ((tok = match(TK_SUB)) != NULL)))
      return result;

    PpResult rhs = pp_mul();
    if (tok->kind == TK_ADD)
      result += rhs;
    else
      result -= rhs;
  }
}

static PpResult pp_shift(void) {
  PpResult result = pp_add();
  for (;;) {
    Token *tok;
    if (!(((tok = match(TK_LSHIFT)) != NULL) ||
          ((tok = match(TK_RSHIFT)) != NULL)))
      return result;

    PpResult lhs = result, rhs = pp_add();
    if (tok->kind == TK_LSHIFT)
      result = lhs << rhs;
    else
      result = lhs >> rhs;
  }
}

static PpResult pp_cmp(void) {
  PpResult result = pp_shift();
  for (;;) {
    Token *tok;
    if (!(((tok = match(TK_LT)) != NULL) ||
          ((tok = match(TK_GT)) != NULL) ||
          ((tok = match(TK_LE)) != NULL) ||
          ((tok = match(TK_GE)) != NULL)))
      return result;

    PpResult lhs = result, rhs = pp_shift();
    switch (tok->kind) {
    case TK_LT:  result = lhs <  rhs ? 1 : 0; break;
    case TK_LE:  result = lhs <= rhs ? 1 : 0; break;
    case TK_GE:  result = lhs >= rhs ? 1 : 0; break;
    case TK_GT:  result = lhs >  rhs ? 1 : 0; break;
    default:  assert(false); break;
    }
  }
}

static PpResult pp_eq(void) {
  PpResult result = pp_cmp();
  for (;;) {
    Token *tok;
    if (!(((tok = match(TK_EQ)) != NULL) ||
          ((tok = match(TK_NE)) != NULL)))
      return result;

    PpResult lhs = result, rhs = pp_cmp();
    result = lhs == rhs ? 1 : 0;
    if (tok->kind != TK_EQ)
      result = 1 - result;
  }
}

static PpResult pp_and(void) {
  PpResult result = pp_eq();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_AND)) != NULL) {
      PpResult lhs = result, rhs = pp_eq();
      result = lhs & rhs;
    } else
      return result;
  }
}

static PpResult pp_xor(void) {
  PpResult result = pp_and();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_HAT)) != NULL) {
      PpResult lhs = result, rhs = pp_and();
      result = lhs ^ rhs;
    } else
      return result;
  }
}

static PpResult pp_or(void) {
  PpResult result = pp_xor();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_OR)) != NULL) {
      PpResult lhs = result, rhs = pp_xor();
      result = lhs | rhs;
    } else
      return result;
  }
}

static PpResult pp_logand(void) {
  PpResult result = pp_or();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_LOGAND)) != NULL) {
      PpResult rhs = pp_logand();
      result = result && rhs;
    } else
      return result;
  }
}

static PpResult pp_logior(void) {
  PpResult result = pp_logand();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_LOGIOR)) != NULL) {
      PpResult rhs = pp_logand();
      result = result || rhs;
    } else
      return result;
  }
}

static PpResult pp_conditional(void) {
  return pp_logior();
}

static PpResult pp_assign(void) {
  return pp_conditional();
}

PpResult pp_expr(void) {
  PpResult result = pp_assign();
  const Token *tok;
  while ((tok = match(TK_COMMA)) != NULL) {
    PpResult next_result = pp_assign();
    result = next_result;
  }
  return result;
}

static Token *match2(enum TokenKind kind, Stream *stream) {
  while (match(TK_EOF)) {
    char *line = NULL;
    size_t capa = 0;
    ssize_t len = getline(&line, &capa, stream->fp);
    if (len == -1)
      return NULL;
    ++stream->lineno;
    set_source_string(line, stream->filename, stream->lineno);
  }
  return match(kind);
}

Vector *pp_funargs(Stream *stream) {
  Vector *args = NULL;
  if (match2(TK_LPAR, stream)) {
    args = new_vector();
    if (!match2(TK_RPAR, stream)) {
      StringBuffer sb;
      sb_init(&sb);
      const char *start = NULL;
      const char *end = NULL;
      int paren = 0;
      for (;;) {
        Token *tok;
        for (;;) {
          tok = match(-1);
          if (tok->kind != TK_EOF)
            break;

          if (start != end)
            sb_append(&sb, start, end);
          if (!sb_empty(&sb))
            sb_append(&sb, "\n", NULL);
          start = end = NULL;

          ssize_t len = -1;
          char *line = NULL;
          if (stream != NULL) {
            size_t capa = 0;
            len = getline(&line, &capa, stream->fp);
          }
          if (len == -1) {
            parse_error(NULL, "`)' expected");
            return NULL;
          }
          ++stream->lineno;
          set_source_string(line, stream->filename, stream->lineno);
        }

        if (tok->kind == TK_COMMA || tok->kind == TK_RPAR) {
          if (paren <= 0) {
            if (sb_empty(&sb)) {
              if (start == end)
                parse_error(tok, "expression expected");
              vec_push(args, strndup_(start, end - start));
            } else {
              if (start != end)
                sb_append(&sb, start, end);
              vec_push(args, sb_to_string(&sb));
              sb_clear(&sb);
            }
            start = end = NULL;

            if (tok->kind == TK_RPAR)
              break;
            else
              continue;
          }

          if (tok->kind == TK_RPAR)
            --paren;
        } else if (tok->kind == TK_LPAR) {
          ++paren;
        }
        if (start == NULL)
          start = tok->begin;
        end = tok->end;
      }
    }
  }
  return args;
}
#include "preprocessor.h"

#include <assert.h>
#include <ctype.h>
#include <libgen.h>  // dirname
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "lexer.h"
#include "macro.h"
#include "pp_parser.h"
#include "table.h"
#include "type.h"
#include "util.h"

char *cat_path_cwd(const char *dir, const char *path) {
  char *cwd = getcwd(NULL, 0);
  char *root = cat_path(cwd, dir);
  free(cwd);
  return cat_path(root, path);
}

char *fullpath(const char *filename) {
  return cat_path_cwd(dirname(strdup_(filename)), basename((char*)filename));
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

Table macro_table;  // <Name, Macro*>

static FILE *pp_ofp;
static Vector *sys_inc_paths;  // <const char*>
static Vector *pragma_once_files;  // <const char*>

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
  if (!is_fullpath(filename))
    filename = fullpath(filename);
  vec_push(pragma_once_files, filename);
}

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
    fn = cat_path_cwd(dirname(strdup_(srcname)), path);
    if (registered_pragma_once(fn))
      return;
    fp = fopen(fn, "r");
  }
  if (fp == NULL) {
    // Search from system include directries.
    for (int i = 0; i < sys_inc_paths->len; ++i) {
      fn = cat_path_cwd(sys_inc_paths->data[i], path);
      if (registered_pragma_once(fn))
        return;
      fp = fopen(fn, "r");
      if (fp != NULL)
        break;
    }
    if (fp == NULL) {
      error("Cannot open file: %s", path);
      return;
    }
  }

  fprintf(pp_ofp, "# 1 \"%s\" 1\n", fn);
  int lineno = preprocess(fp, fn);
  fprintf(pp_ofp, "# %d \"%s\" 2\n", lineno, fn);
  fclose(fp);
}

void handle_pragma(const char *p, const char *filename) {
  const char *begin = p;
  const char *end = read_ident(p);
  if ((end - begin) == 4 && strncmp(begin, "once", 4) == 0) {
    if (!registered_pragma_once(filename))
      register_pragma_once(filename);
  } else {
    fprintf(stderr, "Warning: unhandled #pragma: %s\n", p);
  }
}

static void push_text_segment(Vector *segments, const char *start, const char *token_begin) {
  if (token_begin > start) {
    Segment *seg = malloc(sizeof(*seg));
    seg->kind = SK_TEXT;
    seg->text = strndup_(start, token_begin - start);
    vec_push(segments, seg);
  }
}

Vector *parse_macro_body(const char *p, const Vector *params, bool va_args, Stream *stream) {
  Vector *segments = new_vector();
  set_source_string(p, stream->filename, stream->lineno);
  int param_len = params != NULL ? params->len : 0;
  const Name *key_va_args = alloc_name("__VA_ARGS__", NULL, false);
  const char *start = p;
  const char *end = start;
  for (;;) {
    Token *tok = match(-1);
    if (tok->kind == TK_EOF)
      break;
    switch (tok->kind) {
    case TK_IDENT:
      {
        int param_index = -1;
        if (va_args && equal_name(tok->ident, key_va_args)) {
          param_index = param_len;
        } else {
          for (int i = 0; i < param_len; ++i) {
            if (equal_name(tok->ident, params->data[i])) {
              param_index = i;
              break;
            }
          }
        }
        if (param_index >= 0) {
          push_text_segment(segments, start, tok->begin);

          Segment *seg = malloc(sizeof(*seg));
          seg->kind = SK_PARAM;
          seg->param = param_index;
          vec_push(segments, seg);

          start = end = tok->end;
          continue;
        }
      }
      break;
    case PPTK_CONCAT:
      push_text_segment(segments, start, end);

      start = end = skip_whitespaces(tok->end);
      continue;
    case PPTK_STRINGIFY:
      {
        Token *ident;
        if ((ident = match(TK_IDENT)) != NULL) {
          int param_index = -1;
          for (int i = 0; i < param_len; ++i) {
            if (equal_name(ident->ident, params->data[i])) {
              param_index = i;
              break;
            }
          }
          if (param_index >= 0) {
            push_text_segment(segments, start, tok->begin);

            Segment *seg = malloc(sizeof(*seg));
            seg->kind = SK_STRINGIFY;
            seg->param = param_index;
            vec_push(segments, seg);

            start = end = ident->end;
            continue;
          }
      }
      }
      break;
    default:
      break;
    }
    end = tok->end;
  }

  if (start != end) {
    Segment *seg = malloc(sizeof(*seg));
    seg->kind = SK_TEXT;
    seg->text = strndup_(start, end - start);
    vec_push(segments, seg);
  }
  return segments;
}

void handle_define(const char *p, Stream *stream) {
  const char *begin = p;
  const char *end = read_ident(p);
  if (end == NULL)
    error("`ident' expected");
  const Name *name = alloc_name(begin, end, false);
  p = end;

  Vector *params = NULL;
  bool va_args = false;
  if (*p == '(') {
    // Macro with parameter.
    params = new_vector();
    set_source_string(p + 1, stream->filename, stream->lineno);
    if (!match(TK_RPAR)) {
      for (;;) {
        Token *tok;
        if ((tok = match(TK_ELLIPSIS)) != NULL) {
          va_args = true;
          consume(TK_RPAR, "`)' expected");
          break;
        } else {
          tok = consume(TK_IDENT, "`ident' expected");
          vec_push(params, tok->ident);
          if (match(TK_RPAR))
            break;
          consume(TK_COMMA, "`,' or `)' expected");
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
  table_put(&macro_table, name, new_macro(params, va_args, segments));
}

void handle_undef(const char *p) {
  const char *begin = p;
  const char *end = read_ident(p);
  if (end == NULL)
    error("`ident' expected");
  const Name *name = alloc_name(begin, end, false);

  table_delete(&macro_table, name);
}

bool handle_block_comment(const char *begin, const char **pp, Stream *stream) {
  const char *p = skip_whitespaces(*pp);
  if (*p != '/' || p[1] != '*')
    return false;

  p += 2;
  for (;;) {
    if (*p == '\0') {
      fwrite(begin, p - begin, 1, pp_ofp);
      fputc('\n', pp_ofp);

      char *line = NULL;
      size_t capa = 0;
      ssize_t len = getline(&line, &capa, stream->fp);
      if (len == -1) {
        *pp = p;
        return true;
      }
      ++stream->lineno;
      begin = p = line;
      continue;
    }

    if (*p == '*' && p[1] == '/') {
      p += 2;
      fwrite(begin, p - begin, 1, pp_ofp);
      *pp = p;
      return true;
    }

    ++p;
  }
}

void process_line(const char *line, Stream *stream) {
  set_source_string(line, stream->filename, stream->lineno);

  const char *begin = get_lex_p();
  for (;;) {
    const char *p = get_lex_p();
    if (p != NULL) {
      if (handle_block_comment(begin, &p, stream)) {
        begin = p;
        set_source_string(begin, stream->filename, stream->lineno);
      }
    }

    if (match(TK_EOF))
      break;

    Token *ident = match(TK_IDENT);
    Macro *macro;
    if (ident != NULL && (macro = table_get(&macro_table, ident->ident)) != NULL) {
      s_stream = stream;
      Vector *args = NULL;
      if (macro->params != NULL)
        args = pp_funargs(s_stream);

      StringBuffer sb;
      sb_init(&sb);
      if (!expand(macro, ident, args, ident->ident, &sb))
        continue;

      if (ident->begin != begin)
        fwrite(begin, ident->begin - begin, 1, pp_ofp);

      const char *left = get_lex_p();
      if (left != NULL)
        sb_append(&sb, left, NULL);
      char *expanded = sb_to_string(&sb);

      set_source_string(expanded, NULL, -1);
      begin = get_lex_p();
      continue;
    }

    match(-1);
  }

  fprintf(pp_ofp, "%s\n", begin);
}

bool handle_ifdef(const char *p) {
  const char *begin = p;
  const char *end = read_ident(p);
  if (end == NULL)
    error("`ident' expected");
  const Name *name = alloc_name(begin, end, false);
  return table_get(&macro_table, name) != NULL;
}

bool handle_if(const char *p, Stream *stream) {
  set_source_string(p, stream->filename, stream->lineno);
  return pp_expr() != 0;
}

#define CF_ENABLE         (1 << 0)
#define CF_SATISFY_SHIFT  (1)
#define CF_SATISFY_MASK   (3 << CF_SATISFY_SHIFT)

intptr_t cond_value(bool enable, int satisfy) {
  return (enable ? CF_ENABLE : 0) | (satisfy << CF_SATISFY_SHIFT);
}

static void define_file_macro(const char *filename, const Name *key_file) {
  size_t len = strlen(filename);
  char *buf = malloc(len + 2 + 1);
  snprintf(buf, len + 2 + 1, "\"%s\"", filename);
  table_put(&macro_table, key_file, new_macro_single(buf));
}

void init_preprocessor(FILE *ofp) {
  pp_ofp = ofp;
  sys_inc_paths = new_vector();
  pragma_once_files = new_vector();

  init_lexer();
}

int preprocess(FILE *fp, const char *filename) {
  Vector *condstack = new_vector();
  bool enable = true;
  int satisfy = 0;  // #if condition: 0=not satisfied, 1=satisfied, 2=else
  char linenobuf[sizeof(int) * 3 + 1];  // Buffer for __LINE__

  const Name *key_file = alloc_name("__FILE__", NULL, false);
  const Name *key_line = alloc_name("__LINE__", NULL, false);

  Macro *old_file_macro = table_get(&macro_table, key_file);
  Macro *old_line_macro = table_get(&macro_table, key_line);

  define_file_macro(filename, key_file);
  table_put(&macro_table, key_line, new_macro_single(linenobuf));

  Stream stream;
  stream.filename = filename;
  stream.fp = fp;

  for (stream.lineno = 1;; ++stream.lineno) {
    char *line = NULL;
    size_t capa = 0;
    ssize_t len = getline(&line, &capa, fp);
    if (len == -1)
      break;

    snprintf(linenobuf, sizeof(linenobuf), "%d", stream.lineno);

    for (;;) {
      if (len > 0 && line[len - 1] == '\n')
        line[--len] = '\0';
      if (len < 1 || line[len - 1] != '\\')
        break;
      // Continue line.
      ++stream.lineno;
      line[--len] = '\0';
      ssize_t nextlen = getline_cat(&line, &capa, fp, len);
      if (nextlen != -1)
        len = nextlen;
    }

    // Find '#'
    const char *directive = find_directive(line);
    if (directive == NULL) {
      if (enable)
        process_line(line, &stream);
      else
        fprintf(pp_ofp, "\n");
      continue;
    }
    fprintf(pp_ofp, "\n");

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
        fprintf(pp_ofp, "# %d \"%s\" 1\n", stream.lineno + 1, filename);
      } else if ((next = keyword(directive, "define")) != NULL) {
        handle_define(next, &stream);
      } else if ((next = keyword(directive, "undef")) != NULL) {
        handle_undef(next);
      } else if ((next = keyword(directive, "pragma")) != NULL) {
        handle_pragma(next, filename);
      } else if ((next = keyword(directive, "error")) != NULL) {
        fprintf(stderr, "%s(%d): error\n", filename, stream.lineno);
        error("%s", line);
      } else {
        error("unknown directive: %s", directive);
      }
    }
  }

  if (condstack->len > 0)
    error("#if not closed");

  table_put(&macro_table, key_file, old_file_macro);
  table_put(&macro_table, key_line, old_line_macro);

  return stream.lineno;
}

void define_macro(const char *arg) {
  char *p = strchr(arg, '=');
  if (p == NULL) {
    table_put(&macro_table, alloc_name(arg, NULL, true), new_macro(NULL, false, NULL));
  } else {
    const Name *name = alloc_name(arg, p, true);
    table_put(&macro_table, name, new_macro_single(p + 1));
  }
}

void define_macro_simple(const char *label) {
  table_put(&macro_table, alloc_name(label, NULL, true), new_macro(NULL, false, NULL));
}

void add_system_inc_path(const char *path) {
  vec_push(sys_inc_paths, strdup_(path));
}
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
