// Shell.

#include "ctype.h"
#include "fcntl.h"
#include "stdbool.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "sys/wait.h"
#include "termios.h"
#include "unistd.h"

bool setRawMode(bool enable, int fd) {
  static struct termios orig_termios;
  static bool termios_saved;
  static bool rawmode;

  if (enable) {
    struct termios raw;

    if (!isatty(fd))
      return false;
    if (!termios_saved) {
      if (tcgetattr(fd, &orig_termios) == -1)
        return false;
      termios_saved = true;
    }

    raw = orig_termios;  /* modify the original mode */
    /* input modes: no break, no CR to NL, no parity check, no strip char,
     * no start/stop output control. */
    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    /* output modes - disable post processing */
    raw.c_oflag &= ~OPOST;
    /* control modes - set 8 bit chars */
    raw.c_cflag |= CS8;
    /* local modes - choing off, canonical off, no extended functions,
     * no signal chars (^Z,^C) */
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    /* control chars - set return condition: min number of bytes and timer. */
    //raw.c_cc[VMIN] = 0; /* Return each byte, or zero for timeout. */
    //raw.c_cc[VTIME] = 1; /* 100 ms timeout (unit is tens of second). */
    raw.c_cc[VMIN] = 1; /* Return each byte, or zero for timeout. */
    raw.c_cc[VTIME] = 0; /* 100 ms timeout (unit is tens of second). */

    /* put terminal in raw mode after flushing */
    if (tcsetattr(fd,TCSAFLUSH,&raw) < 0)
      return false;
    rawmode = true;
    return true;
  } else {
    if (rawmode && termios_saved) {
      tcsetattr(fd, TCSAFLUSH, &orig_termios);
      rawmode = false;
    }
    return true;
  }
}

// ================================================
// Ring buffer

typedef struct {
  const void** buf;
  unsigned int capa;
  unsigned int r;
  unsigned int w;
} RingBuf;

bool init_ringbuf(RingBuf *rb, unsigned int capa) {
  const void ** buf = malloc(sizeof(void*) * capa);
  if (buf == NULL)
    return false;

  rb->buf = buf;
  rb->capa = capa;
  rb->r = rb->w = 0;
  return true;
}

int ringbuf_count(RingBuf *rb) {
  return rb->w - rb->r;
}

bool ringbuf_full(RingBuf *rb) {
  return rb->w - rb->r >= rb->capa;
}

const void *ringbuf_get(RingBuf *rb, int i) {
  return rb->buf[(rb->w - i - 1) % rb->capa];
}

void ringbuf_put(RingBuf *rb, const void *value) {
  unsigned int capa = rb->capa;
  unsigned int w = rb->w, r = rb->r;
  rb->buf[w % capa] = value;
  ++w;
  if (w - r > capa) {
    r = w - capa;
    //if (r >= capa) {
    //  r -= capa;
    //  w -= capa;
    //}
    rb->r = r;
  }
  rb->w = w;
}

// ================================================
// Readline

#define OUTCAPA  (128)
#define MAX_HISTORY  (128)

#define C(x)  ((x)-'@')  // Control-x

typedef struct {
  char outbuf[OUTCAPA];
  int outp;

  RingBuf history;

  char *buf;
  size_t capa;

  int ifd, ofd;
  int p, n;
} ReadLine;

ReadLine rl;

void init_readline(int ifd, int ofd) {
  rl.ifd = ifd;
  rl.ofd = ofd;

  rl.outp = 0;
  init_ringbuf(&rl.history, MAX_HISTORY);
  rl.buf = NULL;
  rl.capa = 0;
  rl.p = rl.n = 0;
}

static void rl_add_history(const char *input) {
  const char *dup = strdup(input);
  if (dup == NULL)
    return;

  RingBuf *history = &rl.history;
  if (ringbuf_full(history)) {
    char *last = (char*)ringbuf_get(history, ringbuf_count(history) - 1);
    free(last);
  }

  ringbuf_put(history, dup);
}

static int readc(int fd, bool use_raw_mode) {
  if (use_raw_mode) {
    ssize_t nread;
    unsigned char c;
    while ((nread = read(fd, &c, 1)) == 0)
      ;
    return nread == -1 ? -1 : c;
  } else {
    unsigned char c;
    int nread = read(fd, &c, 1);
    return nread == 1 ? c : -1;
  }
}

static void flush() {
  if (rl.outp <= 0)
    return;
  write(rl.ofd, rl.outbuf, rl.outp);
  rl.outp = 0;
}

static void out(char c) {
  rl.outbuf[rl.outp++] = c;
  if (rl.outp >= OUTCAPA)
    flush();
}

static void extend(size_t n, char **pp, size_t *pcapa) {
  if (n <= *pcapa)
    return;
  const int MIN = 16;
  if (n - *pcapa < MIN)
    n = *pcapa + MIN;
  char *p = realloc(*pp, n);
  if (p == NULL) {
    fprintf(stderr, "malloc failed\n");
    exit(1);
  }
  *pp = p;
  *pcapa = n;
}

void rl_move_to_start(void) {
  for (int i = 0; i < rl.p; ++i)
    out('\b');
  rl.p = 0;
}

void rl_move_to_last(void) {
  if (rl.p < rl.n) {
    for (int i = rl.p; i < rl.n; ++i)
      out(rl.buf[i]);
    rl.p = rl.n;
  }
}

void rl_delete(void) {
  if (rl.p < rl.n) {
    --rl.n;
    int m = rl.n - rl.p;
    if (m > 0) {
      memmove(&rl.buf[rl.p], &rl.buf[rl.p + 1], m);
      for (int i = rl.p; i < rl.n; ++i)
        out(rl.buf[i]);
    }
    out(' ');
    for (int i = -1; i < m; ++i)
      out('\b');
  }
}

void rl_backspace(void) {
  if (rl.p <= 0)
    return;
  out('\b');
  --rl.p;
  rl_delete();
}

void rl_kill_after(void) {
  int m = rl.n - rl.p;
  if (m > 0) {
    for (int i = 0; i < m; ++i)
      out(' ');
    for (int i = 0; i < m; ++i)
      out('\b');
    rl.n = rl.p;
  }
}

void rl_clear_input(void) {
  rl_move_to_start();
  rl_kill_after();
}

void rl_putc(char c) {
  extend(rl.n + 1, &rl.buf, &rl.capa);
  int m = rl.n - rl.p;
  if (m > 0) {
    memmove(&rl.buf[rl.p + 1], &rl.buf[rl.p], m);
  }
  rl.buf[rl.p] = c;

  ++rl.n;
  for (int i = rl.p; i < rl.n; ++i)
    out(rl.buf[i]);
  ++rl.p;
  for (int i = 0; i < m; ++i)
    out('\b');
}

void rl_set_input(const char *input) {
  rl_clear_input();
  for (const char *p = input; *p != '\0'; ++p)
    rl_putc(*p);
}

ssize_t readline(char **lineptr, size_t *pcapa, bool use_raw_mode) {
  rl.buf = *lineptr;
  rl.capa = *pcapa;
  rl.p = rl.n = 0;
  int his_index = -1;

  if (use_raw_mode && !setRawMode(true, rl.ifd)) {
    fprintf(stderr,"Failed to set raw mode\n");
    exit(1);
  }

  for (;;) {
    flush();
    int c = readc(rl.ifd, use_raw_mode);
    switch (c) {
    case -1:
      return -1;

    case C('C'):
      return -1;

    case C('B'):
      if (rl.p > 0) {
        out('\b');
        --rl.p;
      }
      continue;

    case C('F'):
      if (rl.p < rl.n) {
        out(rl.buf[rl.p]);
        ++rl.p;
      }
      continue;

    case C('A'):  // Beginning of line
      rl_move_to_start();
      continue;

    case C('E'):  // End of line
      rl_move_to_last();
      continue;

    case C('K'):  // Kill
      rl_kill_after();
      continue;

    case C('H'): case '\x7f':  // Backspace
      rl_backspace();
      continue;

    case C('D'):  // Delete
      rl_delete();
      continue;

    case '\r': case '\n':
      extend(rl.n + 1, &rl.buf, &rl.capa);
      rl.buf[rl.n] = '\0';
      write(rl.ofd, "\r\n", 2);
      if (rl.n > 0 && rl.buf[0] != ' ')
        rl_add_history(rl.buf);
      *lineptr = rl.buf;
      *pcapa = rl.capa;
      if (use_raw_mode)
        setRawMode(false, rl.ifd);
      return rl.n;

    case C('P'):  // Prev history
      if (his_index + 1 < ringbuf_count(&rl.history)) {
        ++his_index;
        rl_set_input(ringbuf_get(&rl.history, his_index));
      }
      continue;

    case C('N'):  // Next history
      if (his_index > -1) {
        --his_index;
        if (his_index < 0)
          rl_clear_input();
        else
          rl_set_input(ringbuf_get(&rl.history, his_index));
      }
      continue;

    default:
      break;
    }

    if (c >= ' ') {
      rl_putc(c);
    }
  }
}

// ================================================

// Parsed command representation
#define EXEC  1
#define REDIR 2
#define PIPE  3
#define LIST  4
#define BACK  5

#define MAXARGS 10

#ifndef FALSE
#define FALSE  (0)
#endif
#ifndef TRUE
#define TRUE   (1)
#endif

#define AND  ('^')

struct cmd {
  int type;
};

struct execcmd {
  int type;
  char *argv[MAXARGS];
};

struct redircmd {
  int type;
  struct cmd *cmd;
  char *file;
  int mode;
  int fd;
};

struct pipecmd {
  int type;
  struct cmd *left;
  struct cmd *right;
};

struct listcmd {
  int type;
  int cond;
  struct cmd *left;
  struct cmd *right;
};

struct backcmd {
  int type;
  struct cmd *cmd;
};

int fork1(void);  // Fork but panics on failure.
void panic(char*);
struct cmd *parsecmd(char*);

// Shell variables.
typedef struct ShellVarLink {
  struct ShellVarLink *next;
  const char *name;
  char *value;
} ShellVarLink;

static ShellVarLink *s_shellvars;

static ShellVarLink*
findsvar(const char *name)
{
  ShellVarLink *p;
  for (p = s_shellvars; p; p = p->next) {
    if (strcmp(p->name, name) == 0)
      return p;
  }
  return NULL;
}

char*
getsvar(const char *name)
{
  ShellVarLink *p;
  p = findsvar(name);
  return p ? p->value : NULL;
}

void
setsvar(const char *name, const char *value)
{
  ShellVarLink *p;

  p = findsvar(name);
  if (p != NULL) {
    free(p->value);
  } else {
    p = malloc(sizeof(*p));
    p->next = s_shellvars;
    s_shellvars = p;
    p->name = strdup(name);
  }
  p->value = strdup(value);
}

static int readHex(const char* p, int order) {
  int x = 0;
  for (int i = 0; i < order; ++i) {
    char c = *p++;
    if ('0' <= c && c <= '9')
      c -= '0';
    else if ('A' <= c && c <= 'F')
      c -= 'A' - 10;
    else if ('a' <= c && c <= 'f')
      c -= 'a' - 10;
    else
      break;
    x = (x << 4) | c;
  }
  return x;
}

static char* strdup2(const char *start, const char *end) {
  size_t len = end - start;
  char *str = malloc(len + 1);
  if (str != NULL) {
    memcpy(str, start, len);
    str[len] = '\0';
  }
  return str;
}

// Expand environment variables.
char*
expandarg(char *start, char *end)
{
  if (*start == '$') {
    char buf[100];
    size_t len = end - start - 1;
    len = len > sizeof(buf) - 1 ? sizeof(buf) - 1 : len;
    strncpy(buf, start + 1, len);
    buf[len] = '\0';
    char* var = getsvar(buf);  // TODO: Expand multi-values.
    if (var == NULL) {
      var = getenv(buf);
      if (var == NULL)
        var = " ";
    }
    return strdup(var);
  } else {
    // Handle escape sequence: Inplace replacement.
    char *p, *q;
    for (p = q = start; p < end; ) {
      char c = *p++;
      if (c != '\\') {
        *q++ = c;
        continue;
      }

      // Escape
      c = p < end ? *p++ : '\0';
      switch (c) {
        case 'n':
          c = '\n';
          break;
        case 't':
          c = '\t';
          break;
        case '\'':
          c = '\'';
          break;
        case '"':
          c = '"';
          break;
        case '$':
          c = '$';
          break;
        case '\\':
          c = '\\';
          break;
        case 'x':
          if (p + 2 <= end) {
            c = readHex(p, 2);
            p += 2;
          } else {
            c = '?';
            p = end;
          }
          break;
        default:
          break;
      }
      *q++ = c;
    }
    return strdup2(start, q);
  }
}

static void
putLastExitCode(int exitcode)
{
  char buf[16];
  sprintf(buf, "%d", exitcode);
  setsvar("?", buf);
}

// Execute execcmd.  Never returns.
void
runecmd(struct execcmd *ecmd)
{
  execvp(ecmd->argv[0], ecmd->argv);
  fprintf(stderr, "sh: command not found: %s\n", ecmd->argv[0]);
  exit(1);
}

// Execute cmd.  Never returns.
void
runcmd(struct cmd *cmd)
{
  int p[2];
  struct backcmd *bcmd;
  struct execcmd *ecmd;
  struct listcmd *lcmd;
  struct pipecmd *pcmd;
  struct redircmd *rcmd;
  int ec1, ec2;

  if(cmd == NULL)
    exit(0);

  switch(cmd->type){
  default:
    panic("runcmd");

  case EXEC:
    ecmd = (struct execcmd*)cmd;
    if(ecmd->argv[0] == NULL)
      exit(0);
    runecmd(ecmd);
    break;

  case REDIR:
    rcmd = (struct redircmd*)cmd;
    close(rcmd->fd);
    if(open(rcmd->file, rcmd->mode) < 0){
      fprintf(stderr, "open %s failed\n", rcmd->file);
      exit(1);
    }
    runcmd(rcmd->cmd);
    break;

  case LIST:
    lcmd = (struct listcmd*)cmd;
    if(fork1() == 0)
      runcmd(lcmd->left);
    wait(&ec1);
    putLastExitCode(ec1);
    if (lcmd->cond == AND && ec1 != EXIT_SUCCESS)
      exit(ec1);
    runcmd(lcmd->right);
    break;

  case PIPE:
    pcmd = (struct pipecmd*)cmd;
    if(pipe(p) < 0)
      panic("pipe");
    if(fork1() == 0){
      close(1);
      dup(p[1]);
      close(p[0]);
      close(p[1]);
      runcmd(pcmd->left);
    }
    if(fork1() == 0){
      close(0);
      dup(p[0]);
      close(p[0]);
      close(p[1]);
      runcmd(pcmd->right);
    }
    close(p[0]);
    close(p[1]);
    wait(&ec1);
    wait(&ec2);
    exit(ec1 != 0 ? ec1 : ec2);
    break;

  case BACK:
    bcmd = (struct backcmd*)cmd;
    if(fork1() == 0)
      runcmd(bcmd->cmd);
    break;
  }
  exit(1);
}

int
getcmd(FILE* fp, char **linebuf, size_t *pcapa, bool raw_mode)
{
  ssize_t r = readline(linebuf, pcapa, raw_mode);
  if (r < 0)
    return -1;
  return 0;
}

void
sh(FILE* fp)
{
  int tty = isatty(fileno(fp));
  char *buf = NULL;
  size_t bufcapa = 0;
  int exitcode;

  // Read and run input commands.
  for (;;) {
    if (tty)
      fprintf(stderr, "$ ");
    if (getcmd(fp, &buf, &bufcapa, tty) < 0)
      break;
    if(strncmp(buf, "cd ", 3) == 0){
      // Chdir must be called by the parent, not the child.
      //buf[strlen(buf)-1] = '\0';  // chop \n
      if(chdir(buf+3) < 0)
        fprintf(stderr, "cannot cd %s\n", buf+3);
      continue;
    }
    if(fork1() == 0)
      runcmd(parsecmd(buf));
    wait(&exitcode);
    putLastExitCode(exitcode);
  }
}

void onexit(void) {
  setRawMode(false, STDIN_FILENO);
}

int
main(int argc, char* argv[])
{
  int fd;

  // Ensure that three file descriptors are open.
  while((fd = open("console", O_RDWR)) >= 0){
    if(fd >= 3){
      close(fd);
      break;
    }
  }

  if (argc < 2) {
    atexit(onexit);

    init_readline(STDIN_FILENO, STDOUT_FILENO);

    sh(stdin);
  } else {
    for (int i = 1; i < argc; ++i) {
      FILE* fp = fopen(argv[i], "r");
      if (fp == NULL) {
        char buf[128];
        sprintf(buf, "Cannot open: %s", argv[i]);
        panic(buf);
      }

      init_readline(fileno(fp), STDOUT_FILENO);

      sh(fp);
      fclose(fp);
    }
  }
  return 0;
}

void
panic(char *s)
{
  fprintf(stderr, "%s\n", s);
  exit(1);
}

int
fork1(void)
{
  int pid;

  pid = fork();
  if(pid == -1)
    panic("fork");
  return pid;
}

//PAGEBREAK!
// Constructors

struct cmd*
execcmd(void)
{
  struct execcmd *cmd;

  cmd = calloc(1, sizeof(*cmd));
  cmd->type = EXEC;
  return (struct cmd*)cmd;
}

struct cmd*
redircmd(struct cmd *subcmd, char *file, int mode, int fd)
{
  struct redircmd *cmd;

  cmd = calloc(1, sizeof(*cmd));
  cmd->type = REDIR;
  cmd->cmd = subcmd;
  cmd->file = file;
  cmd->mode = mode;
  cmd->fd = fd;
  return (struct cmd*)cmd;
}

struct cmd*
pipecmd(struct cmd *left, struct cmd *right)
{
  struct pipecmd *cmd;

  cmd = calloc(1, sizeof(*cmd));
  cmd->type = PIPE;
  cmd->left = left;
  cmd->right = right;
  return (struct cmd*)cmd;
}

struct cmd*
listcmd(int cond, struct cmd *left, struct cmd *right)
{
  struct listcmd *cmd;

  cmd = calloc(1, sizeof(*cmd));
  cmd->type = LIST;
  cmd->cond = cond;
  cmd->left = left;
  cmd->right = right;
  return (struct cmd*)cmd;
}

struct cmd*
backcmd(struct cmd *subcmd)
{
  struct backcmd *cmd;

  cmd = calloc(1, sizeof(*cmd));
  cmd->type = BACK;
  cmd->cmd = subcmd;
  return (struct cmd*)cmd;
}
//PAGEBREAK!
// Parsing

static const char symbols[] = "<|>&;()#";

char *skipws(char *s, char *es) {
  while(s < es && isspace(*s))
    s++;
  return s;
}

int
peektoken(char *s, char *es, char **ps)
{
  int tok;

  s = skipws(s, es);
  tok = *s;
  switch(tok){
  case '\0':
    break;
  case '|':
  case '(':
  case ')':
  case ';':
  case '<':
  case '#':
    s++;
    break;
  case '&':
    s++;
    if(*s == '&'){
      tok = AND;
      s++;
    }
    break;
  case '>':
    s++;
    if(*s == '>'){
      tok = '+';
      s++;
    }
    break;
  case '\'':
  case '"':
  default:
    tok = 'a';
    break;
  }

  if (ps != NULL)
    *ps = s;
  return tok;
}

int
gettoken(char **ps, char *es, char **q)
{
  char *s, *start;
  int tok;

  tok = peektoken(*ps, es, &s);
  start = s;
  if (tok != 'a') {
    if (q != NULL)
      *q = NULL;
  } else {
    int singleQuote = FALSE;
    switch(*s){
    case '\'':
    case '"':
      {
        char term = *s++;
        singleQuote = term == '\'';
        start = s;
        for (;;) {
          if (*s == term) {
            *s = ' ';
            break;
          }
          if (*s == '\\') {
            if (++s >= es) {
              // TODO: Continue line.
              panic("quote not closed");
            }
          }
          if (++s >= es)
            panic("quote not closed");
        }
      }
      break;
    default:
      while(s < es && !isspace(*s) && strchr(symbols, *s) == NULL)
        s++;
      break;
    }
    if (q != NULL) {
      if (singleQuote)
        *q = strdup2(start, s);
      else
        *q = expandarg(start, s);
    }
  }

  *ps = s;
  return tok;
}

int
peek(char **ps, char *es, char *toks)
{
  char *s;
  int tok;

  *ps = s = skipws(*ps, es);
  tok = peektoken(s, es, NULL);
  return tok != '\0' && strchr(toks, tok) != NULL;
}

struct cmd *parseline(char**, char*);
struct cmd *parsepipe(char**, char*);
struct cmd *parseexec(char**, char*);

struct cmd*
parsecmd(char *s)
{
  char *es;
  struct cmd *cmd;

  es = s + strlen(s);
  cmd = parseline(&s, es);
  peek(&s, es, "");
  if(s != es){
    fprintf(stderr, "leftovers: %s\n", s);
    panic("syntax");
  }
  return cmd;
}

struct cmd*
parseline(char **ps, char *es)
{
  struct cmd *cmd;

  cmd = parsepipe(ps, es);
  while(peek(ps, es, "&")){
    gettoken(ps, es, NULL);
    cmd = backcmd(cmd);
  }

  if (peek(ps, es, "#")) {
    gettoken(ps, es, NULL);
    *ps = es;
    return cmd;
  }

  if(peek(ps, es, ";^")){
    int tok = gettoken(ps, es, NULL);
    cmd = listcmd(tok, cmd, parseline(ps, es));
  }
  return cmd;
}

struct cmd*
parsepipe(char **ps, char *es)
{
  struct cmd *cmd;

  cmd = parseexec(ps, es);
  if(peek(ps, es, "|")){
    gettoken(ps, es, NULL);
    cmd = pipecmd(cmd, parsepipe(ps, es));
  }
  return cmd;
}

struct cmd*
parseredirs(struct cmd *cmd, char **ps, char *es)
{
  int tok;
  char *q;
  int mode;
  int fd;

  while(peek(ps, es, "<>+")){
    tok = gettoken(ps, es, NULL);
    if(gettoken(ps, es, &q) != 'a')
      panic("missing file for redirection");
    switch(tok){
    default:
    case '<':
      mode = O_RDONLY;
      fd = STDIN_FILENO;
      break;
    case '>':
      mode = O_WRONLY | O_CREAT | O_TRUNC;
      fd = STDOUT_FILENO;
      break;
    case '+':  // >>
      mode = O_WRONLY | O_CREAT| O_APPEND;
      fd = STDOUT_FILENO;
      break;
    }
    cmd = redircmd(cmd, q, mode, fd);
  }
  return cmd;
}

struct cmd*
parseblock(char **ps, char *es)
{
  struct cmd *cmd;

  if(!peek(ps, es, "("))
    panic("parseblock");
  gettoken(ps, es, NULL);
  cmd = parseline(ps, es);
  if(!peek(ps, es, ")"))
    panic("syntax - missing )");
  gettoken(ps, es, NULL);
  cmd = parseredirs(cmd, ps, es);
  return cmd;
}

struct cmd*
parseexec(char **ps, char *es)
{
  char *q;
  int tok, argc;
  struct execcmd *cmd;
  struct cmd *ret;

  if(peek(ps, es, "("))
    return parseblock(ps, es);

  ret = execcmd();
  cmd = (struct execcmd*)ret;

  argc = 0;
  ret = parseredirs(ret, ps, es);
  while(!peek(ps, es, "|)&;^#")){
    if((tok=gettoken(ps, es, &q)) == 0)
      break;
    if(tok != 'a')
      panic("syntax");
    cmd->argv[argc] = q;
    argc++;
    if(argc >= MAXARGS)
      panic("too many args");
    ret = parseredirs(ret, ps, es);
  }
  cmd->argv[argc] = NULL;
  return ret;
}
