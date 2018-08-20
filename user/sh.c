// Shell.

#include "ctype.h"
#include "fcntl.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "unistd.h"

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
    if (var == NULL)
      var = " ";
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

  exec(ecmd->argv[0], ecmd->argv);
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
    exit(ec1 ? ec1 : ec2 ? ec2 : 0);
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
getcmd(FILE* fp, char *buf, int nbuf)
{
  memset(buf, 0, nbuf);
  if (fgets_s(buf, nbuf, fp) == 0)  // EOF
    return -1;
  return 0;
}

void
sh(FILE* fp, int tty)
{
  char buf[100];
  int exitcode;

  // Read and run input commands.
  for (;;) {
    if (tty)
      fprintf(stderr, "$ ");
    if (getcmd(fp, buf, sizeof(buf)) < 0)
      break;
    if(strncmp(buf, "cd ", 3) == 0){
      // Chdir must be called by the parent, not the child.
      buf[strlen(buf)-1] = '\0';  // chop \n
      if(chdir(buf+3) < 0)
        fprintf(stderr, "cannot cd %s\n", buf+3);
      continue;
    }
    if(fork1() == 0)
      runcmd(parsecmd(buf));
    wait(&exitcode);
    putLastExitCode(exitcode);
    if (tty)
      fprintf(stderr, "(exitcode = %d)\n", exitcode);
  }
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
    sh(stdin, TRUE);
  } else {
    for (int i = 1; i < argc; ++i) {
      FILE* fp = fopen(argv[i], "r");
      if (fp == NULL) {
        char buf[128];
        sprintf(buf, "Cannot open: %s", argv[i]);
        panic(buf);
      }
      sh(fp, FALSE);
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

  cmd = calloc(sizeof(*cmd));
  cmd->type = EXEC;
  return (struct cmd*)cmd;
}

struct cmd*
redircmd(struct cmd *subcmd, char *file, int mode, int fd)
{
  struct redircmd *cmd;

  cmd = calloc(sizeof(*cmd));
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

  cmd = calloc(sizeof(*cmd));
  cmd->type = PIPE;
  cmd->left = left;
  cmd->right = right;
  return (struct cmd*)cmd;
}

struct cmd*
listcmd(int cond, struct cmd *left, struct cmd *right)
{
  struct listcmd *cmd;

  cmd = calloc(sizeof(*cmd));
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

  cmd = calloc(sizeof(*cmd));
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
