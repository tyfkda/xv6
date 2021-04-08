#define __NO_FLONUM
 #define __NO_ELF_OBJ

#if defined(__linux__)
// To use kill on Linux,, include signal.h with `_POSIX_SOURCE` declaration.
#define _POSIX_SOURCE  // To use `kill`
#endif

#include <assert.h>
#include <fcntl.h>  // open
#include <libgen.h>  // dirname
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "util.h"

#if !defined(__XV6) && !defined(__linux__)

#define AS_USE_CC

#endif

static char *get_ext(const char *filename) {
  const char *last_slash = strrchr(filename, '/');
  if (last_slash == NULL)
    last_slash = filename;
  char *dot = strrchr(last_slash, '.');
  return dot != NULL ? (char*)&dot[1]: (char*)&last_slash[strlen(last_slash)];
}

static pid_t fork1(void) {
  pid_t pid = fork();
  if (pid < 0)
    error("fork failed");
  return pid;
}

static int wait_process(pid_t pid) {
  int ec = -1;
  if (waitpid(pid, &ec, 0) < 0)
    error("wait failed");
  return ec;
}

static pid_t wait_child(int *result) {
  *result = -1;
  return waitpid(0, result, 0);
}

// command > ofd
static pid_t exec_with_ofd(char **command, int ofd) {
  pid_t pid = fork1();
  if (pid == 0) {
    if (ofd >= 0 && ofd != STDOUT_FILENO) {
      close(STDOUT_FILENO);
      if (dup(ofd) == -1)
        error("dup failed");
    }

    if (execvp(command[0], command) < 0) {
      perror(command[0]);
      exit(1);
    }
  }
  return pid;
}

// | command > ofd
pid_t pipe_exec(char **command, int ofd, int fd[2]) {
  if (pipe(fd) < 0)
    error("pipe failed");

  pid_t pid = fork1();
  if (pid == 0) {
    close(STDIN_FILENO);
    if (dup(fd[0]) == -1)
      error("dup failed");

    if (ofd >= 0 && ofd != STDOUT_FILENO) {
      close(STDOUT_FILENO);
      if (dup(ofd) == -1)
        error("dup failed");
    }

    close(fd[0]);
    close(fd[1]);
    if (execvp(command[0], command) < 0) {
      perror(command[0]);
      exit(1);
    }
  }
  return pid;
}

static int cat(const char *filename, int ofd) {
  int ifd = open(filename, O_RDONLY);
  if (ifd < 0)
    return 1;

  const int SIZE = 4096;
  char *buf = malloc(SIZE);
  for (;;) {
    ssize_t size = read(ifd, buf, SIZE);
    if (size < 0)
      return 1;
    if (size > 0) {
      ssize_t wsize = write(ofd, buf, size);
      if (wsize != size)
        error("Write failed");
    }
    if (size < SIZE)
      break;
  }
  free(buf);

  close(ifd);
  return 0;
}

static void create_local_label_prefix_option(int index, char *out, size_t n) {
  static const char LOCAL_LABEL_PREFIX[] = "--local-label-prefix=";
  static const char DIGITS[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
  char prefix[8];
  char *p = &prefix[sizeof(prefix) - 1];
  *p-- = '\0';
  assert(index >= 0);
  if (index > 0) {
    --index;
    do {
      if (p <= prefix)
        error("Label prefix buffer overflow");
      *p-- = DIGITS[index % (sizeof(DIGITS) - 1)];
      index /= sizeof(DIGITS) - 1;
    } while (index > 0);
  }
  *p = 'L';

  snprintf(out, n, "%s%s", LOCAL_LABEL_PREFIX, p);
}

static int compile(const char *src, Vector *cpp_cmd, Vector *cc1_cmd, int ofd) {
  int ofd2 = ofd;
  int cc_fd[2];
  pid_t cc_pid = -1;
  int running = 0;
  if (cc1_cmd != NULL) {
    cc_pid = pipe_exec((char**)cc1_cmd->data, ofd, cc_fd);
    ofd2 = cc_fd[1];
    ++running;
  }

  cpp_cmd->data[cpp_cmd->len - 2] = (void*)src;
  pid_t cpp_pid = exec_with_ofd((char**)cpp_cmd->data, ofd2);
  ++running;

  int res = 0;
  for (; running > 0; --running) {
    int r = 0;
    pid_t done = wait_child(&r);  // cpp or cc1
    if (done > 0) {
      res |= r;
      if (done == cpp_pid) {
        cpp_pid = -1;
        if (cc_pid != -1) {
          if (r != 0) {
            // Illegal: cpp exit with failure.
#if !defined(__XV6)
            kill(cc_pid, SIGKILL);
#endif
            break;
          }
          close(cc_fd[0]);
          close(cc_fd[1]);
        }
      } else if (done == cc_pid) {
        cc_pid = -1;
        if (cpp_pid != -1) {
          // Illegal: cc dies earlier than cpp.
#if !defined(__XV6)
          kill(cpp_pid, SIGKILL);
#endif
          break;
        }
      }
    } else {
      res |= 1;
    }
  }
  return res;
}

void usage(FILE *fp) {
  fprintf(
      fp,
      "Usage: xcc [options] file...\n"
      "Options:\n"
      "  -I<path>            Add include path\n"
      "  -D<label[=value]>   Define label\n"
      "  -o<filename>        Set output filename (Default: a.out)\n"
      "  -c                  Output object file\n"
      "  -S                  Output assembly code\n"
      "  -E                  Output preprocess result\n"
  );
}

int main(int argc, char *argv[]) {
  const char *ofn = NULL;
  bool out_pp = false;
  bool out_obj = false;
  bool out_asm = false;
  bool run_asm = true;
  int iarg;

  const char *root = dirname(strdup_(argv[0]));
  char *cpp_path = cat_path(root, "cpp");
  char *cc1_path = cat_path(root, "cc1");
#if !defined(AS_USE_CC)
  char *as_path = cat_path(root, "as");
#endif

  Vector *cpp_cmd = new_vector();
  vec_push(cpp_cmd, cpp_path);

  Vector *cc1_cmd = new_vector();
  vec_push(cc1_cmd, cc1_path);

  Vector *as_cmd = new_vector();
#if !defined(AS_USE_CC)
  vec_push(as_cmd, as_path);
#else
  vec_push(as_cmd, "cc");
  vec_push(as_cmd, "-x");
  vec_push(as_cmd, "assembler");
#endif

  for (iarg = 1; iarg < argc; ++iarg) {
    char *arg = argv[iarg];
    if (*arg != '-')
      break;

    if (starts_with(arg, "-I") || starts_with(arg, "-D")) {
      vec_push(cpp_cmd, arg);
    } else if (starts_with(arg, "-c")) {
      out_obj = true;
      vec_push(as_cmd, arg);
    } else if (starts_with(arg, "-o")) {
      ofn = arg + 2;
      vec_push(as_cmd, arg);
    } else if (strcmp(arg, "-E") == 0) {
      out_pp = true;
      run_asm = false;
    } else if (strcmp(arg, "-S") == 0) {
      out_asm = true;
      run_asm = false;
    } else if (strcmp(arg, "--dump-ir") == 0) {
      run_asm = false;
      vec_push(cc1_cmd, arg);
    } else if (starts_with(arg, "--local-label-prefix")) {
      vec_push(cc1_cmd, arg);
    } else if (strcmp(arg, "--help") == 0) {
      usage(stdout);
      return 0;
    } else if (strcmp(arg, "--version") == 0) {
      show_version("xcc");
      return 0;
    } else {
      fprintf(stderr, "Unknown option: %s\n", arg);
      return 1;
    }
  }

  if (iarg >= argc) {
    fprintf(stderr, "No input files\n\n");
    usage(stderr);
    return 1;
  }

  if (ofn == NULL) {
    if (out_obj) {
      if (iarg < argc)
        ofn = change_ext(basename(argv[iarg]), "o");
      else
        ofn = "a.o";
    } else if (out_asm) {
      if (iarg < argc)
        ofn = change_ext(basename(argv[iarg]), "s");
      else
        ofn = "a.s";
    } else {
      ofn = "a.out";
    }

    StringBuffer sb;
    sb_init(&sb);
    sb_append(&sb, "-o", NULL);
    sb_append(&sb, ofn, NULL);
    vec_push(as_cmd, sb_to_string(&sb));
  }

  vec_push(cpp_cmd, NULL);  // Buffer for src.
  vec_push(cpp_cmd, NULL);  // Terminator.
  vec_push(cc1_cmd, NULL);  // Buffer for label prefix.
  vec_push(cc1_cmd, NULL);  // Terminator.
#if defined(AS_USE_CC)
  vec_push(as_cmd, "-");
#endif
  vec_push(as_cmd, NULL);  // Terminator.

  int ofd = STDOUT_FILENO;
  int as_fd[2];
  pid_t as_pid = -1;

  if (run_asm) {
    as_pid = pipe_exec((char**)as_cmd->data, -1, as_fd);
    ofd = as_fd[1];
  } else if (out_asm) {
#if !defined(__XCC) && !defined(__XV6)
    close(STDOUT_FILENO);
    ofd = open(ofn, O_WRONLY | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
    if (ofd == -1) {
      perror("Failed to open output file");
      exit(1);
    }
#endif
  }

  int res = 0;
  if (iarg < argc) {
    for (int i = iarg; i < argc; ++i) {
      char *src = argv[i];
      char *ext = get_ext(src);
      if (strcasecmp(ext, "c") == 0) {
        char prefix_option[32];
        create_local_label_prefix_option(i - iarg, prefix_option, sizeof(prefix_option));
        cc1_cmd->data[cc1_cmd->len - 2] = prefix_option;

        res = compile(src, cpp_cmd, out_pp ? NULL : cc1_cmd, ofd);
      } else if (strcasecmp(ext, "s") == 0) {
        res = cat(src, ofd);
      } else {
        fprintf(stderr, "Unsupported file type: %s\n", src);
        res = -1;
      }
      if (res != 0)
        break;
    }
  } else {
    // cpp is read from stdin.
    res = compile(NULL, cpp_cmd, out_pp ? NULL : cc1_cmd, ofd);
  }

  if (res != 0 && as_pid != -1) {
#if !defined(__XV6)
    kill(as_pid, SIGKILL);
    remove(ofn);
#else
    (void)ofn;
#endif
    close(as_fd[0]);
    close(as_fd[1]);
    as_pid = -1;
  }

  if (as_pid != -1) {
    close(as_fd[0]);
    close(as_fd[1]);

    res = wait_process(as_pid);
  }
  return res == 0 ? 0 : 1;
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
