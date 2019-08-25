#include <assert.h>
#include <fcntl.h>  // open
#include <libgen.h>  // dirname
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "util.h"

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

// cmd1 | cmd2 > dst_fd
static int pipe_command(char *const *cmd1, char *const *cmd2, int dst_fd) {
  int fd[2];
  if (pipe(fd) < 0)
    error("pipe failed");
  pid_t pid1 = fork1();
  if (pid1 == 0) {
    close(STDOUT_FILENO);
    if (dup(fd[1]) == -1)
      error("dup failed");
    close(fd[0]);
    close(fd[1]);
    if (execvp(cmd1[0], cmd1) < 0) {
      perror(cmd1[0]);
      exit(1);
    }
  }
  pid_t pid2 = fork1();
  if (pid2 == 0) {
    close(STDIN_FILENO);
    if (dup(fd[0]) == -1)
      error("dup failed");
    if (dst_fd >= 0) {
      close(STDOUT_FILENO);
      if (dup(dst_fd) == -1)
        error("dup failed");
    }
    close(fd[0]);
    close(fd[1]);
    if (execvp(cmd2[0], cmd2) < 0) {
      perror(cmd2[0]);
      exit(1);
    }
  }
  close(fd[0]);
  close(fd[1]);

  int ec1 = -1, ec2 = -1;
  int r1 = waitpid(pid1, &ec1, 0);
  int r2 = waitpid(pid2, &ec2, 0);
  if (r1 < 0 || r2 < 0)
    error("wait failed");
  return ec1 != 0 ? ec1 : ec2;
}

static int cat(const char *filename, int dst_fd) {
  int ifd = open(filename, O_RDONLY);
  if (ifd < 0)
    return 1;

  const int SIZE = 4096;
  char *buf = malloc(SIZE);
  for (;;) {
    ssize_t size = read(ifd, buf, SIZE);
    if (size < 0)
      return 1;
    if (size > 0)
      write(dst_fd, buf, size);
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

static int compile(const char *src, int index, Vector *cpp_cmd, char *cc1_path, int dst_fd) {
  char prefix_option[32];
  create_local_label_prefix_option(index, prefix_option, sizeof(prefix_option));

  cpp_cmd->data[cpp_cmd->len - 2] = (void*)src;
  char *const cc1_cmd[] = {cc1_path, prefix_option, NULL};
  return pipe_command((char**)cpp_cmd->data, cc1_cmd, dst_fd);
}

int main(int argc, char* argv[]) {
  const char *ofn = "a.out";
  bool out_asm = false;
  int iarg;

  char *cpp_path = cat_path(dirname(strdup_(argv[0])), "cpp");
  char *cc1_path = cat_path(dirname(strdup_(argv[0])), "cc1");
  char *as_path = cat_path(dirname(strdup_(argv[0])), "as");

  Vector *cpp_cmd = new_vector();
  vec_push(cpp_cmd, cpp_path);

  Vector *as_cmd = new_vector();
  vec_push(as_cmd, as_path);

  for (iarg = 1; iarg < argc; ++iarg) {
    if (*argv[iarg] != '-')
      break;
    if (strncmp(argv[iarg], "-I", 2) == 0)
      vec_push(cpp_cmd, argv[iarg]);
    if (strncmp(argv[iarg], "-o", 2) == 0) {
      ofn = argv[iarg] + 2;
      vec_push(as_cmd, argv[iarg]);
    }
    if (strncmp(argv[iarg], "-S", 2) == 0)
      out_asm = true;
  }

  vec_push(cpp_cmd, NULL);  // Buffer for src.
  vec_push(cpp_cmd, NULL);  // Terminator.

  vec_push(as_cmd, NULL);  // Terminator.

  int fd[2];
  pid_t pid = -1;

  if (!out_asm) {
    // Pipe for as.
    if (pipe(fd) < 0)
      error("pipe failed");

    // as
    pid = fork1();
    if (pid == 0) {
      close(STDIN_FILENO);
      if (dup(fd[0]) == -1)
        error("dup failed");
      close(fd[0]);
      close(fd[1]);
      if (execvp(as_cmd->data[0], (char**)as_cmd->data) < 0) {
        perror(as_cmd->data[0]);
        exit(1);
      }
    }
  }

  int dst_fd = out_asm ? -1 : fd[1];
  int res;
  if (iarg < argc) {
    for (int i = iarg; i < argc; ++i) {
      char *src = argv[i];
      char *ext = get_ext(src);
      if (strcasecmp(ext, "c") == 0) {
        res = compile(src, i - iarg, cpp_cmd, cc1_path, dst_fd);
      } else if (strcasecmp(ext, "s") == 0) {
        res = cat(src, dst_fd);
      } else {
        fprintf(stderr, "Unsupported file type: %s\n", src);
        res = -1;
      }
      if (res != 0)
        break;
    }
  } else {
    // cpp is read from stdin.
    char *const cc1_cmd[] = {cc1_path, NULL};
    res = pipe_command((char**)cpp_cmd->data, cc1_cmd, dst_fd);
  }

  if (res != 0 && !out_asm) {
    assert(pid != -1);
#if !defined(__XV6)
    kill(pid, SIGKILL);
    remove(ofn);
#else
    (void)ofn;
#endif
  }

  if (!out_asm) {
    close(fd[0]);
    close(fd[1]);

    int ec;
    int r1 = waitpid(pid, &ec, 0);
    if (r1 < 0 || ec != 0)
      return 1;
  }
  return res == 0 ? 0 : 1;
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
