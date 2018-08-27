#include "errno.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "sys/stat.h"
#include "unistd.h"
#include "../kernel/param.h"

#define PATH_ENV_VAR "PATH"
#define ENV_VAR_LEN    (128)
#define MAX_PATH_NR    (32)
#define PATH_MAX       (128)
#define PATH_SEPARATOR ':'
#define FILE_SEPARATOR '/'

#ifndef FALSE
#define FALSE (0)
#endif
#ifndef TRUE
#define TRUE  (1)
#endif

typedef struct _path_store{
  char *path_str;
  char *path[MAX_PATH_NR];  // Note: path[MAX_PATH_NR - 1] should be '\0'
}path_store;

int _env_find_by_idx(int _idx, char **_namep, char **_valp);

static path_store g_path={0, {0}};

static char*
skip_path_delim(char *val, char **pathp){
  char *s;
  int len;

  while (*val == PATH_SEPARATOR)
    ++val;
  if (*val == '\0')
    return 0;

  s = val;
  while (*val != PATH_SEPARATOR && *val != '\0')
    ++val;

  len = val - s;
  if (len >= PATH_MAX)
    len = PATH_MAX - 1;
  s[len] = '\0';

  *pathp = s;

  ++val;
  while (*val == PATH_SEPARATOR)
    ++val;

  return val;
}

const char *
path_refer(int idx) {
  if (idx < 0 || idx >= MAX_PATH_NR)
    return 0;
  return g_path.path[idx];
}

int
_path_update(const char *path_val) {
  int i, rc;
  char *new_path;
  char *name, *next;

  new_path = strdup(path_val);
  if (new_path == NULL) {
    rc = ENOMEM;
    goto free_out;
  }

  if (g_path.path_str != NULL)
    free(g_path.path_str);
  for (i = 0; i < MAX_PATH_NR; ++i)
    g_path.path[i] = NULL;
  g_path.path_str = new_path;

  i = 0;
  next = skip_path_delim(new_path, &name);
  while (next != NULL && *name != '\0') {
    g_path.path[i] = name;
    next = skip_path_delim(next, &name);
    ++i;
    if (i == MAX_PATH_NR - 1)
      break;
  }
  g_path.path[i] = NULL;

  return 0;

free_out:
  free(new_path);
  return rc;
}

static int findExe(const char* path, char* out) {
  char *envpath = getenv(PATH_ENV_VAR);
  if (envpath != NULL)
    _path_update(envpath);

  const char *dir;
  for (int i = 0; (dir = path_refer(i)) != NULL; ++i) {
    snprintf(out, PATH_MAX, "%s/%s", dir, path);
    struct stat st;
    if (stat(out, &st) == 0)
      return TRUE;
  }
  return FALSE;
}

int
execv(const char *path, char *const argv[]){
  char *envs[MAXENV];
  char estr[ENV_VAR_LEN];
  char *name, *val;
  int i, rc;

  memset(&envs[0], 0, sizeof(envs));
  for (i = 0; i < MAXENV - 1; ++i) {
    rc = _env_find_by_idx(i, &name, &val);
    if (rc != 0)
      break;
    snprintf(estr, ENV_VAR_LEN, "%s=%s", name, val);
    envs[i] = strdup(estr);
    if (envs[i] == NULL) {
      rc = -1;
      goto free_env_out;
    }
  }
  envs[i] = NULL;

  rc = execve(path, argv, envs);
  // If reached here, execve failed.

free_env_out:
  for (i = 0; i < MAXENV; ++i) {
    if (envs[i] != NULL) {
      free(envs[i]);
      envs[i] = NULL;
    }
  }

  return rc;
}

int
execvp(const char *path, char *const argv[]){
  if (strchr(path, FILE_SEPARATOR) != NULL)
    return execv(path, argv);

  char exepath[PATH_MAX];
  if (!findExe(path, exepath))
    return -1;
  return execv(exepath, argv);
}
