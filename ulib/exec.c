#include "errno.h"
#include "stdbool.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "sys/stat.h"
#include "unistd.h"
#include "../kernel/param.h"

#define ENV_VAR_LEN    (128)
#define PATH_MAX       (128)
#define PATH_SEPARATOR ':'
#define FILE_SEPARATOR '/'

extern int _env_find_by_idx(int _idx, char **_namep, char **_valp);

static bool find_path(const char* cmd, char* out) {
  char *envpath = getenv("PATH");
  if (envpath == NULL)
    return false;

  for (char *p = envpath; *p != '\0'; ) {
    char *sep = strchr(p, PATH_SEPARATOR);
    if (sep != NULL) {
      int len = sep - p;
      strncpy(out, p, len + 1);
      out[len] = FILE_SEPARATOR;
      strncpy(&out[len + 1], cmd, PATH_MAX - len - 1);
      p = sep + 1;
    } else {
      snprintf(out, PATH_MAX, "%s%c%s", p, FILE_SEPARATOR, cmd);
      p += strlen(p);  // end
    }
    struct stat st;
    if (stat(out, &st) == 0)
      return true;
  }
  return false;
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
      errno = ENOMEM;
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
  if (!find_path(path, exepath)) {
    errno = ENOENT;
    return -1;
  }
  char *newargv[MAXARG];
  memcpy(newargv, argv, sizeof(newargv));
  newargv[0] = exepath;
  return execv(exepath, newargv);
}
