#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "unistd.h"
#include "errno.h"
#include "../kernel/param.h"

#define ENV_VAR_LEN    (128)
#define MAX_PATH_NR    (32)
#define PATH_MAX       (128)
#define PATH_SEPARATOR ':'
#define FILE_SEPARATOR '/'

typedef struct _path_store{
  char *path_str;
  char *path[MAX_PATH_NR];  /* Note: path[MAX_PATH_NR - 1] should be NULL */
}path_store;

int _env_find_by_idx(int _idx, char **_namep, char **_valp);

static path_store g_path={0, {0}};

static char*
skip_path_delim(char *val, char **pathp){
  char *s;
  int len;
  
  while(*val == PATH_SEPARATOR)
    ++val;
  if( *val == '\0' )
    return 0;
  
  s = val;
  while( (*val != PATH_SEPARATOR) && (*val != '\0') )
    ++val;
  
  len = val - s;
  if ( PATH_MAX <= len ) 
    len = PATH_MAX - 1;
  s[len] = '\0';
  
  *pathp = s;
  
  ++val;	
  while( *val == PATH_SEPARATOR )
    ++val;
  
  return (char *)val;
}

const char *
path_refer(int idx) {

  if ( ( 0 > idx ) || ( MAX_PATH_NR <= idx ) )
    return 0;
  
  return g_path.path[idx];
}

int
_path_update(const char *path_val) {
  int    i, rc;
  char *new_path;
  char *name, *next;
  
  new_path = strdup(path_val);
  if ( new_path == NULL ) {
    
    rc = ENOMEM;
    goto free_out;
  }
  
  if ( g_path.path_str != NULL ) 
    free(g_path.path_str);
  for( i = 0; MAX_PATH_NR > i; ++i) 
    g_path.path[i] = NULL;
  g_path.path_str = new_path;
  
  i = 0;
  next = skip_path_delim(new_path, &name);
  while( ( next != NULL ) && ( *name != '\0' ) ) {
    
    g_path.path[i] = name;
    next = skip_path_delim(next, &name);
    ++i;
    if ( i == ( MAX_PATH_NR - 1 ) )
      break;
  }
  g_path.path[i] = NULL;
  
  return 0;
  
 free_out:
  free( new_path );
  return rc;
}

int 
exec(const char *path, char *const argv[]){
  char      cmd[PATH_MAX];
  char      *envs[MAXENV];
  char  estr[ENV_VAR_LEN];
  const char         *dir;
  char        *name, *val;
  int               i, rc;
  char                *ch;

  memset(&envs[0], 0, sizeof(envs));
  for(i = 0; ( MAXENV - 1 ) > i; ++i) {
    
    rc = _env_find_by_idx(i, &name, &val);
    if ( rc != 0 )
      break;
    snprintf(estr, ENV_VAR_LEN, "%s=%s", name, val);
    envs[i] = strdup(estr);
    if ( envs[i] == NULL ) 
      goto free_env_out;
  }
  envs[i] = NULL;

  ch = strchr(path, FILE_SEPARATOR);
  if ( ch == NULL ) {

    /*
     * First, we try to execute a binary according to PATH environment variable
     * when path does not contain any slash.
     */
    dir = path_refer(0);
    for(i = 1; dir != NULL; ++i) {

      snprintf(cmd, PATH_MAX, "%s/%s", dir, path);
      rc = execve(cmd, argv, envs);
      dir = path_refer(i);
    }
  } else {

    /*
     * Second, we try to execute a binary with relative path and absolute path
     * ( e.g.,  ./path/a.out, /path/a.out and a.out/ ).
     */
    rc = execve(path, argv, envs);
  }

free_env_out:
  for(i = 0; MAXENV > i; ++i) {
    
    if ( envs[i] != NULL ) {
      
      free(envs[i]);
      envs[i] = NULL;
    }
  }
  
  return rc;
}
