#include "fcntl.h"
#include "file_def.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "unistd.h"
#include "errno.h"
#include "../kernel/param.h"

#define ENV_VAR_LEN   (128)
#define MAX_PATH_NR   (32)
#define PATH_MAX      (128)
#define PATH_DELIM    ':'
#define FILE_DELIM    '/'

typedef struct _path_store{
	char *path_str;
	char *path[MAX_PATH_NR];  /* Note: path[MAX_PATH_NR - 1] should be NULL */
}path_store;

int env_find_by_idx(int _idx, char **_namep, char **_valp);

static path_store g_path={0, {0}};

static char*
skip_path_delim(char *val, char **pathp){
  char *s;
  int len, i;
  
  while(*val == PATH_DELIM)
    ++val;
  if( *val == '\0' )
    return 0;
  
  s = val;
  while( (*val != PATH_DELIM) && (*val != '\0') )
    ++val;
  
  while( (*s != '\0') && (*s == FILE_DELIM ) && (*(s + 1) == FILE_DELIM ) )
    ++s;
  
  len = val - s;
  if ( PATH_MAX <= len ) 
    len = PATH_MAX - 1;
  s[len] = '\0';
  if ( (len - 1) != '\0' ) {
    
    for( i = len - 1; i >= 0; --i ){
      if ( s[i] != FILE_DELIM )
	break;
      s[i] = '\0';
    }
  }
  
  *pathp = s;
  
  ++val;	
  while( *val == PATH_DELIM )
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
  if ( new_path == 0 ) {
    
    rc = ENOMEM;
    goto free_out;
  }
  
  if ( g_path.path_str != 0 ) 
    free(g_path.path_str);
  for( i = 0; MAX_PATH_NR > i; ++i) 
    g_path.path[i] = 0;
  g_path.path_str = new_path;
  
  i = 0;
  next = skip_path_delim(new_path, &name);
  while( ( next != 0 ) && ( *name != '\0' ) ) {
    
    g_path.path[i] = name;
    next = skip_path_delim(next, &name);
    ++i;
    if ( i == ( MAX_PATH_NR - 1 ) )
      break;
  }
  g_path.path[i] = 0;
  
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

  memset(&envs[0], 0, sizeof(envs));
  for(i = 0; ( MAXENV - 1 ) > i; ++i) {
    
    rc = env_find_by_idx(i, &name, &val);
    if ( rc != 0 )
      break;
    snprintf(estr, ENV_VAR_LEN, "%s=%s", name, val);
    envs[i] = strdup(estr);
    if ( envs[i] == 0 ) 
      goto free_env_out;
  }
  envs[i] = 0;
  
  /*
   * First, we try to execute a binary with relative path and absolute path
   * ( e.g.,  ./path/a.out, /path/a.out and path/a.out ).
   */
  execve(path, argv, envs);
  
  /*
   * Second, we try to execute a binary according to PATH environment variable.
   */
  dir = path_refer(i);
  for(i = 0; dir != 0; ++i) {
    
    snprintf(cmd, PATH_MAX, "%s/%s", dir, path);
    rc = execve(cmd, argv, envs);
    dir = path_refer(i);
  }
  
 free_env_out:
  for(i = 0; MAXENV > i; ++i) {
    
    if ( envs[i] != 0 ) {
      
      free(envs[i]);
      envs[i] = 0;
    }
  }
  
  return rc;
}
