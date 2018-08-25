#include "stdio.h"
#include "unistd.h"
#include "stdlib.h"
#include "string.h"
#include "errno.h"
#include "../kernel/param.h"

#define PATH_ENV_VAR "PATH"
#define ENV_VAR_LEN   (128)
#define ENV_VAR_DELIM   '='

extern int _path_update(const char *);

typedef struct __evar_list{
  struct __evar_list *prev;
  struct __evar_list *next;
}_evar_list;

typedef struct __evar_head{
  struct __evar_list *prev;
  struct __evar_list *next;
}_evar_head;

typedef struct _evar_type{
  _evar_list link;
  char       *var;
  char     *namep;
  char      *valp;
}evar_type;

static evar_type evar_array[MAXENV];
static _evar_head evars={.prev = (struct __evar_list *)&evars, .next = (struct __evar_list *)&evars};

static int
env_add_from_environ(const char *var, int overwrite){
  int        i;
  evar_type *p;
  int      len;

  for(i=0; MAXENV>i; ++i) {

    p = &evar_array[i];
		
    if ( p->var != NULL ) {

      len = strlen(p->namep);
      if ( ( strncmp(p->namep, var,  len) != 0 ) ||
           ( var[len] != ENV_VAR_DELIM ) )
        continue;
      if ( !overwrite )
        return EAGAIN;
      free( p->var );  /* Replace the variable */
    }
    p->var = strdup(var);

    if ( p->var == NULL ) {
			
      p->namep = NULL;
      p->valp = NULL;

      /*
       * unlink
       */
      (p->link.prev)->next = p->link.next;
      (p->link.next)->prev = p->link.prev;
      p->link.prev = p->link.next = &(p->link);
			
      return ENOMEM;
    }
    goto add_var;
  }

  return ENOENT;

add_var:
  len = strlen(p->var);
	
  p->namep = p->var;
  p->valp = strchr(p->var, '=');
  if ( p->valp == NULL ) 
    p->valp = &p->var[len];
  else {

    *p->valp = '\0';
    ++p->valp;
  }
	
  /*
   * link
   */
  p->link.prev = evars.prev;
  p->link.next = (struct __evar_list *)&evars;
  evars.prev->next = &p->link;
  evars.prev = &p->link;
	
  return 0;
}

static int
env_add(const char *name, const char *val, int overwrite) {
  char env_ent[ENV_VAR_LEN];
  
  snprintf(env_ent, ENV_VAR_LEN, "%s=%s", name, val);
  return env_add_from_environ(env_ent, overwrite);
}

static int
env_del(const char *name) {
  int        i;
  evar_type *p;
        
  for(i=0; MAXENV>i; ++i) {

    p = &evar_array[i];
    if ( strcmp(p->namep, name) == 0 ) {
			
      p->namep = NULL;
      p->valp = NULL;
      if ( p->var != NULL )
        free(p->var);
      p->var = NULL;

      /*
       * unlink
       */
      (p->link.prev)->next = p->link.next;
      (p->link.next)->prev = p->link.prev;
      p->link.prev = p->link.next = &(p->link);

      return 0;
    }
  }

  return ENOENT;
}

static int
env_find_by_name(const char *name, char **valp) {
  evar_type *p;

  for(p = (evar_type *)(evars.next);
      p != (evar_type *)(&evars);
      p = (evar_type *)p->link.next) {

    if ( strcmp(p->namep, name) == 0 ) {

      *valp = p->valp;
      return 0;
    }
  }

  return ENOENT;
}


int
_env_find_by_idx(int idx, char **namep, char **valp) {
  int        i;
  evar_type *p;

  for(i = 0, p = (evar_type *)(evars.next);
      ( MAXENV > i) && ( p != (evar_type *)(&evars) );
      p = (evar_type *)p->link.next, ++i) {

    if ( i == idx ) {
			
      *namep = p->namep;
      *valp = p->valp;
      return 0;
    }
  }

  return ENOENT;
}

void
env_init(void) {
  int i;
	
  for(i=0; MAXENV>i; ++i) {

    memset(&evar_array[i], 0, sizeof(evar_type));
    evar_array[i].link.prev = evar_array[i].link.next = &(evar_array[i].link);
  }
}

void
_setup_environment(char *envs[]){
  int i, rc;
  char *val;

  env_init();

  for(i = 0; envs[i] != NULL; ++i) {

    env_add_from_environ(envs[i], 1);
  }

  rc = env_find_by_name(PATH_ENV_VAR, &val);
  if ( rc == 0 )
    _path_update(val);

  return;
}

char *
getenv(const char *name) {
  int    rc;
  char *val;

  if ( name == NULL )
    return 0;

  rc = env_find_by_name(name, &val);
  if ( rc != 0 )
    return 0;

  return val;
}

int
setenv(const char *name, const char *value, int overwrite) {
  int   rc;

  if ( ( name == NULL ) || ( value == NULL ) )
    return -1;

  rc = env_add(name, value, overwrite);
  if ( rc != 0 )
    return -1;
  return 0;
}

int 
unsetenv(const char *name) {
  int rc;

  if ( name == NULL )
    return -1;
	
  rc = env_del(name);
  if ( rc != 0 )
    return -1;

  return 0;
}
