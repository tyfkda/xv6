#include "fcntl.h"
#include "file_def.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "unistd.h"
#include "errno.h"
#include "../kernel/param.h"

#define ENV_VAR_LEN   (128)
#define ENV_VAR_DELIM   '='

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

int env_add_from_environ(const char *_var, int _overwrite);

static evar_type evar_array[MAXENV];
static _evar_head evars={.prev = (struct __evar_list *)&evars, .next = (struct __evar_list *)&evars};

static int
env_add(const char *name, const char *val, int overwrite) {
	char env_ent[ENV_VAR_LEN];

	snprintf(env_ent, ENV_VAR_LEN, "%s=%s", name, val);
	return env_add_from_environ(env_ent, overwrite);
}
static int
env_del(const char *name) {
	int i;
	evar_type *p;

	for(i=0; MAXENV>i; ++i) {

		p = &evar_array[i];
		if ( strcmp(p->namep, name) == 0 ) {
			
			p->namep = 0;
			p->valp = 0;
			if ( p->var != 0 )
				free(p->var);
			p->var = 0;

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

int
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
env_add_from_environ(const char *var, int overwrite){
	int i;
	evar_type *p;
	int  len;

	for(i=0; MAXENV>i; ++i) {

		p = &evar_array[i];
		
		if ( p->var != 0 ) {

			len = strlen(p->namep);
			if ( ( strncmp(p->namep, var,  len) != 0 ) ||
			     ( var[len] != ENV_VAR_DELIM ) )
				continue;
			if ( !overwrite )
				return EAGAIN;
			free( p->var );  /* Replace the variable */
		}
		p->var = strdup(var);

		if ( p->var == 0 ) {
			
			p->namep = 0;
			p->valp = 0;

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
	if ( p->valp == 0 ) 
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

int
env_find_by_idx(int idx, char **namep, char **valp) {
	int i;
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

char *
getenv(const char *name) {
	int    rc;
	char *val;

	if ( name == 0 )
		return 0;

	rc = env_find_by_name(name, &val);
	if ( rc != 0 )
		return 0;

	return val;
}

int
setenv(const char *name, const char *value, int overwrite) {
	int   rc;

	if ( ( name == 0 ) || ( value == 0 ) )
		return -1;

	rc = env_add(name, value, overwrite);
	if ( rc != 0 )
		return -1;
	return 0;
}

int 
unsetenv(const char *name) {
	int rc;

	if ( name == 0 )
		return -1;
	
	rc = env_del(name);
	if ( rc != 0 )
		return -1;

	return 0;
}
