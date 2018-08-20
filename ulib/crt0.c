//#define DEBUG_SHOW_EXECVE_ENVS

#include "stdlib.h"

#if defined(DEBUG_SHOW_EXECVE_ENVS)
#include "fcntl.h"
#include "stdio.h"
#include "unistd.h"
#endif  /*  DEBUG_SHOW_EXECVE_ENVS  */

#define PATH_ENV_VAR "PATH"

char **environ={0};

extern void env_init(void);
extern int env_find_by_name(const char *, char **);
extern int env_add_from_environ(const char *, int );
extern int path_update(const char *);

extern int main(int argc, char** argv);

static void
setup_environment(char *envs[]){
	int i, rc;
	char *val;

	env_init();

	for(i = 0; envs[i] != 0; ++i) {

          env_add_from_environ(envs[i], 1);
	}

	rc = env_find_by_name(PATH_ENV_VAR, &val);
	if ( rc == 0 )
		path_update(val);

	return;
}

void _start(int argc, char** argv, char **envp) {
#if defined(DEBUG_SHOW_EXECVE_ENVS)
  int fd;
#endif  /*  DEBUG_SHOW_EXECVE_ENVS  */

#if defined(DEBUG_SHOW_EXECVE_ENVS)
  // Open standard input, output and error in advance.
  while((fd = open("console", O_RDWR)) >= 0){
    if(fd >= 3){
      close(fd);
      break;
    }
  }
#endif  /*  DEBUG_SHOW_EXECVE_ENVS  */

  environ = envp;

#if defined(DEBUG_SHOW_EXECVE_ENVS)
  fprintf(stderr, "crt0: envp: %p environ: %p\n", envp, environ);
#endif  /*  DEBUG_SHOW_EXECVE_ENVS  */
  setup_environment(envp);

  int code = main(argc, argv);
  exit(code);
}
