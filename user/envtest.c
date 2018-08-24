#include "stdio.h"
#include "../kernel/param.h"

extern int _env_find_by_idx(int , char **, char **);
int
main(int argc, char *argv[])
{
  int i, rc;
  char *name;
  char *val;

  printf("user argv:%p\n", argv);
  for(i = 1; i < argc; i++)
    printf("args: %s%s", argv[i], i+1 < argc ? " " : "\n");
  for(i = 0; i < MAXENV; i++) {

    rc = _env_find_by_idx(i, &name, &val);
    if ( rc == 0 )
      printf("envs: environ[%d]:  %s=%s\n", i, name, val);
  }

  return 0;
}
