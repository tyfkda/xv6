// Test that fork fails gracefully.
// Tiny executable so that the limit can be filling the proc table.

#include "stdio.h"
#include "stdlib.h"
#include "unistd.h"

#define N  1000

void
forktest(void)
{
  int n, pid;

  printf("fork test\n");

  for(n=0; n<N; n++){
    pid = fork();
    if(pid < 0)
      break;
    if(pid == 0)
      exit(EXIT_SUCCESS);
  }

  if(n == N){
    fprintf(stderr, "fork claimed to work N times!\n", N);
    exit(EXIT_FAILURE);
  }

  for(; n > 0; n--){
    if(wait(0) < 0){
      fprintf(stderr, "wait stopped early\n");
      exit(EXIT_FAILURE);
    }
  }

  if(wait(0) != -1){
    fprintf(stderr, "wait got too many\n");
    exit(EXIT_FAILURE);
  }

  printf("fork test OK\n");
}

int
main(void)
{
  forktest();
  return EXIT_SUCCESS;
}
