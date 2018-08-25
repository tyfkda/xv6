#include "sys/wait.h"

int wait(int* pstatus) {
  return waitpid(-1, pstatus, 0);
}
