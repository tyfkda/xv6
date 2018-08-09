#include "dirent.h"
#include "fcntl.h"
#include "sys/stat.h"
#include "stdio.h"
#include "string.h"
#include "unistd.h"

#define FALSE  (0)
#define TRUE   (1)

#define PATH_SEPARATOR   "/"

#ifndef DIRSIZ
#define DIRSIZ  (16)
#endif

static int mygetcwd(char* resultPath);
static char* goUp(ino_t ino, char* ancestorPath, char* resultPath);
static int dirlookup(DIR *dir, ino_t ino, char* p);

int main(int argc, char *argv[]) {
  char resultPath[512];
  if (!mygetcwd(resultPath)) {
    fprintf(stderr, "pwd failed\n");
    return 1;
  }

  printf("%s\n", resultPath);
  return 0;
}

static int mygetcwd(char* resultPath) {
  resultPath[0] = '\0';

  char ancestorPath[512];
  strcpy(ancestorPath, ".");

  struct stat st;
  if (stat(ancestorPath, &st) < 0)
    return FALSE;

  char* p = goUp(st.st_ino, ancestorPath, resultPath);
  if (p == NULL)
    return FALSE;
  if (resultPath[0] == '\0')
    strcpy(resultPath, PATH_SEPARATOR);
  return TRUE;
}

static char* goUp(ino_t ino, char* ancestorPath, char* resultPath) {
  strcpy(ancestorPath + strlen(ancestorPath), PATH_SEPARATOR "..");
  struct stat st;
  if (stat(ancestorPath, &st) < 0 || st.st_ino == ino) {
    // No parent directory exists: must be the root.
    return resultPath;
  }

  char* foundPath = NULL;
  DIR *dir = opendir(ancestorPath);
  if (dir != NULL) {
    char* p = goUp(st.st_ino, ancestorPath, resultPath);
    if (p != NULL) {
      strcpy(p, PATH_SEPARATOR);
      p += sizeof(PATH_SEPARATOR) - 1;

      // Find current directory.
      if (dirlookup(dir, ino, p))
        foundPath = p + strlen(p);
    }
    closedir(dir);
  }
  return foundPath;
}

// @param fd   file descriptor for a directory.
// @param ino  target inode number.
// @param p    [out] file name (part of absPath), overwritten by the file name of the ino.
static int dirlookup(DIR *dir, ino_t ino, char* p) {
  struct dirent* de;
  while ((de = readdir(dir)) != NULL) {
    if (de->d_ino == ino) {
      strcpy(p, de->d_name);
      return TRUE;
    }
  }
  return FALSE;
}
