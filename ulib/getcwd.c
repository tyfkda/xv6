#include "dirent.h"
#include "fcntl.h"
#include "sys/stat.h"
#include "stdio.h"
#include "stdlib.h"  // malloc
#include "string.h"
#include "unistd.h"

#define FALSE  (0)
#define TRUE   (1)

#define PATH_SEPARATOR   "/"

#ifndef DIRSIZ
#define DIRSIZ  (16)
#endif

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

char *getcwd(char *resultPath, size_t size) {
  if (resultPath == NULL) {
    resultPath = malloc(512);  // TODO: Fix
    if (resultPath == NULL)
      return NULL;
  }

  resultPath[0] = '\0';

  char ancestorPath[512];
  strcpy(ancestorPath, ".");

  struct stat st;
  if (stat(ancestorPath, &st) < 0)
    return NULL;

  char* p = goUp(st.st_ino, ancestorPath, resultPath);
  if (p == NULL)
    return NULL;
  if (resultPath[0] == '\0')
    strcpy(resultPath, PATH_SEPARATOR);
  return resultPath;
}
