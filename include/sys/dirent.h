/* <dirent.h> includes <sys/dirent.h>, which is this file.  On a
   system which supports <dirent.h>, this file is overridden by
   dirent.h in the libc/sys/.../sys directory.  On a system which does
   not support <dirent.h>, we will get this file which uses #error to force
   an error.  */

#ifdef __cplusplus
extern "C" {
#endif
  //#error "<dirent.h> not supported"

  typedef struct __dirstream DIR;

  // Directory is a file containing a sequence of dirent structures.
  #define DIRSIZ 14

  struct dirent {
    unsigned short d_ino;
    char d_name[DIRSIZ];
  };

  DIR* opendir(const char *path);
  DIR* fdopendir(int fd);
  int closedir(DIR*);
  struct dirent *readdir(DIR*);

#ifdef __cplusplus
}
#endif
