//
// File descriptors
//

#include "file.h"
#include "types.h"
#include "defs.h"
#include "param.h"
#include "fs.h"
#include "spinlock.h"
#include "sleeplock.h"
#include "sys/stat.h"

struct {
  struct spinlock lock;
  //struct file file[NFILE];
} ftable;

void
fileinit(void)
{
  initlock(&ftable.lock, "ftable");
}

// Allocate a file structure.
struct file*
filealloc(void)
{
  return 0;
}

// Increment ref count for file f.
struct file*
filedup(struct file *f)
{
  return f;
}

// Close file f.  (Decrement ref count, close when reaches 0.)
void
fileclose(struct file *f, int error)
{
}

// Get metadata about file f.
int
filestat(struct file *f, struct stat *st)
{
  return -1;
}

// Read from file f.
int
fileread(struct file *f, void *addr, int n)
{
  panic("fileread");
}

// Read directory from file f.
int
filereaddir(struct file *f, void *addr)
{
  panic("filereaddir");
}

//PAGEBREAK!
// Write to file f.
int
filewrite(struct file *f, void *addr, int n)
{
  panic("filewrite");
}

long
filelseek(struct file *f, long offset, int whence)
{
  return -1;
}

int
filetruncate(struct file* f, uint length)
{
  return -1;
}
