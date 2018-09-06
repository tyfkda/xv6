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

struct devsw devsw[NDEV];
struct {
  struct spinlock lock;
  struct file file[NFILE];
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
  struct file *f;

  acquire(&ftable.lock);
  for(f = ftable.file; f < ftable.file + NFILE; f++){
    if(f->ref == 0){
      f->ref = 1;
      release(&ftable.lock);
      return f;
    }
  }
  release(&ftable.lock);
  return 0;
}

// Increment ref count for file f.
struct file*
filedup(struct file *f)
{
  acquire(&ftable.lock);
  if(f->ref < 1)
    panic("filedup");
  f->ref++;
  release(&ftable.lock);
  return f;
}

// Close file f.  (Decrement ref count, close when reaches 0.)
void
fileclose(struct file *f, int error)
{
  struct file ff;

  acquire(&ftable.lock);
  if(f->ref < 1)
    panic("fileclose");
  if(--f->ref > 0){
    release(&ftable.lock);
    return;
  }
  ff = *f;
  f->ref = 0;
  f->type = FD_NONE;
  release(&ftable.lock);

  if(ff.type == FD_INODE){
    int bUpdate = 0;
    if (ff.writable) {
      // Update mtime.
      uint mtime = cmosepochtime();
      ilock(ff.ip);
      ff.ip->mtime = mtime;
      iunlock(ff.ip);
      bUpdate = 1;
    }

    begin_op();
    if (bUpdate)
      iupdate(ff.ip);
    iput(ff.ip);
    end_op();
  }
}

// Get metadata about file f.
int
filestat(struct file *f, struct stat *st)
{
  if(f->type == FD_INODE){
    ilock(f->ip);
    stati(f->ip, st);
    iunlock(f->ip);
    return 0;
  }
  return -1;
}

// Read from file f.
int
fileread(struct file *f, void *addr, int n)
{
  int r;

  if(f->readable == 0)
    return -1;
  if(f->type == FD_INODE){
    ilock(f->ip);
    if (f->ip->type == T_DIR) {
      r = -1;
    } else {
      if((r = readi(f->ip, addr, f->off, n)) > 0)
        f->off += r;
    }
    iunlock(f->ip);
    return r;
  }
  panic("fileread");
}

// Read directory from file f.
int
filereaddir(struct file *f, void *addr)
{
  int r;

  if(f->readable == 0)
    return -1;
  if(f->type == FD_INODE){
    ilock(f->ip);
    if (f->ip->type == T_DIR) {
      if((r = readi(f->ip, addr, f->off, sizeof(struct dirent))) > 0)
        f->off += r;
    } else {
      r = -1;
    }
    iunlock(f->ip);
    return r;
  }
  panic("filereaddir");
}

//PAGEBREAK!
// Write to file f.
int
filewrite(struct file *f, void *addr, int n)
{
  int r;

  if(f->writable == 0)
    return -1;
  if(f->type == FD_INODE){
    // write a few blocks at a time to avoid exceeding
    // the maximum log transaction size, including
    // i-node, indirect block, allocation blocks,
    // and 2 blocks of slop for non-aligned writes.
    // this really belongs lower down, since writei()
    // might be writing a device like the console.
    int max = ((MAXOPBLOCKS-1-1-2) / 2) * 512;
    int i = 0;
    while(i < n){
      int n1 = n - i;
      if(n1 > max)
        n1 = max;

      begin_op();
      ilock(f->ip);
      if ((r = writei(f->ip, addr + i, f->off, n1)) > 0)
        f->off += r;
      iunlock(f->ip);
      end_op();

      if(r < 0)
        break;
      if(r != n1)
        panic("short filewrite");
      i += r;
    }
    return i == n ? n : -1;
  }
  panic("filewrite");
}

long
filelseek(struct file *f, long offset, int whence)
{
  long pos = -1;
  if(f->type == FD_INODE){
    ilock(f->ip);
    if (f->ip->type == T_FILE) {
      uint size = f->ip->size;
      switch (whence) {
      case 0:  // SEEK_SET
        pos = offset;
        break;
      case 1:  // SEEK_CUR
        pos = f->off + offset;
        break;
      case 2:  // SEEK_END
        pos = size + offset;
        break;
      }
      pos = pos < 0 ? 0 : pos > size ? size : pos;
      f->off = pos;
    }
    iunlock(f->ip);
  }
  return pos;
}

int
filetruncate(struct file* f, uint length)
{
  if(f->type == FD_INODE){
    begin_op();
    ilock(f->ip);
    isetsize(f->ip, length);
    iunlock(f->ip);
    end_op();
    return 0;
  }
  return -1;
}

int
fileisatty(struct file* f)
{
  int result = 0;
  switch (f->type) {
  case FD_INODE:
    {
      ilock(f->ip);
      short type = f->ip->type;
      iunlock(f->ip);
      if (type == T_DEV) {  // TODO: Check
        result = 1;
      }
    }
    break;
  default:
    break;
  }
  return result;
}
