#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

//#include "../include/stat.h"
#include "../include/sys/_ttype.h"
#include "../kernel/fs.h"
#include "../kernel/spinlock.h"
#include "../kernel/sleeplock.h"
#include "../kernel/file.h"
#include "../kernel/param.h"

#include "./hostfsaux.h"

#ifndef static_assert
# define static_assert(a, b) do { switch (0) case 0: case (a): ; } while (0)
#endif // static_assert

#define min(a, b) ((a) < (b) ? (a) : (b))

#ifndef FALSE
#define FALSE  (0)
#endif
#ifndef TRUE
#define TRUE   (1)
#endif

typedef enum {
  UNKNOWN = -1,
  LS,
} SUBCMD;

static const char* kSubCommands[] = {
  "ls",
  NULL,
};

int fsfd;
struct superblock sb;

static SUBCMD getSubcommand(const char* str) {
  for (int i = 0; kSubCommands[i] != NULL; ++i) {
    if (strcmp(kSubCommands[i], str) == 0)
      return (SUBCMD)i;
  }
  return UNKNOWN;
}

static void showHelp(void) {
  FILE* fp = stderr;
  fprintf(fp,
          "Usage: fs <img> <subcommand>\n"
          "\n"
          "Subcommands:\n"
          "\tls [path] ...    list files\n"
    );
}

static void panic(const char* msg) {
  fprintf(stderr, "%s\n", msg);
  exit(1);
}

static void rsect(int sec, void *buf) {
  if (lseek(fsfd, sec * BSIZE, 0) != sec * BSIZE) {
    perror("lseek");
    exit(1);
  }
  if (read(fsfd, buf, BSIZE) != BSIZE) {
    perror("read");
    exit(1);
  }
}

static void rinode(uint inum, struct dinode *ip) {
  uint bn = IBLOCK(inum, sb);
  char buf[BSIZE];
  rsect(bn, buf);
  struct dinode *dip = ((struct dinode*)buf) + (inum % IPB);
  *ip = *dip;
}



// Inode content
//
// The content (data) associated with each inode is stored
// in blocks on the disk. The first NDIRECT block numbers
// are listed in ip->addrs[].  The next NINDIRECT blocks are
// listed in block ip->addrs[NDIRECT].

// Allocate a zeroed disk block.
static uint
balloc(void)
{
  fprintf(stderr, "balloc: not implementd\n");
  exit(1);
}

// Return a locked buf with the contents of the indicated block.
static void
bread(uint blockno, char* out)
{
  rsect(blockno, out);
}

// Return the disk block address of the nth block in inode ip.
// If there is no such block, bmap allocates one.
static uint
bmap(struct dinode *ip, uint bn)
{
  uint addr;

  if(bn < NDIRECT){
    if((addr = ip->addrs[bn]) == 0)
      ip->addrs[bn] = addr = balloc();
    return addr;
  }
  bn -= NDIRECT;

#if 0
  if(bn < NINDIRECT){
    // Load indirect block, allocating if necessary.
    if((addr = ip->addrs[NDIRECT]) == 0)
      ip->addrs[NDIRECT] = addr = balloc();
    struct buf *bp = bread(ip->dev, addr);
    uint *a = (uint*)bp->data;
    if((addr = a[bn]) == 0){
      a[bn] = addr = balloc(ip->dev);
      log_write(bp);
    }
    //brelse(bp);
    return addr;
  }
#endif

  panic("bmap: out of range");
  return -1;
}

// Read data from inode.
// Caller must hold ip->lock.
int
readi(struct dinode *ip, char *dst, uint off, uint n)
{
  uint tot, m;
  //struct buf *bp;

  //if(ip->type == T_DEV){
  //  if(ip->major < 0 || ip->major >= NDEV || !devsw[ip->major].read)
  //    return -1;
  //  return devsw[ip->major].read(ip, dst, n);
  //}

  if(off > ip->size || off + n < off)
    return -1;
  if(off + n > ip->size)
    n = ip->size - off;

  for(tot=0; tot<n; tot+=m, off+=m, dst+=m){
    char buf[BSIZE];
    bread(bmap(ip, off/BSIZE), buf);
    m = min(n - tot, BSIZE - off%BSIZE);
    memmove(dst, buf + off%BSIZE, m);
    //brelse(bp);
  }
  return n;
}

// Copy the next path element from path into name.
// Return a pointer to the element following the copied one.
// The returned path has no leading slashes,
// so the caller can check *path=='\0' to see if the name is the last one.
// If no name to remove, return 0.
//
// Examples:
//   skipelem("a/bb/c", name) = "bb/c", setting name = "a"
//   skipelem("///a//bb", name) = "bb", setting name = "a"
//   skipelem("a", name) = "", setting name = "a"
//   skipelem("", name) = skipelem("////", name) = 0
//
static const char*
skipelem(const char *path, char *name)
{
  const char *s;
  int len;

  while(*path == '/')
    path++;
  if(*path == 0)
    return 0;
  s = path;
  while(*path != '/' && *path != 0)
    path++;
  len = path - s;
  if(len >= DIRSIZ)
    memmove(name, s, DIRSIZ);
  else {
    memmove(name, s, len);
    name[len] = 0;
  }
  while(*path == '/')
    path++;
  return path;
}

int
namecmp(const char *s, const char *t)
{
  return strncmp(s, t, DIRSIZ);
}

// Look for a directory entry in a directory.
// If found, set *poff to byte offset of entry.
int
dirlookup(struct dinode *din, const char *name)
{
  uint off;
  struct dirent de;

  //if(dp->type != T_DIR)
  //  panic("dirlookup not DIR");

  for(off = 0; off < din->size; off += sizeof(de)){
    if(readi(din, (char*)&de, off, sizeof(de)) != sizeof(de))
      panic("dirlookup read");
    if(de.d_ino == 0)
      continue;
    if(namecmp(name, de.d_name) == 0){
      return de.d_ino;
    }
  }
  return -1;
}

static int namei(const char* path) {
  int inum = ROOTINO;
  char name[DIRSIZ];
  while((path = skipelem(path, name)) != 0){
    struct dinode din;
    rinode(inum, &din);

    int next = dirlookup(&din, name);
    if (next  == -1) {
      inum = -1;
      break;
    }
    inum = next;
  }
  return inum;
}

void setupImgFs(const char* imgFn, int write) {
  if (write) {
    fprintf(stderr, "TODO: Implement write open for image file\n");
    exit(1);
  }

  fsfd = host_readopen(imgFn);
  if (fsfd < 0) {
    perror(imgFn);
    exit(1);
  }

  char buf[BSIZE];
  rsect(1, buf);
  memcpy(&sb, buf, sizeof(sb));
}

const char*
fmtname(const char *path)
{
  static char buf[DIRSIZ+1];
  const char *p;

  // Find first character after last slash.
  for(p=path+strlen(path); p >= path && *p != '/'; p--)
    ;
  p++;

  // Return blank-padded name.
  if(strlen(p) >= DIRSIZ)
    return p;
  memmove(buf, p, strlen(p));
  memset(buf+strlen(p), ' ', DIRSIZ-strlen(p));
  return buf;
}

void doLs(const char* path) {
  int inum = namei(path);
  if (inum == -1) {
    fprintf(stderr, "Cannot find %s\n", path);
    exit(1);
  }

  struct dinode ip;
  rinode(inum, &ip);
  switch (ip.type) {
  case T_FILE:
    printf("%s %d %d %d\n", fmtname(path), ip.type, inum, ip.size);
    break;
  case T_DIR:
    {
      uint off;
      struct dirent de;
      for(off = 0; off < ip.size; off += sizeof(de)){
        if(readi(&ip, (char*)&de, off, sizeof(de)) != sizeof(de))
          panic("dirlookup read");
        if(de.d_ino == 0)
          continue;

        struct dinode ip2;
        rinode(de.d_ino, &ip2);
        printf("%s %d %d %d\n", fmtname(de.d_name), ip2.type, de.d_ino, ip2.size);
      }
    }
    break;
  default:
    fprintf(stderr, "Unhandled type: %d\n", ip.type);
    exit(1);
    break;
  }
}

int main(int argc, char *argv[]) {
  static_assert(sizeof(int) == 4, "Integers must be 4 bytes!");

  if (argc < 3) {
    showHelp();
    return 1;
  }

  const char* imgFn = argv[1];
  const char* subcmd = argv[2];

  switch (getSubcommand(subcmd)) {
  case LS:
    setupImgFs(imgFn, FALSE);
    if (argc <= 3) {
      doLs("/");
    } else {
      for (int i = 3; i < argc; ++i)
        doLs(argv[i]);
    }
    break;
  case UNKNOWN:
  default:
    fprintf(stderr, "Unknown command: %s\n", subcmd);
    showHelp();
    return 1;
  }

  return 0;
}
