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

#define DS  ('/')  // Directory separator

void iupdate(struct dinode *ip, uint inum);

typedef enum {
  UNKNOWN = -1,
  LS,
  PULL,
  MKDIR,
  RM,
} SUBCMD;

static const char* kSubCommands[] = {
  "ls",
  "pull",
  "mkdir",
  "rm",
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
          "\tls [path] ...         List files (default: /)\n"
          "\tpull <source> [dest]  Pull a file from image to local (default: to current)\n"
          "\tmkdir [path] ...      Make directory.\n"
          "\trm [path] ...         Remove file(s).\n"
    );
}

static const char* getBasename(const char* path) {
  const char* p = strrchr(path, DS);
  return p != NULL ? p + 1 : path;
}

// Is given path absolute?
static int isAbsPath(const char* path) {
  return path[0] == DS;
}

// Combine two paths and put it to the output buffer.
static void combinePath(const char* dirPath, const char* mainPath,
                        char* out, size_t outBufSize) {
  if (isAbsPath(mainPath)) {
    strncpy(out, mainPath, outBufSize);
    return;
  }

  int l = strlen(dirPath);
  strncpy(out, dirPath, outBufSize);
  out += l;
  if (l > 0 && out[-1] != DS && l < outBufSize)
    *out++ = DS;
  strncpy(out, mainPath, outBufSize - l);
}

static void panic(const char* msg) {
  fprintf(stderr, "%s\n", msg);
  exit(1);
}

// convert to intel byte order
static ushort xshort(ushort x) {
  ushort y;
  uchar *a = (uchar*)&y;
  a[0] = x;
  a[1] = x >> 8;
  return y;
}

static uint xint(uint x) {
  uint y;
  uchar *a = (uchar*)&y;
  a[0] = x;
  a[1] = x >> 8;
  a[2] = x >> 16;
  a[3] = x >> 24;
  return y;
}

static void wsect(uint sec, void *buf) {
  if(lseek(fsfd, sec * BSIZE, 0) != sec * BSIZE){
    perror("lseek");
    exit(1);
  }
  if(write(fsfd, buf, BSIZE) != BSIZE){
    perror("write");
    exit(1);
  }
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

static void winode(uint inum, struct dinode *ip) {
  char buf[BSIZE];
  uint bn;
  struct dinode *dip;

  bn = IBLOCK(inum, sb);
  rsect(bn, buf);
  dip = ((struct dinode*)buf) + (inum % IPB);
  *dip = *ip;
  wsect(bn, buf);
}

static void rinode(uint inum, struct dinode *ip) {
  uint bn = IBLOCK(inum, sb);
  char buf[BSIZE];
  rsect(bn, buf);
  struct dinode *dip = ((struct dinode*)buf) + (inum % IPB);
  *ip = *dip;
}

static void wbmap(uint inum, int value) {
  char bmap[BSIZE];
  rsect(sb.bmapstart, bmap);
  int mask = 1 << (inum % 8);
  char b = bmap[inum / 8];
  char nb = (b & ~mask) | (value ? mask : 0);
  if (b != nb) {
    bmap[inum / 8] = nb;
    wsect(sb.bmapstart, bmap);
  }
}

// Free a disk block.
static void
bfree(uint b)
{
  wbmap(b, 0);
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

static int findFreeBlock(void) {
  int imax = (sb.size + BPB - 1) / BPB;
  for (int i = 0; i < imax; ++i) {
    uchar bmap[BSIZE];
    rsect(sb.bmapstart + i, bmap);
    int jmax = (sb.size - i * BPB + 7) / 8;
    jmax = jmax >= BSIZE ? BSIZE : jmax;
    for (int j = 0; j < jmax; ++j) {
      uchar b = bmap[j];
      if (b == 0xff)
        continue;

      // Space found: detect which bit to use.
      int bi = i * BPB + j * 8;
      int kmax = sb.size - bi;
      kmax = kmax >= 8 ? 8 : kmax;
      for (int k = 0; k < 8; ++k) {
        if ((b & (1 << k)) == 0)
          return bi + k;
      }
    }
  }
  panic("Failed to allocate free block.");
  return -1;
}

static int allocFreeBlock(void) {
  int inum = findFreeBlock();
  wbmap(inum, 1);
  return inum;
}

static int findFreeInode(void) {
  int inum;
  struct dinode *dip;

  for(inum = 1; inum < sb.ninodes; inum++){
    char buf[BSIZE];
    bread(IBLOCK(inum, sb), buf);
    dip = (struct dinode*)buf + inum%IPB;
    if(dip->type == 0){  // a free inode
      return inum;
    }
  }
  panic("findFreeInode: no inodes");
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

// Write data to inode.
// Caller must hold ip->lock.
int
writei(uint inum, struct dinode *ip, char *src, uint off, uint n)
{
  uint tot, m;
  //struct buf *bp;

  //if(ip->type == T_DEV){
  //  if(ip->major < 0 || ip->major >= NDEV || !devsw[ip->major].write)
  //    return -1;
  //  return devsw[ip->major].write(ip, src, n);
  //}

  if(off > ip->size || off + n < off)
    return -1;
  if(off + n > MAXFILE*BSIZE)
    return -1;

  for(tot=0; tot<n; tot+=m, off+=m, src+=m){
    char buf[BSIZE];
    bread(bmap(ip, off/BSIZE), buf);
    m = min(n - tot, BSIZE - off%BSIZE);
    memmove(buf + off%BSIZE, src, m);
    //log_write(bp);
    //brelse(bp);
    wsect(bmap(ip, off/BSIZE), buf);
  }

  if(n > 0 && off > ip->size){
    ip->size = off;
    iupdate(ip, inum);
  }
  return n;
}

static void iappend(uint inum, void *xp, int n) {
  char *p = (char*)xp;
  uint fbn, off, n1;
  struct dinode din;
  char buf[BSIZE];
  uint indirect[NINDIRECT];
  uint x;

  rinode(inum, &din);
  off = xint(din.size);
  // printf("append inum %d at off %d sz %d\n", inum, off, n);
  while(n > 0){
    fbn = off / BSIZE;
    assert(fbn < MAXFILE);
    if(fbn < NDIRECT){
      if(xint(din.addrs[fbn]) == 0){
        uint freeblock = allocFreeBlock();
        din.addrs[fbn] = xint(freeblock);
      }
      x = xint(din.addrs[fbn]);
    } else {
      if(xint(din.addrs[NDIRECT]) == 0){
        uint freeblock = allocFreeBlock();
        din.addrs[NDIRECT] = xint(freeblock);
      }
      rsect(xint(din.addrs[NDIRECT]), (char*)indirect);
      if(indirect[fbn - NDIRECT] == 0){
        uint freeblock = allocFreeBlock();
        indirect[fbn - NDIRECT] = xint(freeblock);
        wsect(xint(din.addrs[NDIRECT]), (char*)indirect);
      }
      x = xint(indirect[fbn-NDIRECT]);
    }
    n1 = min(n, (fbn + 1) * BSIZE - off);
    rsect(x, buf);
    bcopy(p, buf + off - (fbn * BSIZE), n1);
    wsect(x, buf);
    n -= n1;
    off += n1;
    p += n1;
  }
  din.size = xint(off);
  winode(inum, &din);
}

static void putdirent(uint parent, uint inum, const char *name) {
  // Find space.
  struct dinode din;
  rinode(parent, &din);
  struct dirent de;
  for(uint off = 0; off < din.size; off += sizeof(de)){
    if(readi(&din, (char*)&de, off, sizeof(de)) != sizeof(de))
      panic("putdirent read");
    if(de.d_ino == 0) {
      // Found: put here.
      bzero(&de, sizeof(de));
      de.d_ino = xshort(inum);
      strncpy(de.d_name, name, DIRSIZ);

      if (writei(parent, &din, (char*)&de, off, sizeof(de)) != sizeof(de))
        panic("putdirent write");
      return;
    }
  }

  // Add new one.
  assert(strchr(name, DS) == NULL);
  bzero(&de, sizeof(de));
  de.d_ino = xshort(inum);
  strncpy(de.d_name, name, DIRSIZ);
  iappend(parent, &de, sizeof(de));
}

static int ialloc(ushort type) {
  int inum = findFreeInode();

  struct dinode din;
  bzero(&din, sizeof(din));
  din.type = xshort(type);
  din.nlink = xshort(1);
  din.size = xint(0);
  winode(inum, &din);
  return inum;
}

// Allocate inode for directory, and put defaults ("." and "..")
static uint iallocdir(uint parent, const char *name) {
  uint inum = ialloc(T_DIR);
  putdirent(inum, inum, ".");
  putdirent(inum, parent, "..");
  if (name != NULL) {
    putdirent(parent, inum, name);
  }
  return inum;
}

// Truncate inode (discard contents).
// Only called when the inode has no links
// to it (no directory entries referring to it)
// and has no in-memory reference to it (is
// not an open file or current directory).
static void
itrunc(struct dinode *ip, uint inum)
{
  int i, j;
  uint *a;

  for(i = 0; i < NDIRECT; i++){
    if(ip->addrs[i]){
      bfree(ip->addrs[i]);
      ip->addrs[i] = 0;
    }
  }

  if(ip->addrs[NDIRECT]){
    char buf[BSIZE];
    bread(ip->addrs[NDIRECT], buf);
    a = (uint*)buf;
    for(j = 0; j < NINDIRECT; j++){
      if(a[j])
        bfree(a[j]);
    }
    //brelse(bp);
    bfree(ip->addrs[NDIRECT]);
    ip->addrs[NDIRECT] = 0;
  }

  ip->size = 0;
  //iupdate(ip, inum);
}

// Copy a modified in-memory inode to disk.
// Must be called after every change to an ip->xxx field
// that lives on disk, since i-node cache is write-through.
// Caller must hold ip->lock.
void
iupdate(struct dinode *ip, uint inum)
{
  //struct buf *bp;
  char buf[BSIZE];
  struct dinode *dip;

  bread(IBLOCK(inum, sb), buf);
  dip = (struct dinode*)buf + inum%IPB;
  dip->type = ip->type;
  dip->major = ip->major;
  dip->minor = ip->minor;
  dip->nlink = ip->nlink;
  dip->size = ip->size;
  memmove(dip->addrs, ip->addrs, sizeof(ip->addrs));
  //log_write(bp);
  //brelse(bp);
  wsect(IBLOCK(inum, sb), buf);

  if (ip->nlink == 0) {
    assert(ip->nlink == 0);
    itrunc(ip, inum);
  }
}

// Is the directory dp empty except for "." and ".." ?
static int
isdirempty(struct dinode *dp)
{
  int off;
  struct dirent de;

  for(off=2*sizeof(de); off<dp->size; off+=sizeof(de)){
    if(readi(dp, (char*)&de, off, sizeof(de)) != sizeof(de))
      panic("isdirempty: readi");
    if(de.d_ino != 0)
      return FALSE;
  }
  return TRUE;
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
dirlookup(struct dinode *din, const char *name, uint* poff)
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
      // entry matches path element
      if(poff != NULL)
        *poff = off;
      return de.d_ino;
    }
  }
  return -1;
}

static int namex(const char* path, int nameiparent, char* name) {
  int inum = ROOTINO;
  while((path = skipelem(path, name)) != 0){
    struct dinode din;
    rinode(inum, &din);

    if (din.type != T_DIR) {
      return -1;
    }

    if(nameiparent && *path == '\0'){
      // Stop one level early.
      return inum;
    }

    int next = dirlookup(&din, name, NULL);
    if (next  == -1) {
      inum = -1;
      break;
    }
    inum = next;
  }
  return inum;
}

static int namei(const char* path) {
  char name[DIRSIZ];
  return namex(path, FALSE, name);
}

static int nameiparent(const char* path, char* name) {
  return namex(path, TRUE, name);
}

void setupImgFs(const char* imgFn, int readwrite) {
  fsfd = readwrite ? host_readwriteopen(imgFn) : host_readopen(imgFn);
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

static void* readFileFromImg(const char* path, size_t* pSize) {
  int inum = namei(path);
  if (inum == -1)
    return NULL;

  struct dinode ip;
  rinode(inum, &ip);
  if (ip.type != T_FILE) {
    fprintf(stderr, "%s is not a file.\n", path);
    return NULL;
  }

  char* contents = malloc(ip.size);
  if (contents == NULL) {
    fprintf(stderr, "alloc failed (size=%d)\n", ip.size);
    return NULL;
  }

  for (uint off = 0; off < ip.size; off += BSIZE) {
    uint bn = off / BSIZE;
    if (bn < NDIRECT) {
      char buf[BSIZE];
      rsect(ip.addrs[bn], buf);
      memcpy(contents + off, buf, min(BSIZE, ip.size - off));
    } else {
      fprintf(stderr, "pull: not implemented, block >= NDIRECT (%d), size=%d\n", bn, ip.size);
      return NULL;
    }
  }

  if (pSize != NULL)
    *pSize = ip.size;

  return contents;
}

void doPull(const char* srcPath, const char* dstPath) {
  size_t size;
  unsigned char* contents = readFileFromImg(srcPath, &size);
  if (contents == NULL) {
    fprintf(stderr, "Cannot load from img file: path=[%s]\n", srcPath);
    exit(1);
  }

  if (dstPath == NULL) {
    // Out to stdout
    write(STDOUT_FILENO, contents, size);
    return;
  }

  char dstFnBuf[512];
  if (host_isdir(dstPath)) {  // Given destination path is a directory.
    combinePath(dstPath, getBasename(srcPath), dstFnBuf, sizeof(dstFnBuf));
    dstPath = dstFnBuf;
  }
  int dstFd = host_writeopen(dstPath);
  if (dstFd < 0) {
    perror(dstPath);
    exit(1);
  }

  if (write(dstFd, contents, size) != size) {
    perror("Write failed");
    exit(1);
  }
  host_close(dstFd);

  free(contents);
}

void doMkdir(const char* path) {
  // like -p option, automatically create directory recursively.

  int inum = ROOTINO;
  char name[DIRSIZ] = {'\0'};
  while ((path = skipelem(path, name)) != 0) {
    struct dinode din;
    rinode(inum, &din);

    if (din.type != T_DIR) {
      fprintf(stderr, "%s is not a directory\n", name);  // TODO: Check length.
      exit(1);
    }

    int next = dirlookup(&din, name, NULL);
    if (next  == -1) {
      next = iallocdir(inum, name);
      if (next == -1) {
        fprintf(stderr, "mkdir [%s] failed.\n", name);
        exit(1);
      }
    }
    inum = next;
  }
}

void doRm(const char* path) {
  char name[DIRSIZ];
  int parent = nameiparent(path, name);
  if (parent == -1) {
    fprintf(stderr, "rm: Cannot find path: %s\n", path);
    exit(1);
  }

  // Cannot unlink "." or "..".
  if (namecmp(name, ".") == 0 || namecmp(name, "..") == 0) {
    fprintf(stderr, "rm: Cannot remove path: %s\n", path);
    exit(1);
  }

  struct dinode dp;
  rinode(parent, &dp);
  uint off;
  int inum = dirlookup(&dp, name, &off);
  if (inum == -1) {
    fprintf(stderr, "rm: Cannot find path: %s\n", path);
    exit(1);
  }

  struct dinode ip;
  rinode(inum, &ip);
  if(ip.type == T_DIR && !isdirempty(&ip)){
    fprintf(stderr, "rm: Directory not empty: %s\n", path);
    exit(1);
  }

  struct dirent de;
  memset(&de, 0, sizeof(de));
  if(writei(parent, &dp, (char*)&de, off, sizeof(de)) != sizeof(de))
    panic("unlink: writei");
  //if(ip.type == T_DIR){
  //  dp.nlink--;
  //  iupdate(&dp, parent);
  //}

  ip.nlink--;
  iupdate(&ip, inum);
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
  case PULL:
    {
      if (argc < 4 || argc > 5) {
        fprintf(stderr, "pull: <src path (in image)> [dst path (in local)]\n");
        exit(1);
      }

      const char* src = argv[3];
      const char* dst = argc > 4 ? argv[4] : NULL;

      setupImgFs(imgFn, FALSE);
      doPull(src, dst);
    }
    break;
  case MKDIR:
    {
      if (argc < 4) {
        fprintf(stderr, "mkdir: path...\n");
        exit(1);
      }

      setupImgFs(imgFn, TRUE);
      for (int i = 3; i < argc; ++i) {
        doMkdir(argv[i]);
      }
    }
    break;
  case RM:
    {
      if (argc < 4) {
        fprintf(stderr, "rm: path...\n");
        exit(1);
      }

      setupImgFs(imgFn, TRUE);
      for (int i = 3; i < argc; ++i) {
        doRm(argv[i]);
      }
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
