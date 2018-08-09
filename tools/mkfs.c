#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <limits.h>

#include "../include/sys/_ttype.h"
#include "../kernel/fs.h"
#include "../kernel/param.h"

#include "./hostfsaux.h"

#ifndef static_assert
# define static_assert(a, b) do { switch (0) case 0: case (a): ; } while (0)
#endif // static_assert

#define FSSIZE       1000  // size of file system in blocks
#define NINODES 200

#define DS  ('/')  // Directory separator

#ifndef FALSE
#define FALSE  (0)
#endif
#ifndef TRUE
#define TRUE  (1)
#endif

// Disk layout:
// [ boot block | sb block | log | inode blocks | free bit map | data blocks ]

#define DIVROUNDUP(x, n)  (((x) + (n) - 1) / (n))

const int nlog = LOGSIZE;

int fsfd;
struct superblock sb;
time_t mtime;

void bfill(int);
void allocsect(int);
void wsect(uint, void*);
void winode(uint, struct dinode*);
void rinode(uint inum, struct dinode *ip);
void rsect(uint sec, void *buf);
uint ialloc(ushort type);
uint iallocdir(uint parent, const char *name);
void iappend(uint inum, void *p, int n);
void put1(uint inum, const char *path, const char *dstName);

static uint balloc(void);

static void panic(const char* msg) {
  fprintf(stderr, "%s\n", msg);
  exit(1);
}

// convert to intel byte order
ushort
xshort(ushort x)
{
  ushort y;
  uchar *a = (uchar*)&y;
  a[0] = x;
  a[1] = x >> 8;
  return y;
}

uint
xint(uint x)
{
  uint y;
  uchar *a = (uchar*)&y;
  a[0] = x;
  a[1] = x >> 8;
  a[2] = x >> 16;
  a[3] = x >> 24;
  return y;
}

static void clear_all_sectors(int fssize) {
  char zeroes[BSIZE];
  int i;

  memset(zeroes, 0, sizeof(zeroes));
  for(i = 0; i < fssize; i++)
    wsect(i, zeroes);
}

static void setup_superblock(int ninodes, int fssize) {
  const int nbitmap = DIVROUNDUP(fssize, BSIZE * 8);
  int nmeta;    // Number of meta blocks (boot, sb, nlog, inode, bitmap)
  int nblocks;  // Number of data blocks
  int ninodeblocks;

  ninodeblocks = DIVROUNDUP(ninodes, IPB);

  // 1 fs block = 1 disk sector
  nmeta = 2 + nlog + ninodeblocks + nbitmap;

  if ( nmeta > fssize ) {
    fprintf(stderr, "nr-inodes: %u is too big.\n", ninodes);
    exit(1);
  }

  nblocks = fssize - nmeta;

  sb.size = xint(fssize);
  sb.nblocks = xint(nblocks);
  sb.ninodes = xint(ninodes);
  sb.nlog = xint(nlog);
  sb.logstart = xint(2);
  sb.inodestart = xint(2 + nlog);
  sb.bmapstart = xint(2 + nlog + ninodeblocks);

  printf("nmeta %d (boot, super, log blocks %u inode blocks %u(%u inodes), bitmap blocks %u) blocks %d total %d\n",
         nmeta, nlog, ninodeblocks, ninodes, nbitmap, nblocks, fssize);
}

void create_fs_img(const char* imgFn, int ninodes, int fssize) {
  fsfd = host_createopen(imgFn);
  if(fsfd < 0){
    perror(imgFn);
    exit(1);
  }

  setup_superblock(ninodes, fssize);

  clear_all_sectors(fssize);

  char buf[BSIZE];
  memset(buf, 0, sizeof(buf));
  memmove(buf, &sb, sizeof(sb));
  wsect(1, buf);

  // Fill meta blocks to be used
  const int ninodeblocks = DIVROUNDUP(ninodes, IPB);
  const int nbitmap = DIVROUNDUP(fssize, BSIZE * 8);
  int nmeta = xint(sb.logstart) + xint(sb.nlog) + xint(ninodeblocks) + nbitmap;
  bfill(nmeta);

  int rootino = iallocdir(ROOTINO, NULL);
  assert(rootino == ROOTINO);

  host_close(fsfd);
  fsfd = -1;
}

void open_fs_img(const char* imgFn) {
  fsfd = host_readwriteopen(imgFn);

  char buf[BSIZE];
  rsect(1, buf);
  memmove(&sb, buf, sizeof(sb));
}

static void putdirent(uint parent, uint inum, const char *name) {
  struct ddirent de;
  assert(strchr(name, DS) == NULL);
  bzero(&de, sizeof(de));
  de.d_ino = xshort(inum);
  strncpy(de.d_name, name, DIRSIZ);
  iappend(parent, &de, sizeof(de));
}

static const char* getbasename(const char *path) {
  const char *p = strrchr(path, DS);
  if (p != NULL) {
    if (p[1] == '\0')  // Path ends with directory separator.
      return NULL;
    return p + 1;
  }
  // No separator.
  return path;
}

// Put a file into the root directory.
// Assumes given dstName doesn't exist in parent.
static void putfile(uint parent, const char *path, const char *dstName) {
  int cc, fd;
  uint inum;
  char buf[BSIZE];

  fd = host_readopen(path);
  if(fd < 0){
    perror(path);
    exit(1);
  }

  inum = ialloc(T_FILE);
  putdirent(parent, inum, getbasename(dstName));

  while((cc = host_read(fd, buf, sizeof(buf))) > 0)
    iappend(inum, buf, cc);

  host_close(fd);
}

// Is current directory?
static int iscurdir(const char* name) {
  return strcmp(name, ".") == 0;
}

// Is parent directory?
static int isparentdir(const char* name) {
  return strcmp(name, "..") == 0;
}

// Allocate inode for directory, and put defaults ("." and "..")
uint iallocdir(uint parent, const char *name) {
  uint inum = ialloc(T_DIR);
  putdirent(inum, inum, ".");
  putdirent(inum, parent, "..");
  if (name != NULL) {
    assert(strchr(name, DS) == NULL);
    putdirent(parent, inum, name);
  }
  return inum;
}

// Put files in a given directory recursively.
static void putdirentries(uint inum, const char *path) {
  // List up directory entries.
  HOSTDIR *dir = host_opendir(path);
  if (dir == NULL) {
    perror(path);
    exit(1);
  }

  const char *entry;
  while ((entry = host_readdir(dir)) != NULL) {
    if (iscurdir(entry) || isparentdir(entry))
      continue;

    char child_path[128];
    // TODO: Avoid buffer overrun.
    snprintf(child_path, sizeof(child_path), "%s%c%s", path, DS, entry);
    put1(inum, child_path, child_path);
  }

  host_closedir(dir);
}

void put1(uint inum, const char *path, const char *dstName) {
  if (host_isdir(path)) {
    uint target = iallocdir(inum, getbasename(path));
    putdirentries(target, path);
  } else {
    putfile(inum, path, path);
  }
}

void
wsect(uint sec, void *buf)
{
  if(host_lseek(fsfd, sec * BSIZE, 0) != sec * BSIZE){
    perror("lseek");
    exit(1);
  }
  if(host_write(fsfd, buf, BSIZE) != BSIZE){
    perror("write");
    exit(1);
  }
}

void
winode(uint inum, struct dinode *ip)
{
  char buf[BSIZE];
  uint bn;
  struct dinode *dip;

  bn = IBLOCK(inum, sb);
  rsect(bn, buf);
  dip = ((struct dinode*)buf) + (inum % IPB);
  *dip = *ip;
  wsect(bn, buf);
}

void
rinode(uint inum, struct dinode *ip)
{
  char buf[BSIZE];
  uint bn;
  struct dinode *dip;

  bn = IBLOCK(inum, sb);
  rsect(bn, buf);
  dip = ((struct dinode*)buf) + (inum % IPB);
  *ip = *dip;
}

void
rsect(uint sec, void *buf)
{
  if(lseek(fsfd, sec * BSIZE, SEEK_SET) != sec * BSIZE){
    perror("lseek");
    exit(1);
  }
  if(host_read(fsfd, buf, BSIZE) != BSIZE){
    perror("read");
    exit(1);
  }
}

uint
ialloc(ushort type)
{
  int inum;
  char buf[BSIZE];
  struct dinode *dip;

  for (inum = ROOTINO; inum < sb.ninodes; ++inum) {
    if (inum % IPB == 0 || inum == ROOTINO) {
      rsect(IBLOCK(inum, sb), buf);
    }
    dip = (struct dinode*)buf + inum % IPB;
    if(dip->type == 0){  // a free inode
      struct dinode din;

      bzero(&din, sizeof(din));
      din.type = xshort(type);
      din.nlink = xshort(1);
      din.size = xint(0);
      din.mtime = (uint)mtime;
      winode(inum, &din);
      return inum;
    }
  }
  panic("inode full");
  return 0;
}

void
bfill(int used)
{
  uchar buf[BSIZE];
  int i;

  printf("bfill: first %d blocks have been allocated\n", used);
  assert(used < BSIZE*8);
  bzero(buf, BSIZE);
  for(i = 0; i < used; i++){
    buf[i/8] = buf[i/8] | (0x1 << (i%8));
  }
  printf("bfill: write bitmap block at sector %d\n", sb.bmapstart);
  wsect(sb.bmapstart, buf);
}

void
allocsect(int sec)
{
  uchar buf[BSIZE];

  assert(sec < sb.size);
  rsect(sb.bmapstart, buf);
  assert((buf[sec / 8] & (0x1 << (sec % 8))) == 0);
  buf[sec / 8] = buf[sec / 8] | (0x1 << (sec % 8));
  wsect(sb.bmapstart, buf);
}

#define min(a, b) ((a) < (b) ? (a) : (b))

void
iappend(uint inum, void *xp, int n)
{
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
        din.addrs[fbn] = xint(balloc());
      }
      x = xint(din.addrs[fbn]);
    } else {
      if(xint(din.addrs[NDIRECT]) == 0){
        din.addrs[NDIRECT] = xint(balloc());
      }
      rsect(xint(din.addrs[NDIRECT]), (char*)indirect);
      if(indirect[fbn - NDIRECT] == 0){
        indirect[fbn - NDIRECT] = xint(balloc());
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

////////////////////////////////////////////////
// Taken from fs.c

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
  int b, bi, m;
  uchar buf[BSIZE];

  // TODO: Consider the case that block has multiple pages.
  for(b = 0; b < sb.size; b += BPB){
    rsect(BBLOCK(b, sb), buf);
    for(bi = 0; bi < BPB && b + bi < sb.size; bi++){
      m = 1 << (bi % 8);
      if((buf[bi/8] & m) == 0){  // Is block free?
        int sec = b + bi;
        allocsect(sec);
        return sec;
      }
    }
  }
  panic("Block full");
  return -1;
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

  if(bn < NINDIRECT){
    // Load indirect block, allocating if necessary.
    if((addr = ip->addrs[NDIRECT]) == 0)
      ip->addrs[NDIRECT] = addr = balloc();
    char buf[BSIZE];
    bread(addr, buf);
    uint *a = (uint*)buf;
    if((addr = a[bn]) == 0){
      a[bn] = addr = balloc();
      //log_write(bp);
      wsect(addr, buf);
    }
    //brelse(bp);
    return addr;
  }

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
dirlookup(struct dinode *din, const char *name, uint* poff)
{
  uint off;
  struct ddirent de;

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
  *name = '\0';
  while((path = skipelem(path, name)) != 0){
    struct dinode din;
    rinode(inum, &din);

    if (din.type != T_DIR) {
      return -1;
    }

    if(nameiparent && *path == '\0'){
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

////////////////////////////////////////////////

int readdirent(const char* path, struct ddirent* pde) {
  char name[DIRSIZ];
  int parent = nameiparent(path, name);
  if (parent == -1)
    return FALSE;

  struct dinode din;
  rinode(parent, &din);
  uint off;
  if (dirlookup(&din, name, &off) < 0)
    return FALSE;

  return readi(&din, (char*)pde, off, sizeof(*pde)) == sizeof(*pde);
}

char*
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
    return (char*)p;
  memmove(buf, p, strlen(p));
  memset(buf+strlen(p), ' ', DIRSIZ-strlen(p));
  return buf;
}

void dumpinfo(const char* name, const struct dinode* de) {
  time_t mt = de->mtime;
  struct tm *t = localtime(&mt);
  printf("%s %4x %8d  %04d/%02d/%02d %02d:%02d\n",
         name, de->type, (int)de->size,
         t->tm_year + 1900, t->tm_mon + 1, t->tm_mday,
         t->tm_hour, t->tm_min);
}

void doLs(const char* path) {
  int ino = namei(path);
  if (ino == -1) {
    fprintf(stderr, "ls: path not found: %s\n", path);
    exit(1);
  }

  struct dinode din;
  rinode(ino, &din);
  if (din.type == T_FILE) {
    dumpinfo(fmtname(path), &din);
  } else {
    uint off;
    struct ddirent de;

    for(off = 0; off < din.size; off += sizeof(de)){
      if(readi(&din, (char*)&de, off, sizeof(de)) != sizeof(de))
        panic("ls read");
      if(de.d_ino == 0)
        continue;
      struct dinode din2;
      rinode(de.d_ino, &din2);
      dumpinfo(fmtname(de.d_name), &din2);
    }
  }
}

void doPut(const char* src, const char* dst) {
  char name[DIRSIZ];
  int parent = nameiparent(dst, name);
  if (parent == -1) {
    fprintf(stderr, "Cannot find dst: %s\n", dst);
    exit(1);
  }
  if (*name != '\0') {
    struct dinode din;
    rinode(parent, &din);
    int ino = dirlookup(&din, name, NULL);
    if (ino != -1) {
      struct dinode din;
      rinode(ino, &din);
      if (din.type == T_DIR) {
        parent = namei(dst);
        assert(parent != -1);
        *name = '\0';
      } else {
        // TODO: overwrite
        fprintf(stderr, "File already exists: %s\n", name);
        exit(1);
      }
    }
  }
  if (*name == '\0') {
    strncpy(name, getbasename(src), sizeof(name));
  }

  put1(parent, src, name);
}

void doMkdir(const char* path) {
  char name[DIRSIZ];
  int parent = nameiparent(path, name);
  if (parent == -1) {
    fprintf(stderr, "mkdir: unavailable path: %s\n", path);
    exit(1);
  }

  struct dinode din;
  rinode(parent, &din);
  int ino = dirlookup(&din, name, NULL);
  if (ino != -1) {
    fprintf(stderr, "mkdir: file already exists: %s\n", name);
    exit(1);
  }

  iallocdir(parent, name);
}

typedef enum {
  UNKNOWN = -1,
  INIT,
  LS,
  PUT,
  MKDIR,
} SUBCMD;

static const char* kSubCommands[] = {
  "init",
  "ls",
  "put",
  "mkdir",
  NULL,
};

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
          "Usage: mkfs [options] <img> <subcommand> ...\n"
          "\n"
          "options:\n"
          "\t-i <nr-inodes>\n"
          "\t-s <fssize>\n"
          "Subcommands:\n"
          "\tinit                    Create initialized image\n"
          "\tls                      List directory contents\n"
          "\tput <source...> [dest]  Put file(s) from local to image\n"
          "\tmkdir path...           Create directory(s) in image\n"
          );
}

int
main(int argc, char *argv[])
{
  long nr_inodes;
  long fssize;

  static_assert(sizeof(int) == 4, "Integers must be 4 bytes!");

  nr_inodes = NINODES;
  fssize = FSSIZE;

  int opt;
  while ((opt = getopt(argc, argv, "i:s:")) != -1) {
    switch (opt) {
    case 'i':
      nr_inodes = strtol(optarg, NULL, 0);
      if (nr_inodes == LONG_MAX || nr_inodes == LONG_MIN) {
        fprintf(stderr, "Illegal inode count\n");
        exit(1);
      }
      break;
    case 's':
      fssize = strtol(optarg, NULL, 0);
      if (fssize == LONG_MAX || fssize == LONG_MIN) {
        fprintf(stderr, "Illegal fssize\n");
        exit(1);
      }
      break;
    default: /* '?' */
      showHelp();
      exit(1);
    }
  }

  assert((BSIZE % sizeof(struct dinode)) == 0);
  assert((BSIZE % sizeof(struct ddirent)) == 0);

  if (optind + 2 > argc) {
    showHelp();
    return 1;
  }

  const char* imgFn = argv[optind];
  const char* subcmdStr = argv[optind + 1];

  SUBCMD subcmd = getSubcommand(subcmdStr);
  if (subcmd == INIT) {
    create_fs_img(imgFn, nr_inodes, fssize);
  } else {
    open_fs_img(imgFn);

    switch (subcmd) {
    case LS:
      if (argc < 4) {
        doLs("/");
      } else {
        for (int i = 3; i < argc; ++i)
          doLs(argv[i]);
      }
      break;
    case PUT:
      {
        if (argc < 4) {
          fprintf(stderr, "put: <src path (in local)> [dst path (in image)]\n");
          exit(1);
        }

        int srcMax = argc > 4 ? argc - 1 : argc;
        const char* dst = argc > 4 ? argv[argc - 1] : "/";

        for (int i = 3; i < srcMax; ++i) {
          doPut(argv[i], dst);
        }
      }
      break;
    case MKDIR:
      {
        if (argc < 4) {
          fprintf(stderr, "mkdir: path\n");
          exit(1);
        }

        for (int i = 3; i < argc; ++i) {
          doMkdir(argv[i]);
        }
      }
      break;
    case UNKNOWN:
    default:
      fprintf(stderr, "Unknown subcommand: %s\n", subcmdStr);
      showHelp();
      return 1;
    }
  }

  return 0;
}
