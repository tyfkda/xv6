#include <assert.h>
#include <dirent.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

#define stat xv6_stat  // avoid clash with host struct stat
#define dirent xv6_dirent  // avoid clash with host struct dirent
#include "../include/types.h"
#include "../include/stat.h"
#include "../kernel/fs.h"
#include "../kernel/param.h"
#undef stat
#undef dirent

#ifndef static_assert
# define static_assert(a, b) do { switch (0) case 0: case (a): ; } while (0)
#endif // static_assert

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

const int nbitmap = DIVROUNDUP(FSSIZE, BSIZE * 8);
const int ninodeblocks = DIVROUNDUP(NINODES, IPB);
const int nlog = LOGSIZE;

int fsfd;
struct superblock sb;
uint freeinode = ROOTINO;
uint freeblock;
time_t mtime;

void balloc(int);
void wsect(uint, void*);
void winode(uint, struct dinode*);
void rinode(uint inum, struct dinode *ip);
void rsect(uint sec, void *buf);
uint ialloc(ushort type);
void iappend(uint inum, void *p, int n);
static void put1(uint inum, const char *path, int create_dir);

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

static void setup_superblock() {
  int nmeta;    // Number of meta blocks (boot, sb, nlog, inode, bitmap)
  int nblocks;  // Number of data blocks

  // 1 fs block = 1 disk sector
  nmeta = 2 + nlog + ninodeblocks + nbitmap;
  nblocks = FSSIZE - nmeta;

  sb.size = xint(FSSIZE);
  sb.nblocks = xint(nblocks);
  sb.ninodes = xint(NINODES);
  sb.nlog = xint(nlog);
  sb.logstart = xint(2);
  sb.inodestart = xint(2 + nlog);
  sb.bmapstart = xint(2 + nlog + ninodeblocks);

  printf("nmeta %d (boot, super, log blocks %u inode blocks %u, bitmap blocks %u) blocks %d total %d\n",
         nmeta, nlog, ninodeblocks, nbitmap, nblocks, FSSIZE);

  freeblock = nmeta;     // the first free block that we can allocate
}

static void clear_all_sectors() {
  char zeroes[BSIZE];
  int i;

  memset(zeroes, 0, sizeof(zeroes));
  for(i = 0; i < FSSIZE; i++)
    wsect(i, zeroes);
}

static void putdirent(uint parent, uint inum, const char *name) {
  struct xv6_dirent de;
  assert(strchr(name, DS) == NULL);
  bzero(&de, sizeof(de));
  de.inum = xshort(inum);
  strncpy(de.name, name, DIRSIZ);
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
static void putfile(uint parent, const char *path) {
  int cc, fd;
  uint inum;
  char buf[BSIZE];

  fd = open(path, 0);
  if(fd < 0){
    perror(path);
    exit(1);
  }

  inum = ialloc(T_FILE);
  putdirent(parent, inum, getbasename(path));

  while((cc = read(fd, buf, sizeof(buf))) > 0)
    iappend(inum, buf, cc);

  close(fd);
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
static uint iallocdir(uint parent, const char *name) {
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
  DIR* dir = opendir(path);
  if (dir == NULL) {
    perror(path);
    exit(1);
  }

  struct dirent* dent;
  while ((dent = readdir(dir)) != NULL) {
    if (iscurdir(dent->d_name) || isparentdir(dent->d_name))
      continue;

    char child_path[128];
    // TODO: Avoid buffer overrun.
    snprintf(child_path, sizeof(child_path), "%s%c%s", path, DS, dent->d_name);
    put1(inum, child_path, TRUE);
  }

  closedir(dir);
}

static void put1(uint inum, const char *path, int create_dir) {
  struct stat st;
  if (stat(path, &st) != 0) {
    perror(path);
    exit(1);
  }

  if ((st.st_mode & S_IFMT) == S_IFDIR) {
    uint target = create_dir ? iallocdir(inum, getbasename(path)) : inum;
    putdirentries(target, path);
  } else {
    putfile(inum, path);
  }
}

int
main(int argc, char *argv[])
{
  int i;
  uint rootino, off;
  char buf[BSIZE];
  struct dinode din;


  static_assert(sizeof(int) == 4, "Integers must be 4 bytes!");

  if(argc < 2){
    fprintf(stderr, "Usage: mkfs fs.img files...\n");
    exit(1);
  }

  assert((BSIZE % sizeof(struct dinode)) == 0);
  assert((BSIZE % sizeof(struct xv6_dirent)) == 0);

  //time(&mtime);
  mtime = 0;  // for test

  fsfd = open(argv[1], O_RDWR|O_CREAT|O_TRUNC, 0666);
  if(fsfd < 0){
    perror(argv[1]);
    exit(1);
  }

  setup_superblock();
  clear_all_sectors();

  memset(buf, 0, sizeof(buf));
  memmove(buf, &sb, sizeof(sb));
  wsect(1, buf);

  rootino = iallocdir(ROOTINO, NULL);
  assert(rootino == ROOTINO);

  for(i = 2; i < argc; i++){
    put1(rootino, argv[i], FALSE);
  }

  // fix size of root inode dir
  rinode(rootino, &din);
  off = xint(din.size);
  off = ((off / BSIZE) + 1) * BSIZE;
  din.size = xint(off);
  winode(rootino, &din);

  balloc(freeblock);

  return 0;
}

void
wsect(uint sec, void *buf)
{
  if(lseek(fsfd, sec * BSIZE, 0) != sec * BSIZE){
    perror("lseek");
    exit(1);
  }
  if(write(fsfd, buf, BSIZE) != BSIZE){
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
  if(lseek(fsfd, sec * BSIZE, 0) != sec * BSIZE){
    perror("lseek");
    exit(1);
  }
  if(read(fsfd, buf, BSIZE) != BSIZE){
    perror("read");
    exit(1);
  }
}

uint
ialloc(ushort type)
{
  uint inum = freeinode++;
  struct dinode din;

  bzero(&din, sizeof(din));
  din.type = xshort(type);
  din.nlink = xshort(1);
  din.size = xint(0);
  din.mtime = (uint)mtime;
  winode(inum, &din);
  return inum;
}

void
balloc(int used)
{
  uchar buf[BSIZE];
  int i;

  printf("balloc: first %d blocks have been allocated\n", used);
  assert(used < BSIZE*8);
  bzero(buf, BSIZE);
  for(i = 0; i < used; i++){
    buf[i/8] = buf[i/8] | (0x1 << (i%8));
  }
  printf("balloc: write bitmap block at sector %d\n", sb.bmapstart);
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
        din.addrs[fbn] = xint(freeblock++);
      }
      x = xint(din.addrs[fbn]);
    } else {
      if(xint(din.addrs[NDIRECT]) == 0){
        din.addrs[NDIRECT] = xint(freeblock++);
      }
      rsect(xint(din.addrs[NDIRECT]), (char*)indirect);
      if(indirect[fbn - NDIRECT] == 0){
        indirect[fbn - NDIRECT] = xint(freeblock++);
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
