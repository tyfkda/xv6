#include "types.h"
#include "param.h"
#include "memlayout.h"
#include "mmu.h"
#include "proc.h"
#include "defs.h"
#include "x86.h"
#include "elf.h"

#define FILE_SEPARATOR  '/'

// Find exe path
static struct inode *find_path(const char *path, char *resultPath, int bufsiz) {
  struct inode *ip;

  if (strchr(path, FILE_SEPARATOR) != 0) {
    ip = namei(path);
    if (resultPath != 0)
      safestrcpy(resultPath, path, bufsiz);
  } else {
    // Also find at root path. TODO: Search from $PATH
    const char PATH[] = "/bin/";
    const int LEN = sizeof(PATH) - 1;
    const int BUFSIZ = 128;
    char exepath[BUFSIZ];
    memmove(exepath, PATH, LEN);
    safestrcpy(exepath + LEN, path, BUFSIZ - LEN);
    ip = namei(exepath);
    if (resultPath != 0)
      safestrcpy(resultPath, exepath, bufsiz);
  }
  return ip;
}

// Check ELF header
int
readelfhdr(struct inode *ip, struct elfhdr *elf)
{
  return readi(ip, elf, 0, sizeof(*elf)) == sizeof(*elf)
      && elf->magic == ELF_MAGIC;
}

int
execelf(const char *progname, const char *path, const char* const *argv,const char *envp[],
        const struct elfhdr *elf, struct inode **pip, pde_t **ppgdir)
{
  const char *s, *last;
  int i, off;
  uintp argc, sz, sp, ustack[4+MAXARG+1+MAXENV+1];
  struct inode *ip;
  struct proghdr ph;
  pde_t *pgdir, *oldpgdir;
  struct proc *curproc = myproc();
  uintp nr_args, env_idx, nr_envs;

  ip = *pip;
  pgdir = 0;

  if((pgdir = setupkvm()) == 0)
    goto bad;

  // Load program into memory.
  sz = 0;
  for(i=0, off=elf->phoff; i<elf->phnum; i++, off+=sizeof(ph)){
    if(readi(ip, &ph, off, sizeof(ph)) != sizeof(ph))
      goto bad;
    if(ph.type != ELF_PROG_LOAD)
      continue;
    if(ph.memsz < ph.filesz)
      goto bad;
    if(ph.vaddr + ph.memsz < ph.vaddr)
      goto bad;
    if((sz = allocuvm(pgdir, sz, ph.vaddr + ph.memsz)) == 0)
      goto bad;
    if(ph.vaddr % PGSIZE != 0)
      goto bad;
    if(loaduvm(pgdir, (char*)ph.vaddr, ip, ph.off, ph.filesz) < 0)
      goto bad;
  }
  iunlockput(ip);
  end_op();
  ip = *pip = 0;

  // Allocate two pages at the next page boundary.
  // Make the first inaccessible.  Use the second as the user stack.
  sz = PGROUNDUP(sz);
  if((sz = allocuvm(pgdir, sz, sz + 2*PGSIZE)) == 0)
    goto bad;
  clearpteu(pgdir, (char*)(sz - 2*PGSIZE));
  sp = sz;

  for(nr_args = 0; (MAXARG > nr_args) && (argv[nr_args] != 0); ++nr_args);
  for(nr_envs = 0; (MAXENV > nr_envs) && (envp[nr_envs] != 0); ++nr_envs);

  for(env_idx = 0; nr_envs > env_idx; ++env_idx) {

    // Copy environment variable including NULL terminate
    // and we ensure each element is aligned to the word.
    sp = (sp - (strlen(envp[nr_envs - env_idx - 1]) + 1)) & ~(sizeof(uintp)-1);
    if (copyout(pgdir, sp, envp[nr_envs - env_idx - 1], 
	strlen(envp[nr_envs - env_idx - 1]) + 1) < 0)
       goto bad;
    
    // store the address of a variable
    ustack[4 + nr_args + 1 + nr_envs - env_idx - 1] = sp; 
  }
  ustack[4 + nr_args + 1 + nr_envs] = (uintp)0;

  // Push argument strings, prepare rest of stack in ustack.
  for(argc = 0; argv[argc] != 0; ++argc) {
    if(argc >= MAXARG)
      goto bad;
    sp = (sp - (strlen(argv[argc]) + 1)) & ~(sizeof(uintp)-1);
    if(copyout(pgdir, sp, argv[argc], strlen(argv[argc]) + 1) < 0)
      goto bad;
    ustack[4+argc] = sp;
  }
  ustack[4+argc] = 0;

  ustack[0] = 0xffffffff;  // fake return PC
  ustack[1] = argc;
  ustack[2] = sp - (argc+1+nr_envs+1)*sizeof(uintp);  // argv pointer
  ustack[3] = sp - (nr_envs+1)*sizeof(uintp);  // env pointer
#if X64
  myproc()->tf->rdi = argc;
  myproc()->tf->rsi = sp - (nr_args+1+nr_envs+1)*sizeof(uintp);
  myproc()->tf->rdx = sp - (nr_envs+1)*sizeof(uintp);
#endif

  sp -= (4+argc+1+nr_envs+1) * sizeof(uintp);
  if(copyout(pgdir, sp, ustack, (4+argc+1+nr_envs+1)*sizeof(uintp)) < 0)
    goto bad;

  // Save program name for debugging.
  for(last=s=progname; *s; s++)
    if(*s == '/')
      last = s+1;
  safestrcpy(curproc->name, last, sizeof(curproc->name));

  // Commit to the user image.
  oldpgdir = curproc->pgdir;
  curproc->pgdir = pgdir;
  curproc->sz = sz;
  curproc->tf->eip = elf->entry;  // main
  curproc->tf->esp = sp;
  switchuvm(curproc);
  freevm(oldpgdir);
  *ppgdir = 0;  // Clear pgdir to avoid to be free
  return 0;

bad:
  *ppgdir = pgdir;
  return -1;
}

int
execshebang(const char *path, const char * const *argv, const char *envp[],
            struct inode **pip, pde_t **ppgdir)
{
  struct inode *ip = *pip;

  // Check shebang
  char line[512];
  int size = readi(ip, line, 0, sizeof(line));

  if (size <= 2)
    goto bad;
  if (strncmp(line, "#!", 2) != 0)
    goto bad;

  iunlockput(ip);
  end_op();
  ip = *pip = 0;

  line[sizeof(line) - 1] = '\n';
  *strchr(line, '\n') = '\0';
  char* shebang = line + 2;

  begin_op();
  struct inode *ip2 = find_path(shebang, 0, 0);
  if(ip2 == 0){
    end_op();
    return -1;
  }
  ilock(ip2);
  *pip = ip2;

  struct elfhdr elf;
  if (!readelfhdr(ip2, &elf))
    goto bad;

  const char *argv2[MAXARG];
  argv2[0] = shebang;
  argv2[1] = path;
  for (int i = 1; i < MAXARG - 1; ++i) {
    argv2[i + 1] = argv[i];
    if (argv[i] == 0)
      break;
  }
  return execelf(path, shebang, argv2, envp, &elf, pip, ppgdir);

bad:
  return -1;
}

int
execve(const char *path, const char*argv[], const char *envp[])
{
  struct inode *ip;
  struct elfhdr elf;
  pde_t *pgdir;
  char exepath[128];

  pgdir = 0;

  begin_op();

  ip = find_path(path, exepath, sizeof(exepath));
  if(ip == 0){
    end_op();
    return -1;
  }
  ilock(ip);

  int result;
  if (readelfhdr(ip, &elf)) {
    result = execelf(path, path, argv, envp, &elf, &ip, &pgdir);
  } else {
    result = execshebang(exepath, argv, envp, &ip, &pgdir);
  }

  if(pgdir)
    freevm(pgdir);
  if(ip){
    iunlockput(ip);
    end_op();
  }
  return result;
}
