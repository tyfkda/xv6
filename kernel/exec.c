#include "types.h"
#include "param.h"
#include "memlayout.h"
#include "mmu.h"
#include "proc.h"
#include "defs.h"
#include "x86.h"
#include "elf.h"

#define FILE_SEPARATOR  '/'

#define EXECVE_USTK_ARGS_NR  (4)
//#define DEBUG_SHOW_EXECVE_ENVS

// Find exe path
static struct inode *find_path(const char *path) {
  struct inode *ip;

  if (strchr(path, FILE_SEPARATOR) != 0) {
    ip = namei(path);
  } else {
    // Also find at root path. TODO: Search from $PATH
    const char PATH[] = "/bin/";
    const int LEN = sizeof(PATH) - 1;
    const int BUFSIZ = 128;
    char exepath[BUFSIZ];
    memmove(exepath, PATH, LEN);
    safestrcpy(exepath + LEN, path, BUFSIZ - LEN);
    ip = namei(exepath);
  }
  return ip;
}

int
exec(const char *path, char* const *argv)
{
  const char *s, *last;
  int i, off;
  uintp argc, sz, sp, ustack[3+MAXARG+1];
  struct elfhdr elf;
  struct inode *ip;
  struct proghdr ph;
  pde_t *pgdir, *oldpgdir;
  struct proc *curproc = myproc();

  begin_op();

  ip = find_path(path);
  if(ip == 0){
    end_op();
    return -1;
  }
  ilock(ip);
  pgdir = 0;

  // Check ELF header
  if(readi(ip, &elf, 0, sizeof(elf)) != sizeof(elf))
    goto bad;
  if(elf.magic != ELF_MAGIC)
    goto bad;

  if((pgdir = setupkvm()) == 0)
    goto bad;

  // Load program into memory.
  sz = 0;
  for(i=0, off=elf.phoff; i<elf.phnum; i++, off+=sizeof(ph)){
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
  ip = 0;

  // Allocate two pages at the next page boundary.
  // Make the first inaccessible.  Use the second as the user stack.
  sz = PGROUNDUP(sz);
  if((sz = allocuvm(pgdir, sz, sz + 2*PGSIZE)) == 0)
    goto bad;
  clearpteu(pgdir, (char*)(sz - 2*PGSIZE));
  sp = sz;

  // Push argument strings, prepare rest of stack in ustack.
  for(argc = 0; argv[argc]; argc++) {
    if(argc >= MAXARG)
      goto bad;
    sp = (sp - (strlen(argv[argc]) + 1)) & ~(sizeof(uintp)-1);
    if(copyout(pgdir, sp, argv[argc], strlen(argv[argc]) + 1) < 0)
      goto bad;
    ustack[3+argc] = sp;
  }
  ustack[3+argc] = 0;

  ustack[0] = 0xffffffff;  // fake return PC
  ustack[1] = argc;
  ustack[2] = sp - (argc+1)*sizeof(uintp);  // argv pointer

#if X64
  myproc()->tf->rdi = argc;
  myproc()->tf->rsi = sp - (argc+1)*sizeof(uintp);
#endif

  sp -= (3+argc+1) * sizeof(uintp);
  if(copyout(pgdir, sp, ustack, (3+argc+1)*sizeof(uintp)) < 0)
    goto bad;

  // Save program name for debugging.
  for(last=s=path; *s; s++)
    if(*s == '/')
      last = s+1;
  safestrcpy(curproc->name, last, sizeof(curproc->name));

  // Commit to the user image.
  oldpgdir = curproc->pgdir;
  curproc->pgdir = pgdir;
  curproc->sz = sz;
  curproc->tf->eip = elf.entry;  // main
  curproc->tf->esp = sp;
  switchuvm(curproc);
  freevm(oldpgdir);
  return 0;

 bad:
  if(pgdir)
    freevm(pgdir);
  if(ip){
    iunlockput(ip);
    end_op();
  }
  return -1;
}

int
execve(const char *path, char *const argv[], char *const envp[]){
  const char *s, *last;
  int i, off;
  uintp sz, sp, ustack[EXECVE_USTK_ARGS_NR+MAXARG+1+MAXENV+1];
  uintp arg_idx, nr_args, env_idx, nr_envs;
  uintp uargvp, uenvp;
  struct elfhdr elf;
  struct inode *ip;
  struct proghdr ph;
  pde_t *pgdir, *oldpgdir;
  struct proc *curproc = myproc();

  begin_op();

  ip = find_path(path);
  if( ip == 0 ){

    end_op();
    return -1;
  }

  ilock(ip);
  pgdir = 0;

  // Check ELF header
  if(readi(ip, &elf, 0, sizeof(elf)) != sizeof(elf))
    goto bad;
  if(elf.magic != ELF_MAGIC)
    goto bad;

  if((pgdir = setupkvm()) == 0)
    goto bad;

  // Load program into memory.
  sz = 0;
  for(i=0, off=elf.phoff; i<elf.phnum; i++, off+=sizeof(ph)){
    if(readi(ip, &ph, off, sizeof(ph)) != sizeof(ph))
      goto bad;
    if(ph.type != ELF_PROG_LOAD)
      continue;
    if(ph.memsz < ph.filesz)
      goto bad;
    if(ph.vaddr + ph.memsz < ph.vaddr)
      goto bad;
    if(ph.vaddr % PGSIZE != 0)
      goto bad;
    if((sz = allocuvm(pgdir, sz, PGROUNDUP(ph.vaddr + ph.memsz))) == 0)
      goto bad;
    if(loaduvm(pgdir, (char*)ph.vaddr, ip, ph.off, ph.filesz) < 0)
      goto bad;
  }
  iunlockput(ip);
  end_op();
  ip = 0;

  // Allocate two pages at the next page boundary.
  // Make the first inaccessible.  Use the second as the user stack.
  sz = PGROUNDUP(sz);
  if((sz = allocuvm(pgdir, sz, sz + 2*PGSIZE)) == 0)
    goto bad;
  clearpteu(pgdir, (char*)(sz - 2*PGSIZE));
  sp = sz;

  for(nr_args = 0; (MAXARG > nr_args) && (argv[nr_args] != 0); ++nr_args);
  for(nr_envs = 0; (MAXENV > nr_envs) && (envp[nr_envs] != 0); ++nr_envs);
  
  /*
   * Push environment strings, prepare rest of stack in ustack.
   */
  for(env_idx = 0; nr_envs > env_idx; ++env_idx) {

    /* Copy environment variable including NULL terminate
     * and we ensure each element is aligned to the word.
     */
    sp = (sp - (strlen(envp[nr_envs - env_idx - 1]) + 1)) & ~(sizeof(uintp)-1);
    if (copyout(pgdir, sp, envp[nr_envs - env_idx - 1], 
                strlen(envp[nr_envs - env_idx - 1]) + 1) < 0)
      goto bad;
    
    /* store the address of a variable */		
    ustack[EXECVE_USTK_ARGS_NR + nr_args + 1 + nr_envs - env_idx - 1] = sp; 
  }
  ustack[EXECVE_USTK_ARGS_NR + nr_args + 1 + nr_envs] = (uintp)0;
  
  /*
   * Push argument strings, prepare rest of stack in ustack.
   */
  for(arg_idx = 0; nr_args > arg_idx; ++arg_idx) {
    
    /* Copy an argument including NULL terminate
     * and we ensure each element is aligned to the word.
     */
    sp = (sp - (strlen(argv[nr_args - arg_idx - 1]) + 1)) & ~(sizeof(uintp)-1);
    if(copyout(pgdir, sp, 
               argv[nr_args - arg_idx - 1], 
               strlen(argv[nr_args - arg_idx - 1]) + 1) < 0)
      goto bad;
    ustack[EXECVE_USTK_ARGS_NR + nr_args - arg_idx - 1] = sp;
  }
  ustack[EXECVE_USTK_ARGS_NR+nr_args] = (uintp)0;

  uargvp = sp - (nr_args + 1 + nr_envs + 1)*sizeof(uintp);  // argv pointer
  uenvp = sp - (nr_envs + 1)*sizeof(uintp);  // env pointer
  
  ustack[0] = 0xffffffff;  // fake return PC
  ustack[1] = nr_args;
  ustack[2] = uargvp;  // argv pointer
  ustack[3] = uenvp;   // env pointer
#if X64
  myproc()->tf->rdi = nr_args;
  myproc()->tf->rsi = uargvp; // argv pointer
  myproc()->tf->rdx = uenvp;  // env pointer
#endif

  sp -= (EXECVE_USTK_ARGS_NR + nr_args + 1 + nr_envs + 1) * sizeof(uintp);
  if(copyout(pgdir, sp, ustack, (EXECVE_USTK_ARGS_NR + nr_args + 1 + nr_envs + 1)*sizeof(uintp)) < 0)
    goto bad;

#if defined(DEBUG_SHOW_EXECVE_ENVS)
  cprintf("kernel execve: sp=%p\n", sp);
  for(i = 0; argv[i] != 0; ++i)
    cprintf("kernel execve: user argv addr=%p kernel argv[%d]=%s\n", 
            uargvp, i, argv[i]);
  for(i = 0; envp[i] != 0; ++i)
    cprintf("kernel execve: user environment addr=%p kernel envp[%d]=%s\n", 
            uenvp, i, envp[i]);
#endif 


  // Save program name for debugging.
  for(last=s=path; *s; s++)
    if(*s == '/')
      last = s+1;
  safestrcpy(curproc->name, last, sizeof(curproc->name));

  // Commit to the user image.
  oldpgdir = curproc->pgdir;
  curproc->pgdir = pgdir;
  curproc->sz = sz;
  curproc->tf->eip = elf.entry;  // main
  curproc->tf->esp = sp;
  switchuvm(curproc);
  freevm(oldpgdir);

  return 0;

 bad:
  if( pgdir != 0 )
    freevm(pgdir);
  if( ip != 0 ){
    iunlockput(ip);
    end_op();
  }
  return -1;
}
