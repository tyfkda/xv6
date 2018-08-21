#include "types.h"
#include "param.h"
#include "memlayout.h"
#include "mmu.h"
#include "proc.h"
#include "defs.h"
#include "x86.h"
#include "elf.h"
#include "fs.h"

#define FILE_SEPARATOR  '/'

#define EXECVE_USTK_ARGS_NR       (4)
#define EXECVE_SHEBANG_STR       "#!"
#define EXECVE_SHEBANG_STRLEN     (2)

static int
execelf(const char *path, const char *argv[], const char *envp[]){
  const char *s, *last;
  int   i, off;
  uintp sz, sp, ustack[EXECVE_USTK_ARGS_NR+MAXARG+1+MAXENV+1];
  uintp arg_idx, nr_args, env_idx, nr_envs;
  uintp uargvp, uenvp;
  struct elfhdr elf;
  struct inode *ip;
  struct proghdr ph;
  pde_t *pgdir, *oldpgdir;
  struct proc *curproc = myproc();

  begin_op();

  ip = namei(path);
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
    if(copyout(pgdir, sp, argv[nr_args - arg_idx - 1], 
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

static int
execshebang(const char *path, const char *argv[], const char *envp[])
{
  struct inode          *ip;
  char            line[512];
  int                  size;
  char            * shebang;
  const char *argv2[MAXARG];
  int                     i;

  /*
   *  Load shebang line from the first block
   */
  begin_op();

  ip = namei(path);
  if(ip == 0){
    end_op();
    return -1;
  }

  ilock(ip);
  size = readi(ip, line, 0, sizeof(line));
  iunlockput(ip);

  end_op();

  if (size <= 2)
    return -1;
  if (strncmp(line, "#!", 2) != 0)
    return -1;

  line[sizeof(line) - 1] = '\n';
  *strchr(line, '\n') = '\0';

  /*
   * Setup argment
   */
  shebang = line + 2;
  argv2[0] = shebang;
  argv2[1] = path;
  for (i = 1; i < MAXARG - 1; ++i) {

    argv2[i + 1] = argv[i];
    if (argv[i] == 0)
      break;
  }

  /*
   * Invoking execve to handle user program again.
   * It is needed to invoke shebang using shell command
   * like #!/bin/script.sh
   * Note that script.sh might contain shebang(#!).
   */
  return execve(argv2[0], argv2, envp);
}

int
execve(const char *path, const char *argv[], const char *envp[])
{
  int               rc;

  rc = execelf(path, argv, envp);
  if ( rc == 0 )
    return 0;
  rc = execshebang(path, argv,envp);
  if ( rc != 0 )
    return 0;

  return -1;
}
