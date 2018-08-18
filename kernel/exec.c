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
#define EXECVE_SHEBANG_RESV_ARGS  (2)
#define EXECVE_SHEBANG_STR       "#!"
#define EXECVE_SHEBANG_STRLEN     (2)

//#define DEBUG_SHOW_EXECVE_ENVS
//#define DEBUG_SHOW_SHEBANG

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

int
exec(const char *path, const char* const *argv)
{
  return -1;  /* No longer supported */
}

int
load_elf(const char *path, const char *argv[], const char *envp[]){
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
  char exepath[128];

  begin_op();

  ip = find_path(path, exepath, sizeof(exepath));
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

int
load_shebang(const char *path, const char *argv[], const char *envp[])
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

  ip = find_path(path, 0, 0);
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
#if defined(DEBUG_SHOW_SHEBANG)
  cprintf("Shebang line:%s\n", line);
#endif  /*  DEBUG_SHOW_SHEBANG  */

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

#if defined(DEBUG_SHOW_SHEBANG)
  for(i = 0; MAXARG > i; ++i) {
    if ( argv2[i] != 0 )
      cprintf("Shebang argv[%d]:%s\n", i, argv2[i]);
    else {
      cprintf("Shebang argv[%d]:NULL\n", i);
      break;
    }
  }
#endif  /*  DEBUG_SHOW_SHEBANG  */

  /*
   * Note: Invoking execve is desirable
   * but we do not have enough kernel memory at this moment.
   */
  return load_elf(argv2[0], argv2, envp);
}

int
execve(const char *path, const char *argv[], const char *envp[])
{
  struct inode     *ip;
  char    exepath[128];
  char   exeblk[BSIZE];
  int          rd_size;
  int               rc;

  begin_op();

  ip = find_path(path, exepath, sizeof(exepath));
  if( ip == 0 ){

    end_op();
    return -1;
  }

  ilock(ip);

  rd_size = readi(ip, &exeblk[0], 0, BSIZE);
  if ( rd_size < 0 )
    goto iunlock_out;

  iunlockput(ip);
  end_op();

  if ( ( rd_size >= sizeof(struct elfhdr) ) &&
       ( ((struct elfhdr *)&exeblk[0])->magic == ELF_MAGIC ) ) {  // Check ELF header

//    cprintf("Load ELF:%s\n", path);
    rc = load_elf(path, argv, envp);
    if ( rc != 0 )
      goto error_out;
  } else if ( ( rd_size >= EXECVE_SHEBANG_STRLEN ) &&
              ( memcmp((void *)&exeblk[0], EXECVE_SHEBANG_STR , 2) == 0 ) ) {

//    cprintf("Load Shebang:%s\n", path);
    rc = load_shebang(exepath, argv,envp);
    if ( rc != 0 )
      goto error_out;
  }
  else 
    panic("execve: unknwon binary");

  return 0;

iunlock_out:
  if( ip != 0 ){

    iunlockput(ip);
    end_op();
  }

error_out:
  return -1;

}
