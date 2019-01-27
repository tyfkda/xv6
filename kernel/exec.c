#include "types.h"
#include "param.h"
#include "memlayout.h"
#include "mmu.h"
#include "proc.h"
#include "defs.h"
#include "x86.h"
#include "elf.h"

// Check ELF header
int
readelfhdr(struct inode *ip, struct elfhdr *elf)
{
  return readi(ip, elf, 0, sizeof(*elf)) == sizeof(*elf)
    && elf->e_ident[0] == ELFMAG0
    && elf->e_ident[1] == ELFMAG1
    && elf->e_ident[2] == ELFMAG2
    && elf->e_ident[3] == ELFMAG3;
}

int
execelf(const char *progname, const char* const *argv, const char *envp[],
        const struct elfhdr *elf, struct inode **pip, pde_t **ppgdir)
{
#if X64
#define SPIDX  (1)
#else
#define SPIDX  (4)
#endif
  const char *s, *last;
  int i, off;
  uintp argc, envc, sp, ustack[SPIDX+MAXARG+1+MAXENV+1];
  uintp textstart, textend, datastart, dataend;
  struct inode *ip;
  struct proghdr ph;
  pde_t *pgdir, *oldpgdir;
  struct proc *curproc = myproc();

  ip = *pip;
  pgdir = 0;

  if((pgdir = setupkvm()) == 0)
    goto bad;

  // Load program into memory.
  textstart = textend = datastart = dataend = 0;
  for(i=0, off=elf->e_phoff; i<elf->e_phnum; i++, off+=sizeof(ph)){
    if(readi(ip, &ph, off, sizeof(ph)) != sizeof(ph))
      goto bad;
    if(ph.p_type != PT_LOAD)
      continue;
    if(ph.p_memsz < ph.p_filesz)
      goto bad;
    if(ph.p_vaddr % PGSIZE != 0)
      goto bad;
    if(ph.p_vaddr + ph.p_memsz < ph.p_vaddr)
      goto bad;

    uintp *psz;
    if (ph.p_flags & PF_X) {  // Executable: .text
      if (textend != 0)
        goto bad;
      textstart = textend = ph.p_vaddr;
      psz = &textend;
    } else {  // Other: .data
      if (dataend != 0)
        goto bad;
      datastart = dataend = ph.p_vaddr;
      psz = &dataend;
    }

    uintp start = *psz, end;
    if((*psz = end = allocuvm(pgdir, start, ph.p_vaddr + ph.p_memsz)) == 0)
      goto bad;
    if(loaduvm(pgdir, (char*)ph.p_vaddr, ip, ph.p_offset, ph.p_filesz) < 0)
      goto bad;

    if (!(ph.p_flags & PF_W)) {
      setpteflags(pgdir, start, end, PTE_U);  // Drop PTE_W flag.
    }
  }
  iunlockput(ip);
  end_op();
  ip = *pip = 0;

  // Allocate user stack
  sp = USTACKBOTTOM;
  if((allocuvm(pgdir, USTACKTOP, USTACKBOTTOM)) == 0)
    goto bad;

  // Push argument strings, prepare rest of stack in ustack.
  for(argc = 0; argv[argc] != 0; ++argc) {
    if(argc >= MAXARG)
      goto bad;
    const char* arg = argv[argc];
    int len = strlen(arg);
    sp -= len + 1;
    if(copyout(pgdir, sp, arg, len + 1) < 0)
      goto bad;
    ustack[SPIDX+argc] = sp;
  }
  ustack[SPIDX+argc] = 0;

  envc = 0;
  if (envp != 0) {
    for(; envp[envc] != 0; ++envc) {
      if(envc >= MAXENV)
        goto bad;
      const char* env = envp[envc];
      int len = strlen(env);
      sp -= len + 1;
      if (copyout(pgdir, sp, env, len + 1) < 0)
        goto bad;

      // store the address of a variable
      ustack[SPIDX + argc + 1 + envc] = sp;
    }
  }
  ustack[SPIDX + argc + 1 + envc] = 0;

  sp &= ~(sizeof(uintp)-1);

  ustack[0] = 0xffffffff;  // fake return PC
#if !X64
  ustack[1] = argc;
  ustack[2] = sp - (argc+1+envc+1)*sizeof(uintp);  // argv pointer
  ustack[3] = sp - (envc+1)*sizeof(uintp);  // env pointer
#else
  myproc()->tf->rdi = argc;
  myproc()->tf->rsi = sp - (argc+1+envc+1)*sizeof(uintp);
  myproc()->tf->rdx = sp - (envc+1)*sizeof(uintp);
#endif

  sp -= (SPIDX+argc+1+envc+1) * sizeof(uintp);
  if(copyout(pgdir, sp, ustack, (SPIDX+argc+1+envc+1)*sizeof(uintp)) < 0)
    goto bad;

  // Save program name for debugging.
  for(last=s=progname; *s; s++)
    if(*s == '/')
      last = s+1;
  safestrcpy(curproc->name, last, sizeof(curproc->name));

  // Commit to the user image.
  oldpgdir = curproc->pgdir;
  curproc->pgdir = pgdir;
  curproc->textstart = textstart;
  curproc->textend = textend;
  curproc->datastart = datastart;
  curproc->dataend = dataend;
  curproc->tf->eip = elf->e_entry;  // main
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
  struct inode *ip2 = namei(shebang);
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
  return execelf(path, argv2, envp, &elf, pip, ppgdir);

bad:
  return -1;
}

int
execve(const char *path, const char*argv[], const char *envp[])
{
  struct inode *ip;
  struct elfhdr elf;
  pde_t *pgdir;

  pgdir = 0;

  begin_op();

  ip = namei(path);
  if(ip == 0){
    end_op();
    return -1;
  }
  ilock(ip);

  int result;
  if (readelfhdr(ip, &elf)) {
    result = execelf(path, argv, envp, &elf, &ip, &pgdir);
  } else {
    result = execshebang(path, argv, envp, &ip, &pgdir);
  }

  if(pgdir)
    freevm(pgdir);
  if(ip){
    iunlockput(ip);
    end_op();
  }
  return result;
}
