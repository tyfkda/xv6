// Format of an ELF executable file

#define ELFMAG0  (0x7f)
#define ELFMAG1  ('E')
#define ELFMAG2  ('L')
#define ELFMAG3  ('F')

// File header
struct elfhdr {
  uchar e_ident[16];
  ushort e_type;
  ushort e_machine;
  uint e_version;
  uintp e_entry;
  uintp e_phoff;
  uintp e_shoff;
  uint e_flags;
  ushort e_ehsize;
  ushort e_phentsize;
  ushort e_phnum;
  ushort e_shentsize;
  ushort e_shnum;
  ushort e_shstrndx;
};

// Program section header
#if X64
struct proghdr {
  uint32 p_type;
  uint32 p_flags;
  uint64 p_offset;
  uint64 p_vaddr;
  uint64 p_paddr;
  uint64 p_filesz;
  uint64 p_memsz;
  uint64 p_align;
};
#else
struct proghdr {
  uint p_type;
  uint p_offset;
  uint p_vaddr;
  uint p_paddr;
  uint p_filesz;
  uint p_memsz;
  uint p_flags;
  uint p_align;
};
#endif

#define ELFCLASS32  (1)
#define ELFCLASS64  (2)

#define ELFDATA2LSB  (1)
// Version
#define EV_CURRENT  (1)

#define ELFOSABI_SYSV  (0)

// Machine
#define EM_386     (3)   // Intel 80386
#define EM_X86_64  (62)  // AMD x86-64 architecture

#define ET_EXEC  (2)  // Executable file

// Values for Proghdr type
#define PT_LOAD  (1)

// Flag bits for Proghdr flags
#define PF_X  (1 << 0)
#define PF_W  (1 << 1)
#define PF_R  (1 << 2)

#ifdef X64
typedef struct elfhdr Elf64_Ehdr;
typedef struct proghdr Elf64_Phdr;
#endif
