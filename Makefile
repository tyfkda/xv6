-include local.mk

ifeq ("$(X32)","")
BITS = 64
XOBJS = obj/knl/vm64.o
XFLAGS = -m64 -DX64 -mcmodel=kernel -mtls-direct-seg-refs -mno-red-zone
LDFLAGS = -m elf_x86_64 -nodefaultlibs
QEMUTARGET = qemu-system-x86_64
else
XFLAGS = -m32
LDFLAGS = -m elf_i386 -nodefaultlibs
QEMUTARGET = qemu-system-i386
endif

#OPT ?= -O0
OPT ?= -O2

OBJS := \
	obj/knl/bio.o\
	obj/knl/commonstr.o\
	obj/knl/console.o\
	obj/knl/exec.o\
	obj/knl/file.o\
	obj/knl/fs.o\
	obj/knl/ide.o\
	obj/knl/input.o\
	obj/knl/ioapic.o\
	obj/knl/ioctl.o\
	obj/knl/kalloc.o\
	obj/knl/kbd.o\
	obj/knl/lapic.o\
	obj/knl/log.o\
	obj/knl/main.o\
	obj/knl/mp.o\
	obj/knl/acpi.o\
	obj/knl/picirq.o\
	obj/knl/pipe.o\
	obj/knl/proc.o\
	obj/knl/sleeplock.o\
	obj/knl/spinlock.o\
	obj/knl/sprintf.o\
	obj/knl/swtch$(BITS).o\
	obj/knl/syscall.o\
	obj/knl/sysfile.o\
	obj/knl/sysproc.o\
	obj/knl/trapasm$(BITS).o\
	obj/knl/trap.o\
	obj/knl/uart.o\
	obj/knl/vectors.o\
	obj/knl/vm.o\
	$(XOBJS)

ifneq ("$(MEMFS)","")
# build filesystem image in to kernel and use memory-ide-device
# instead of mounting the filesystem on ide1
#OBJS := $(filter-out obj/knl/ide.o,$(OBJS)) obj/knl/memide.o
FSIMAGE := fs.img
endif

# Cross-compiling (e.g., on Mac OS X)
#TOOLPREFIX = i386-jos-elf-

# Using native tools (e.g., on X86 Linux)
#TOOLPREFIX =

# Try to infer the correct TOOLPREFIX if not set
ifndef TOOLPREFIX
TOOLPREFIX := $(shell if i386-jos-elf-objdump -i 2>&1 | grep '^elf32-i386$$' >/dev/null 2>&1; \
	then echo 'i386-jos-elf-'; \
	elif objdump -i 2>&1 | grep 'elf32-i386' >/dev/null 2>&1; \
	then echo ''; \
	else echo "***" 1>&2; \
	echo "*** Error: Couldn't find an i386-*-elf version of GCC/binutils." 1>&2; \
	echo "*** Is the directory with i386-jos-elf-gcc in your PATH?" 1>&2; \
	echo "*** If your i386-*-elf toolchain is installed with a command" 1>&2; \
	echo "*** prefix other than 'i386-jos-elf-', set your TOOLPREFIX" 1>&2; \
	echo "*** environment variable to that prefix and run 'make' again." 1>&2; \
	echo "*** To turn off this error, run 'gmake TOOLPREFIX= ...'." 1>&2; \
	echo "***" 1>&2; exit 1; fi)
endif

# If the makefile can't find QEMU, specify its path here
#QEMU =

# Try to infer the correct QEMU
ifndef QEMU
QEMU = $(shell if which qemu > /dev/null; \
	then echo qemu; exit; \
	elif which $(QEMUTARGET) > /dev/null; \
	then echo $(QEMUTARGET); exit; \
	else \
	qemu=/Applications/Q.app/Contents/MacOS/i386-softmmu.app/Contents/MacOS/i386-softmmu; \
	if test -x $$qemu; then echo $$qemu; exit; fi; fi; \
	echo "***" 1>&2; \
	echo "*** Error: Couldn't find a working QEMU executable." 1>&2; \
	echo "*** Is the directory containing the qemu binary in your PATH" 1>&2; \
	echo "*** or have you tried setting the QEMU variable in Makefile?" 1>&2; \
	echo "***" 1>&2; exit 1)
endif

CC = $(TOOLPREFIX)gcc
AS = $(TOOLPREFIX)gas
LD = $(TOOLPREFIX)ld
OBJCOPY = $(TOOLPREFIX)objcopy
OBJDUMP = $(TOOLPREFIX)objdump
CFLAGS = -fno-pic -static -fno-builtin -fno-strict-aliasing -Wall -MD -ggdb -Werror -fno-omit-frame-pointer
CFLAGS += -ffreestanding -fno-common -nostdlib -Iinclude -gdwarf-2 $(XFLAGS) $(OPT)
CFLAGS += $(shell $(CC) -fno-stack-protector -E -x c /dev/null >/dev/null 2>&1 && echo -fno-stack-protector)
CFLAGS += -nostdinc
ASFLAGS = -fno-pic -gdwarf-2 -Wa,-divide -Iinclude $(XFLAGS)

xv6.img: out/bootblock out/kernel.elf
	dd if=/dev/zero of=$@ count=10000
	dd if=out/bootblock of=$@ conv=notrunc
	dd if=out/kernel.elf of=$@ seek=1 conv=notrunc

xv6memfs.img: out/bootblock out/kernelmemfs.elf
	dd if=/dev/zero of=$@ count=10000
	dd if=out/bootblock of=$@ conv=notrunc
	dd if=out/kernelmemfs.elf of=$@ seek=1 conv=notrunc

# kernel object files
obj/knl/%.o: kernel/%.c
	@mkdir -p obj/knl
	$(CC) $(CFLAGS) -c -o $@ $<

obj/knl/%.o: commonsrc/%.c
	@mkdir -p obj/knl
	$(CC) $(CFLAGS) -c -o $@ $<

obj/knl/%.o: kernel/%.S
	@mkdir -p obj/knl
	$(CC) $(ASFLAGS) -c -o $@ $<

# userspace object files
obj/user/%.o: user/%.c
	@mkdir -p obj/user
	$(CC) $(CFLAGS) -c -o $@ $<

obj/ulib/%.o: ulib/%.c
	@mkdir -p obj/ulib
	$(CC) $(CFLAGS) -c -o $@ $<

obj/ulib/%.o: commonsrc/%.c
	@mkdir -p obj/ulib
	$(CC) $(CFLAGS) -c -o $@ $<

obj/ulib/%.o: ulib/%.S
	@mkdir -p obj/ulib
	$(CC) $(ASFLAGS) -c -o $@ $<

out/bootblock: kernel/bootasm.S kernel/bootmain.c
	@mkdir -p out
	$(CC) -fno-builtin -fno-pic -m32 -nostdinc -Iinclude -O -o out/bootmain.o -c kernel/bootmain.c
	$(CC) -fno-builtin -fno-pic -m32 -nostdinc -Iinclude -o out/bootasm.o -c kernel/bootasm.S
	$(LD) -m elf_i386 -nodefaultlibs -N -e start -Ttext 0x7C00 -o out/bootblock.o out/bootasm.o out/bootmain.o
	$(OBJDUMP) -S out/bootblock.o > out/bootblock.asm
	$(OBJCOPY) -S -O binary -j .text out/bootblock.o $@
	tools/sign.pl $@

out/entryother: kernel/entryother.S
	@mkdir -p out
	$(CC) $(CFLAGS) -fno-pic -nostdinc -I. -o out/entryother.o -c kernel/entryother.S
	$(LD) $(LDFLAGS) -N -e start -Ttext 0x7000 -o out/bootblockother.o out/entryother.o
	$(OBJCOPY) -S -O binary -j .text out/bootblockother.o $@
	$(OBJDUMP) -S out/bootblockother.o > out/entryother.asm

INITCODESRC = kernel/initcode$(BITS).S
out/initcode: $(INITCODESRC)
	@mkdir -p out
	$(CC) $(CFLAGS) -nostdinc -I. -o out/initcode.o -c $(INITCODESRC)
	$(LD) $(LDFLAGS) -N -e start -Ttext 0 -o out/initcode.out out/initcode.o
	$(OBJCOPY) -S -O binary out/initcode.out $@
	$(OBJDUMP) -S out/initcode.o > out/initcode.asm

ENTRYCODE = obj/knl/entry$(BITS).o
LINKSCRIPT = kernel/kernel$(BITS).ld
out/kernel.elf: $(OBJS) $(ENTRYCODE) out/entryother out/initcode $(LINKSCRIPT) $(FSIMAGE)
	$(LD) $(LDFLAGS) -T $(LINKSCRIPT) -o $@ $(ENTRYCODE) $(OBJS) -b binary out/initcode out/entryother $(FSIMAGE)
	$(OBJDUMP) -S $@ > out/kernel.asm
	$(OBJDUMP) -t $@ | sed '1,/SYMBOL TABLE/d; s/ .* / /; /^$$/d' > out/kernel.sym

# kernelmemfs is a copy of kernel that maintains the
# disk image in memory instead of writing to a disk.
# This is not so useful for testing persistent storage or
# exploring disk buffering implementations, but it is
# great for testing the kernel on real hardware without
# needing a scratch disk.
MEMFSOBJS = $(filter-out obj/knl/ide.o,$(OBJS)) obj/knl/memide.o
out/kernelmemfs.elf: $(MEMFSOBJS) $(ENTRYCODE) out/entryother out/initcode $(LINKSCRIPT) fs.img
	$(LD) $(LDFLAGS) -T $(LINKSCRIPT) -o $@ $(ENTRYCODE)  $(MEMFSOBJS) -b binary out/initcode out/entryother fs.img
	$(OBJDUMP) -S $@ > out/kernelmemfs.asm
	$(OBJDUMP) -t $@ | sed '1,/SYMBOL TABLE/d; s/ .* / /; /^$$/d' > out/kernelmemfs.sym

MKVECTORS = tools/vectors$(BITS).pl
kernel/vectors.S: $(MKVECTORS)
	perl $(MKVECTORS) > $@

ULIBOBJS = \
	obj/ulib/atexit.o\
	obj/ulib/commonstr.o\
	obj/ulib/crt0.o\
	obj/ulib/ctype.o\
	obj/ulib/dirent.o\
	obj/ulib/environ.o\
	obj/ulib/errno.o\
	obj/ulib/exec.o\
	obj/ulib/exit.o\
	obj/ulib/localtime.o\
	obj/ulib/perror.o\
	obj/ulib/printf.o\
	obj/ulib/setjmp$(BITS).o\
	obj/ulib/sprintf.o\
	obj/ulib/stdio.o\
	obj/ulib/string.o\
	obj/ulib/termios.o\
	obj/ulib/time.o\
	obj/ulib/ulib.o\
	obj/ulib/umalloc.o\
	obj/ulib/usys.o\
	obj/ulib/wait.o\

obj/ulib/ulib.a:	$(ULIBOBJS)
	ar rcs $@ $^

fs/bin/%: obj/user/%.o obj/ulib/ulib.a
	@mkdir -p fs out fs/bin
	$(LD) $(LDFLAGS) -N -e _start -Ttext 0 -o $@ $^
	$(OBJDUMP) -S $@ > out/$*.asm
	$(OBJDUMP) -t $@ | sed '1,/SYMBOL TABLE/d; s/ .* / /; /^$$/d' > out/$*.sym
	strip $@

out/mkfs: tools/mkfs.c tools/hostfsaux.c tools/hostfsaux.h kernel/fs.h
	@mkdir -p out
	gcc -Werror -Wall -o $@ tools/mkfs.c tools/hostfsaux.c

# Prevent deletion of intermediate files, e.g. cat.o, after first build, so
# that disk image changes after first build are persistent until clean.  More
# details:
# http://www.gnu.org/software/make/manual/html_node/Chained-Rules.html
.PRECIOUS: obj/ulib/%.o, obj/user/%.o

UPROGS=\
	fs/bin/cat\
	fs/bin/cp\
	fs/bin/date\
	fs/bin/echo\
	fs/bin/forktest\
	fs/bin/grep\
	fs/bin/init\
	fs/bin/kill\
	fs/bin/ln\
	fs/bin/ls\
	fs/bin/mkdir\
	fs/bin/pwd\
	fs/bin/rm\
	fs/bin/sh\
	fs/bin/stressfs\
	fs/bin/usertests\
	fs/bin/wc\
	fs/bin/zombie\

copyfsdata:
	@mkdir -p fs
	cp -upr fsdata/* fs/

fs.img: out/mkfs $(UPROGS) copyfsdata
	out/mkfs $@ init
	out/mkfs $@ put fs/* /

-include obj/*/*.d

clean:
	rm -rf out fs obj
	rm -f kernel/vectors.S xv6.img xv6memfs.img fs.img .gdbinit

# run in emulators

bochs : fs.img xv6.img
	if [ ! -e .bochsrc ]; then ln -s tools/dot-bochsrc .bochsrc; fi
	bochs -q

# try to generate a unique GDB port
GDBPORT = $(shell expr `id -u` % 5000 + 25000)
# QEMU's gdb stub command line changed in 0.11
QEMUGDB = $(shell if $(QEMU) -help | grep -q '^-gdb'; \
	then echo "-gdb tcp::$(GDBPORT)"; \
	else echo "-s -p $(GDBPORT)"; fi)
ifndef CPUS
CPUS := 2
endif
QEMUOPTS = -net none -drive file=fs.img,index=1,media=disk,format=raw -drive file=xv6.img,index=0,media=disk,format=raw -smp $(CPUS) -m 512 $(QEMUEXTRA)

qemu: fs.img xv6.img
	$(QEMU) -serial mon:stdio $(QEMUOPTS)

qemu-memfs: xv6memfs.img
	$(QEMU) xv6memfs.img -smp $(CPUS) -m 256

qemu-nox: fs.img xv6.img
	$(QEMU) -nographic $(QEMUOPTS)

.gdbinit: tools/gdbinit.tmpl
	sed "s/localhost:1234/localhost:$(GDBPORT)/" < $^ > $@

qemu-gdb: fs.img xv6.img .gdbinit
	@echo "*** Now run 'gdb'." 1>&2
	$(QEMU) -serial mon:stdio $(QEMUOPTS) -S $(QEMUGDB)

qemu-nox-gdb: fs.img xv6.img .gdbinit
	@echo "*** Now run 'gdb'." 1>&2
	$(QEMU) -nographic $(QEMUOPTS) -S $(QEMUGDB)
