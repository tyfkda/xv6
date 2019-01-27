
CFLAGS += -m64 -O2 -nostdinc -Wall -Werror

hello:	hello.o
	ld -nodefaultlibs -e _start -o $@ $<

clean:
	rm -rf *.o hello hello_world_elfout hello hello_world_elfout64 elf


hello_world_elfout: hello_world_elfout.c
	gcc -m32 -Wall -o $@ $<

hello_world_elfout64: hello_world_elfout64.c
	gcc -m64 -Wall -o $@ $<

gen-elf: hello_world_elfout
	./hello_world_elfout > elf
	chmod +x elf

gen-elf64: hello_world_elfout64
	./hello_world_elfout64 > elf
	chmod +x elf
