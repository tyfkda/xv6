OUTPUT_FORMAT(elf64-x86-64)
ENTRY(_start)

MEMORY {
    rom (rx) : ORIGIN = 0, LENGTH = 1024K
    ram (rwx) : ORIGIN = 64K, LENGTH = 1024K
}

SECTIONS {
    .text : {
        *(.text.startup .text.startup.*)
        *(.text)
    } > rom

    .data : {
        *(.data)
        *(.rodata*)

        . = ALIGN(16);
        __preinit_array_start = .;
        *(.preinit_array)
        __preinit_array_end = .;

        __init_array_start = .;
        *(SORT(.init_array.*))
        *(.init_array)
        __init_array_end = .;

        __fini_array_start = .;
        *(SORT(.fini_array.*))
        *(.fini_array)
        __fini_array_end = .;
        . = ALIGN(16);
    } > ram AT > rom
}
