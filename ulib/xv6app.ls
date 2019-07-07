ENTRY(_start)

MEMORY {
    rom (rx) : ORIGIN = 0x1000, LENGTH = 1024K
    ram (rw) : ORIGIN = 0x100000, LENGTH = 1024K
}

SECTIONS {
    .text : {
        *(.text.startup .text.startup.*)
        *(.text)

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
    } > rom

    .data : {
        *(.data)
    } > ram

    .bss : {
        *(.bss)
        *(.bss.*)
        *(COMMON)
    } > ram
}
