ENTRY(_start)

MEMORY {
    rom (rx) : ORIGIN = 0x1000, LENGTH = 1024K
    ram (rw) : ORIGIN = 0x10000, LENGTH = 1024K
}

SECTIONS {
    .text : {
        *(.text.startup .text.startup.*)
        *(.text)

        *(.rodata*)

        . = ALIGN(16);
        __preinit_array_start = .;
        KEEP (*(.preinit_array))
        __preinit_array_end = .;

        __init_array_start = .;
        KEEP (*(SORT(.init_array.*)))
        KEEP (*(.init_array))
        __init_array_end = .;

        __fini_array_start = .;
        KEEP (*(SORT(.fini_array.*)))
        KEEP (*(.fini_array))
        __fini_array_end = .;
        . = ALIGN(16);
    } > rom

    .fini           :
    {
        KEEP (*(SORT_NONE(.fini)))
    }

    .data : {
        *(.data)
    } > ram

    .bss : {
        *(.bss)
        *(.bss.*)
        *(COMMON)
    } > ram
}
