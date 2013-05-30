MEMORY  {
    /* All addresses are file-relative. */
    file : ORIGIN = 0, LENGTH = 0x10000000
}

SECTIONS {
    . = 0;
    .text : {
        /* Header is first, the rest is only code and constants.
           The order doesn't matter as there are offsets stored 
           in the header. */
        *(.header*)
        *(.text*)
        *(.rodata*)
        *(.data.rel.ro.local*)
    } >file

    /* To silence warnings.  Only .text segment is of interest. */
    .eh_frame : {
        *(.eh_frame*)
    } >file
}
