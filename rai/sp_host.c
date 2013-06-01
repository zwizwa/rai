#include "rai.h"
#include <stdio.h>


int main(int argc, char **argv) {
    if (argc != 2) {
        printf("usage: %s <plugin.sp>\n", argv[0]);
        return 1;
    }
    struct rai_info *ri = rai_load_bin(argv[1]);
    if (!ri) return 2;
    rai_print_info(ri, NULL);
}
