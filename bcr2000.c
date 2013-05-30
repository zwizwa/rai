#include <stdio.h>
#include <stdlib.h>

// Quick & dirty Behringer BCR-2000 MIDI parser.
// Preset 2 is relatively predictable: 8 channel operation for all knobs, all 7 bit.
// row cc
// ----------
// 1   10  8   // 4 groups
//     12 13
// 2   71
// 3   74
// 4   7

const char *cc[128] = {
    [10] = "a", // top row
     [8] = "b",
    [12] = "c",
    [13] = "d",

    [71] = "e", // bottom 3 rows
    [74] = "f",
     [7] = "g",
};


unsigned char byte(void) {
    int c = fgetc(stdin);
    unsigned char b = c;
    fprintf(stderr, "byte %02x\n", b);
    return b;
}


void midiroute(void) {
    unsigned char chan;
    unsigned char cmd;
    unsigned char num;
    unsigned char val;
  next:
    do { cmd = byte(); } while (!(cmd & 0x80));
    chan = cmd & 0x0F;
    cmd  = cmd & 0xF0;
    switch(cmd) {
    case 0xc0:
        val = byte();
        // fprintf(stderr, "pc %d %d\n", chan, val);
        // Send a Pd style message.
        fprintf(stdout, "b_%d 1;\n", val);
        fflush(stdout);
        break;
    case 0xb0:
        num = byte();
        val = byte();
        if (!cc[num]) {
            fprintf(stderr, "cc %d %d %d\n", num, chan, val);
        }
        else {
            // Send a Pd style message.
            fprintf(stdout, "%s%d %d;\n", cc[num], chan, val);
            fflush(stdout);
        }
        break;
    default:
        fprintf(stderr, "? %02x\n", cmd);
        break;
    }
    goto next;
}

int main(void) {
    midiroute();
    return 0;
}
