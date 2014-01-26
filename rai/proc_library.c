// TODO: Statically embed library of proc classes in C program.
// gcc -std=gnu99 proc_library.c *.proc.o
#include "proc.h"
extern struct proc_class synth;
extern struct proc_class test_pd;
struct proc_class *proc_library[] = {
    &synth,
    &test_pd,
    NULL
};


// FIXME
void main(void){
}
