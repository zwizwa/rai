#include "proc.h"
extern const struct proc_class synth;
extern const struct proc_class test_pd;
const struct proc_class *proc_library[] = {
    &synth,
    &test_pd,
    NULL
};
