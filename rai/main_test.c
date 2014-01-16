// Example for including .g.h file

#include "prim.h"  // primitive functions
#include "rai.h"   // tools
#include PROC_FILE // generated code

#include <stdlib.h>
#include <stdio.h>

#define NB_SAMP 16

_ *new_array(int n, _ init) {
    _ *a = malloc(sizeof(_) * n);
    for (int i = 0; i < n; i++) a[i] = init;
    return a;
}


static int    nb_arg;
static char **arg;
_ in_default(int i) {
    _ f = ((_)i) / ((_)(proc_size_in + proc_size_param));
    _ v = (i < nb_arg) ? atof(arg[i]) : f;
    printf("in[%d] = %f\n", i, v);
    return v;
}

int main(int argc, char **argv) {
    nb_arg = argc-1;
    arg    = argv+1;

    /* Alloc buffers */
    float **state = malloc(sizeof(*state) * 2);
    state[0] = new_array(PROC(size_state), 0);
    state[1] = new_array(PROC(size_state), 0);

    _ **in    = malloc(sizeof(*in) * PROC(size_in));
    for (int i = 0; i < PROC(size_in); i++)
        in[i]  = new_array(NB_SAMP, in_default(i));

    _ *param = new_array(PROC(size_param), 0);
    for (int i = 0; i < PROC(size_param); i++)
        param[i] = in_default(i);

    _ **out   = malloc(sizeof(*out) * PROC(size_out));
    for (int i = 0; i < PROC(size_out); i++)
        out[i] = new_array(NB_SAMP, 0);

    _ *store = new_array(PROC(size_store), 0);

    /* Run once */
    PROC(loop)((void*)state, (void*)in, (void*)param, (void*)out, (void*)store, NB_SAMP);
    for (int i = 0; i < NB_SAMP; i++) {
        for (int o = 0; o < PROC(size_out); o++) {
            printf("%+0.4f ", out[o][i]);
        }
        printf("\n");
    }

    return 0;
}
