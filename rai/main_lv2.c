/*
 *   LV2 wrapper for rai dsp processor
 *   Copyright (c) 2013 by Tom Schouten
 */

#include "prim.h"
#include PROC_FILE // generated code
#include "rai.h"

#include <math.h>
#include <stdlib.h>
#include "lv2/lv2plug.in/ns/lv2core/lv2.h"

#define URI "http://zwizwa.be/lv2/" PROC_NAME

#define NB_PARAM  proc_size_param
#define NB_IN     proc_size_in
#define NB_OUT    proc_size_out
#define NB_STATE  proc_size_state

#if defined(proc_param_voice_freq) && \
    defined(proc_param_voice_gate) && \
    proc_param_voice_freq == 1 && \
    proc_param_voice_gate == 1
#define HAVE_SYNTH 1
#else
#define HAVE_SYNTH 0
#endif

typedef struct {
    struct proc_si state[2];
    struct proc_param param;
    struct proc_in in;
    struct proc_out out;
    struct proc_store store;
} instance;

static LV2_Handle instantiate(const LV2_Descriptor*     descriptor,
                              double                    rate,
                              const char*               bundle_path,
                              const LV2_Feature* const* features) {
    return (LV2_Handle)malloc(sizeof(instance));
}

static void connect_port(LV2_Handle handle,
                         uint32_t   port,
                         void*      data) {
    instance* x = (instance*)handle;
    if (port < NB_IN)    { ((float**)(&x->in))[port]    = data; return; } port -= NB_IN;
    if (port < NB_OUT)   { ((float**)(&x->out))[port]   = data; return; } port -= NB_OUT;
    if (port < NB_PARAM) { ((float**)(&x->param))[port] = data; return; } port -= NB_PARAM;
}

static void activate(LV2_Handle handle) {
    // FIXME: init state
    instance* x = (instance*)handle;
    int i;
    for (i=0; i<NB_STATE; i++) {
        ((float*)(&x->state[0]))[i] = 0.0f;
        ((float*)(&x->state[1]))[i] = 0.0f;
    }
}

static void run(LV2_Handle handle, uint32_t n_samples) {
    instance* x = (instance*)handle;
    proc_loop((void*)&x->state, &x->in, &x->param, &x->out, &x->store, n_samples);
}

static void deactivate(LV2_Handle handle) {
}

static void cleanup(LV2_Handle handle) {
    free(handle);
}

static const void *extension_data(const char* uri) {
    return NULL;
}

static const LV2_Descriptor descriptor = {
    URI,
    instantiate,
    connect_port,
    activate,
    run,
    deactivate,
    cleanup,
    extension_data
};

LV2_SYMBOL_EXPORT const LV2_Descriptor* lv2_descriptor(uint32_t index) {
    if (index > 0) return NULL;
    return &descriptor;
}
