/*
 *   Pd wrapper for rai dsp processor
 *   Copyright (c) 2000-2013 by Tom Schouten
 */

// Example for including .g.h file

#include "prim.h"  // primitive functions

// #undef PROC_FILE // testing

#define ME "sp: "

#ifdef PROC_FILE
#define HAVE_STATIC 1
#include PROC_FILE // generated code
/* Assume the prefix is default "proc_"
   We just work with flat arrays, not structured grid data. */
#define NB_PARAM  proc_size_param
#define NB_IN     proc_size_in
#define NB_OUT    proc_size_out
#define NB_STATE  proc_size_state
/* Create synth voice allocator if freq and gate params are defined as
   1-dim arrays.  */
#if defined(proc_param_voice_freq) && \
    defined(proc_param_voice_gate) && \
    proc_param_voice_freq == 1 && \
    proc_param_voice_gate == 1
#define HAVE_SYNTH 1
#else
#define HAVE_SYNTH 0
#endif

#else
#define HAVE_STATIC 0
#define HAVE_SYNTH 1
#endif

#include "rai.h"

#include "m_pd.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>


t_class *rai_pd_class;


struct rai_pd {
    t_object x_obj;
    t_float x_f;
#if HAVE_STATIC
    struct proc_si state[2];
    struct proc_param param;
    struct proc_in in;
    struct proc_out out;
    struct proc_store store;
#else
    t_symbol *proc_name;
    struct rai_proc *rai_proc;
    struct rai_info *rai_info;
    int nb_pd_in;  float **pd_in;
    int nb_pd_out; float **pd_out;
    int nb_controls;
#endif
    struct rai_voice voice;
};

t_int *rai_pd_perform(t_int *w) {
    struct rai_pd *x = (void*)w[1];
    t_int n = w[2];
#if HAVE_STATIC
    proc_loop((void*)&x->state, &x->in, &x->param, &x->out, &x->store, n);
#else
    rai_proc_run(x->rai_proc, x->pd_in, x->pd_out, n);
#endif
    return w+3;
}


#if !HAVE_STATIC
static void rai_send_meta(struct rai_pd *x) {
    struct rai_info_control *p = x->rai_info->info_control;
    for (int i = 0; p[i].desc; i++) {
        char sname[32];
        snprintf(sname, sizeof(sname), "slider%d", i);
        sname[sizeof(sname)-1] = 0;
        t_symbol *s = gensym(sname);
        if (s->s_thing) {
            /* Strip whitespace from name. */
            int buflen = 2 + strlen(p[i].desc) + strlen(p[i].unit);
            char buf[buflen];
            strcpy(buf, p[i].desc);
            strcat(buf, "/");
            strcat(buf, p[i].unit);
            for (int j=0; j<buflen; j++) {
                if (isspace(buf[j])) {
                    buf[j] = '_';
                }
            }
            t_atom s_desc = {A_SYMBOL, (union word)gensym(buf)};
            typedmess(s->s_thing, gensym("label"), 1, &s_desc);
        }
    }
}
/* Force code reload by loading stand-alone binary fPIC code.
   Note that libdl won't reload a library if it is opened somewhere else. */
static void rai_pd_load(struct rai_pd *x, t_symbol *filename) {
    struct rai_info *ri = rai_load_bin(filename->s_name);
    if (ri) {

        int nb_in  = rai_info_size(ri->info_in);
        int nb_out = rai_info_size(ri->info_out);

        if (x->pd_in == NULL) {
            /* First run: create I/O and alloc array buffers. */
            while (x->nb_pd_in < nb_in) {
                inlet_new(&x->x_obj, &x->x_obj.ob_pd, gensym("signal"), gensym("signal"));
                x->nb_pd_in++;
            }
            while (x->nb_pd_out < nb_out) {
                outlet_new(&x->x_obj, gensym("signal"));
                x->nb_pd_out++;
            }
            if (x->nb_pd_in)  x->pd_in  = calloc(sizeof(float*), x->nb_pd_in);
            if (x->nb_pd_out) x->pd_out = calloc(sizeof(float*), x->nb_pd_out);
        }

        if (!((nb_in  <= x->nb_pd_in) &&
              (nb_out <= x->nb_pd_out))) {
            post(ME "Can't reload code from %s: I/O not compatible", filename->s_name);
            free(ri);
        }
        else {
            /* Replace code */
            struct rai_proc *proc = rai_proc_new(ri, x->rai_proc);
            if (x->rai_proc) rai_proc_free(x->rai_proc);
            x->rai_proc = proc;
            if (x->rai_info) free(x->rai_info);
            x->rai_info = ri;
            x->proc_name = filename;

            /* Config controls */
            struct rai_info_control *p = ri->info_control;
            for (x->nb_controls = 0; p[x->nb_controls].desc; x->nb_controls++);

            /* Config synth voices */
            bzero(&x->voice, sizeof(x->voice));
            int gate_offset = 0, freq_offset = 0;
            uintptr *dims = NULL;
            if (rai_info_find(ri->info_param, "voice_gate", &gate_offset, &dims) &&
                rai_info_find(ri->info_param, "voice_freq", &freq_offset, &dims)) {
                rai_voice_init(&x->voice,
                               dims[0],
                               proc->param + gate_offset,
                               proc->param + freq_offset);
                post(ME "%d synth voices", dims[0]);
            }

            post(ME "%s (%d->%d) s:%d v:%s",
                 filename->s_name, nb_in, nb_out, ri->build_stamp, ri->version);

            rai_print_info(ri, startpost);
            rai_send_meta(x);
        }
    }
    else {
        post(ME "Can't load code from %s", filename->s_name);
    }
}
#endif


static void rai_pd_param(struct rai_pd *x, t_symbol *name, t_float value);

static void rai_pd_dsp(struct rai_pd *x, t_signal **sp) {
#if HAVE_STATIC
    for (int i = 0; i < NB_IN;  i++) { ((float**)(&x->in))[i]  = sp[i]->s_vec; }
    for (int i = 0; i < NB_OUT; i++) { ((float**)(&x->out))[i] = sp[i+NB_IN]->s_vec; }
#else
    for (int i = 0; i < x->nb_pd_in;  i++) { x->pd_in[i]  = sp[i]->s_vec; }
    for (int i = 0; i < x->nb_pd_out; i++) { x->pd_out[i] = sp[i+x->nb_pd_in]->s_vec; }
#endif
    dsp_add(rai_pd_perform, 2, x, sp[0]->s_n);
    rai_pd_param(x, gensym("samplerate"), sp[0]->s_sr);
    rai_pd_param(x, gensym("timestep"), 1.0 / sp[0]->s_sr);
}


static inline void bzero_float(float *x, int n) {
    for (int i = 0; i < n; i++) x[i] = 0;
}

static void rai_pd_reset_state(struct rai_pd *x) {
#if HAVE_STATIC
    bzero_float((float*)&x->state, 2*NB_STATE);
#else
    int nb_state = rai_info_size(x->rai_info->info_state);
    bzero_float(x->rai_proc->state, nb_state);
#endif
}
static void rai_pd_reset_param(struct rai_pd *x) {
#if HAVE_STATIC
    bzero_float((float*)&x->param, NB_PARAM);
#else
    bzero_float(x->rai_proc->param, rai_info_size(x->rai_info->info_param));
#endif
}

static void rai_pd_note(struct rai_pd *x, t_float freq, t_float gain) {
    if (!x->voice.nb) return;
    if (gain > 0)
        rai_voice_on(&x->voice, freq);
    else
        rai_voice_off(&x->voice, freq);
}

static void *rai_pd_new(t_symbol *filename) {

    struct rai_pd *x = (void*)pd_new(rai_pd_class);

#if HAVE_STATIC
    /* Create I/O */
    for (int i=1; i<NB_IN; i++)
        inlet_new(&x->x_obj, &x->x_obj.ob_pd, gensym("signal"), gensym("signal"));
    for (int i=0; i<NB_OUT; i++)
        outlet_new(&x->x_obj, gensym("signal"));
#else
    /* Need file argument. */
    x->nb_pd_in  = 1;  x->pd_in  = NULL;
    x->nb_pd_out = 0;  x->pd_out = NULL;
    rai_pd_load(x, filename);
    if (!x->rai_proc) {
        // FIXME: pd_free(x); ???
        return NULL;
    }
#endif

    /* Init state and input param vectors. */
    rai_pd_reset_state(x);
    rai_pd_reset_param(x);

    return x;
}

static void rai_pd_free(struct rai_pd *x) {
#if !HAVE_STATIC
    rai_proc_free(x->rai_proc);
    free(x->rai_info);
    if (x->pd_in)  free(x->pd_in);
    if (x->pd_out) free(x->pd_out);
#endif
}

static void rai_pd_param(struct rai_pd *x, t_symbol *name, t_float value) {
    post("rai_pd_param: %s %f", name->s_name, value);
#if HAVE_STATIC
#define HANDLE_PARAM(p_name, ...) \
    if (gensym(#p_name) == name) { *(float *)(&x->param.p_name) = value; return; }
    proc_for_param(HANDLE_PARAM);
#else
    int offset;
    uintptr *dims;
    if (rai_info_find(x->rai_info->info_param, name->s_name, &offset, &dims)) {
        x->rai_proc->param[offset] = value;
    }
#endif
}

/* Controls are a subset of parameters exported to a GUI. */
static void rai_pd_control(struct rai_pd *x, t_float f_index, t_float value) {
    int index = f_index;
#if HAVE_STATIC
    post("rai_pd_control: %d %f", index, value);
    // FIXME: not implemented: see main_vst.c
#else
    if ((index < 0) || (index >= x->nb_controls)) return;
    struct rai_info_control *p = &x->rai_info->info_control[index];
    x->rai_proc->param[p->index] = value;
    float ui_value = rai_info_control_interpolate(p, value);
    //post("rai_pd_control: %s (%f) %f", p->desc, value, ui_value);
#endif
}




void EXTERN_SETUP (void) {
    rai_pd_class = class_new(gensym(EXTERN_NAME), (t_newmethod)rai_pd_new,
                             (t_method)rai_pd_free, sizeof(struct rai_pd), 0, A_DEFSYMBOL, 0);
    class_addmethod(rai_pd_class, (t_method)rai_pd_dsp, gensym("dsp"), 0);
    class_addmethod(rai_pd_class, (t_method)rai_pd_reset_state, gensym("reset"), A_NULL);
    class_addmethod(rai_pd_class, (t_method)rai_pd_param, gensym("param"), A_SYMBOL, A_FLOAT, A_NULL);
    class_addmethod(rai_pd_class, (t_method)rai_pd_control, gensym("control"), A_FLOAT, A_FLOAT, A_NULL);
    class_addmethod(rai_pd_class, (t_method)rai_pd_note, gensym("note"), A_FLOAT, A_FLOAT, A_NULL);
#if !HAVE_STATIC
    class_addmethod(rai_pd_class, (t_method)rai_pd_load, gensym("load"), A_SYMBOL, A_NULL);
#endif
    CLASS_MAINSIGNALIN(rai_pd_class, struct rai_pd, x_f);
}
