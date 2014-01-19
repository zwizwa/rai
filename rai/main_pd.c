/*
 *   Pd wrapper for rai dsp processor
 *   Copyright (c) 2000-2013 by Tom Schouten
 */

// Example for including .g.h file

#include "prim.h"  // primitive functions
#include "rai.h"

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


#include "m_pd.h"
#include "g_canvas.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>


t_class *rai_pd_class;


struct rai_pd {
    t_object x_obj;
    t_float x_f;
    t_canvas *x_canvas;
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
#endif
    struct rai_voice voice;
    int cc_map[128];
};

t_int *rai_pd_perform(t_int *w) {
    struct rai_pd *x = (void*)w[1];
    t_int n = w[2];
#if HAVE_STATIC
    proc_loop((void*)&x->state, &x->in, &x->param, &x->out, &x->store, n);
#else
    rai_proc_run(x->rai_proc, (void*)x->pd_in, (void*)x->pd_out, n);
#endif
    return w+3;
}

#define FLOAT(f)      {.a_type = A_FLOAT,  .a_w = {.w_float  = (f)}}
#define SYMBOL(s)     {.a_type = A_SYMBOL, .a_w = {.w_symbol = (s)}}
#define nb_el(x) (sizeof(x)/sizeof(x[0]))




#if !HAVE_STATIC

static t_symbol *gensym_n(const char *name, int n) {
    char sname[strlen(name)+20];
    snprintf(sname, sizeof(sname), "%s%d", name, n);
    sname[sizeof(sname)-1] = 0;
    return gensym(sname);
}

static t_symbol *slider_label(const struct rai_info_control *p) {
    int buflen = 2 + strlen(p->desc) + strlen(p->unit);
    char buf[buflen];
    strcpy(buf, p->desc);
    strcat(buf, "/");
    strcat(buf, p->unit);
    /* Symbols can't have whitespace. */
    for (int j=0; j<buflen; j++) {
        if (isspace(buf[j])) buf[j] = '_';
    }
    return gensym(buf);
}

static void update_gui_labels(struct rai_pd *x) {
    const struct rai_info_control *p = x->rai_info->info_control;
    for (int i = 0; p[i].desc; i++) {
        t_symbol *s = gensym_n("slider",i);
        if (s->s_thing) {
            t_atom s_desc = {A_SYMBOL, (union word)slider_label(&p[i])};
            typedmess(s->s_thing, gensym("label"), 1, &s_desc);
        }
    }
}
static void update_gui(struct rai_pd *x, int control_index) {
    int param_index = rai_proc_find_control(x->rai_proc, control_index);
    if (param_index < 0) return;
    RAI_NUMBER_T value = rai_proc_get_param(x->rai_proc, param_index);
    t_symbol *s = gensym_n("slider",control_index);
    if (s->s_thing) {
        post("update_gui %d %d %f", control_index, param_index, value);
        t_atom s_desc = FLOAT(value);
        typedmess(s->s_thing, gensym("set"), 1, &s_desc);
    }
}
static void rai_pd_update_gui(struct rai_pd *x) {
    for(int i=0; i<x->rai_proc->nb_control; i++) {
        update_gui(x, i);
    }
}


/* Note that adding new objects to the canvas has to wait until after
   the patch is finished loading.  Otherwise it will mess up the
   numbering of objects recorded in the .pd file. */
static void rai_pd_create_gui(struct rai_pd *x, float x_coord, float y_coord) {

    t_canvas *canvas = glist_getcanvas(x->x_canvas);


    /* Create slider on current canvas if there is no receiver. */
    const struct rai_info_control *p = x->rai_info->info_control;
    t_symbol *obj = gensym("obj");
    t_symbol *hsl = gensym("hsl");
    t_symbol *empty  = gensym("empty");
    t_symbol *msg = gensym("msg");
    t_symbol *control = gensym("control");
    t_symbol *dollar_one = gensym("$1");
    t_symbol *connect = gensym("connect");
    int synth_obj_no = canvas_getindex(canvas, (void*)x);
    for (int i = 0; p[i].desc; i++) {
        t_symbol *slider = gensym_n("slider", i);
        if (!slider->s_thing) {
            int obj_no = canvas_getindex(canvas, NULL);

            /* Examples of the format can be found in .pd files, e.g.:
               #X obj 55 54 hsl 128 15 0 127 0 0 empty empty empty -2 -8 0 10 -262144 -1 -1 0 1;
               see also hslider_new() */
            {
                t_atom args[] = {
                    /** create object at location **/
                    SYMBOL(obj),
                    FLOAT(x_coord),
                    FLOAT(y_coord + 30 * i),
                    SYMBOL(hsl),
                    /** arguments passed to hslider_new() */
                    /* w */    FLOAT(128),
                    /* h */    FLOAT(15),
                    /* min */  FLOAT(0),
                    /* max */  FLOAT(1),
                    /* lilo */ FLOAT(0),
                    /* isa */  FLOAT(1),
                    /* snd */  SYMBOL(empty),
                    /* rcv */  SYMBOL(slider),
                    /* lab */  SYMBOL(slider_label(&p[i])),
                    /* ldx */  FLOAT(-2),
                    /* ldy */  FLOAT(-8),
                    /* fsf */  FLOAT(0),
                    /* fs */   FLOAT(10),
                    /* bflcol[0] */ FLOAT(-262144),
                    /* bflcol[1] */ FLOAT(-1),
                    /* bflcol[2] */ FLOAT(-1),
                    /* val */       FLOAT(0),
                    /* steady */    FLOAT(1),
                };
                pd_forwardmess((t_pd*)canvas, nb_el(args), args);
            }
            {
                t_atom args[] = {
                    /** create message object.  it converts slider output to tagged message **/
                    SYMBOL(msg),
                    FLOAT(x_coord + 200),
                    FLOAT(y_coord + 30 * i + 15),
                    SYMBOL(control),
                    FLOAT(i),
                    SYMBOL(dollar_one),
                };
                pd_forwardmess((t_pd*)canvas, nb_el(args), args);
            }
            {
                t_atom args[] = {
                    /** connect slider to message object. **/
                    SYMBOL(connect),
                    FLOAT(obj_no), FLOAT(0),
                    FLOAT(obj_no+1), FLOAT(0)
                };
                pd_forwardmess((t_pd*)canvas, nb_el(args), args);
            }
            {
                t_atom args[] = {
                    /** connect message object to sp_host object. **/
                    SYMBOL(connect),
                    FLOAT(obj_no+1), FLOAT(0),
                    FLOAT(synth_obj_no), FLOAT(0)
                };
                pd_forwardmess((t_pd*)canvas, nb_el(args), args);
            }
        }
    }
    rai_pd_update_gui(x);
}






/* Force code reload by loading stand-alone binary fPIC code.
   Note that libdl won't reload a library if it is opened somewhere else. */
static void rai_pd_load(struct rai_pd *x, t_symbol *filename) {
    struct rai_info *ri = rai_load_sp(filename->s_name);
    if (ri) {

        int nb_in  = rai_info_param_list_size(ri->info_in);
        int nb_out = rai_info_param_list_size(ri->info_out);

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

            /* Config synth voices */
            bzero(&x->voice, sizeof(x->voice));
            if (0 == rai_voice_init_from_proc(x->rai_proc, &x->voice)) {
                post(ME "%d synth voices", x->voice.nb);
            }

            post(ME "%s (%d->%d) s:%d v:%s",
                 filename->s_name, nb_in, nb_out, ri->build_stamp, ri->version);

            rai_print_info(ri, startpost);
            update_gui_labels(x);
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

static void rai_pd_reset_state(struct rai_pd *x) {
#if HAVE_STATIC
    // FIXME: not implemented
#else
    rai_proc_reset_state(x->rai_proc);
#endif
}
static void rai_pd_reset_param(struct rai_pd *x) {
#if HAVE_STATIC
    // FIXME: not implemented
#else
    rai_proc_reset_param(x->rai_proc);
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
    x->x_canvas = canvas_getcurrent();
    for(int i=0; i<128; i++) x->cc_map[i] = -1;

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
#define HANDLE_PARAM(p_name, p_type, ...)                               \
    if (gensym(#p_name) == name) { *(float *)(&x->param.p_name) = value; return; }
    proc_for_param(HANDLE_PARAM);
#else
    int param_index = rai_proc_find_param(x->rai_proc, name->s_name);
    rai_proc_set_param(x->rai_proc, param_index, value);
#endif
}

/* Controls are a subset of parameters exported to a GUI. */
static void rai_pd_control(struct rai_pd *x, t_float f_index, t_float value) {
    int index = f_index;
#if HAVE_STATIC
    post("rai_pd_control: %d %f", index, value);
    // FIXME: not implemented: see main_vst.c
#else
    int param_index = rai_proc_find_control(x->rai_proc, f_index);
    rai_proc_set_param(x->rai_proc, param_index, value);
    // FIXME: display ui_value = rai_info_control_interpolate(p, value)
#endif
}

static void rai_pd_cc_map(struct rai_pd *x, t_symbol *s, int argc, t_atom *argv) {
#if !HAVE_STATIC
    int i;
    post("nb_control = %d\n", x->rai_proc->nb_control);
    for (int control_index=0;
         control_index < argc && control_index < x->rai_proc->nb_control;
         control_index++) {
        float f = atom_getfloat(&argv[control_index]);
        if ((f > 0) && (f < 128)) {
            x->cc_map[(int)f] = control_index;
        }
    }
#endif
}
// 4 levels of indirections:
// midi CC -> control_index -> param_index -> byte offset

static void rai_pd_post_cc_map(struct rai_pd *x) {
    for (int i = 0; i<128; i++) {
        post("%d -> %d", i, x->cc_map[i]);
    }
}

static void rai_pd_cc(struct rai_pd *x, t_float cc_f, t_float val) {
#if !HAVE_STATIC
    if ((cc_f >= 0) && (cc_f <= 127)) {
        int cc = cc_f;
        int control_index = x->cc_map[(int)cc];
        if (control_index >= 0) { // -1 means not mapped
            int param_index = rai_proc_find_control(x->rai_proc, control_index);
            post("param_index = %d", param_index);
            float range_val = val * (1.0f / 127.0f);
            rai_proc_set_param(x->rai_proc, param_index, range_val);

            float check = rai_proc_get_param(x->rai_proc, param_index);
            if (range_val != check) {
                post("param %d stuck", param_index);
            }

            // FIXME: should we really do this from here??
            update_gui(x, control_index);
        }
        else {
            post("cc %d not mapped", cc);
            rai_pd_post_cc_map(x);
        }
    }
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
    class_addmethod(rai_pd_class, (t_method)rai_pd_cc_map, gensym("cc_map"), A_GIMME, A_NULL);
    class_addmethod(rai_pd_class, (t_method)rai_pd_cc, gensym("cc"), A_FLOAT, A_FLOAT, A_NULL);
#if !HAVE_STATIC
    class_addmethod(rai_pd_class, (t_method)rai_pd_load, gensym("load"), A_SYMBOL, A_NULL);
    class_addmethod(rai_pd_class, (t_method)rai_pd_create_gui, gensym("create_gui"), A_DEFFLOAT, A_DEFFLOAT, A_NULL);
#endif
    CLASS_MAINSIGNALIN(rai_pd_class, struct rai_pd, x_f);
}
