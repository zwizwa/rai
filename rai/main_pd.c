/*
 *   Pd wrapper for rai dsp processor
 *   Copyright (c) 2000-2014 by Tom Schouten
 */


/* Pd is used as the main development host for RAI plugins.
   This object supports dynamic loading using the .sp format. */


#include "prim.h"  // primitive functions
#include "rai.h"

#define ME "sp: "
#define HAVE_SYNTH 1

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
    t_symbol *proc_name;
    struct proc_instance *proc_instance;
    struct proc_class *proc_class;
    int nb_pd_in;  float **pd_in;
    int nb_pd_out; float **pd_out;
    struct rai_voice voice;
    int cc_map[128];
};

t_int *rai_pd_perform(t_int *w) {
    struct rai_pd *x = (void*)w[1];
    t_int n = w[2];
    proc_instance_run(x->proc_instance, (void*)x->pd_in, (void*)x->pd_out, n);
    return w+3;
}

#define FLOAT(f)      {.a_type = A_FLOAT,  .a_w = {.w_float  = (f)}}
#define SYMBOL(s)     {.a_type = A_SYMBOL, .a_w = {.w_symbol = (s)}}
#define nb_el(x) (sizeof(x)/sizeof(x[0]))



static t_symbol *gensym_n(const char *name, int n) {
    char sname[strlen(name)+20];
    snprintf(sname, sizeof(sname), "%s%d", name, n);
    sname[sizeof(sname)-1] = 0;
    return gensym(sname);
}

static t_symbol *slider_label(const struct proc_class_control *p) {
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
    const struct proc_class_control *p = x->proc_class->info_control;
    for (int i = 0; p[i].desc; i++) {
        t_symbol *s = gensym_n("slider",i);
        if (s->s_thing) {
            t_atom s_desc = {A_SYMBOL, (union word)slider_label(&p[i])};
            typedmess(s->s_thing, gensym("label"), 1, &s_desc);
        }
    }
}
static void update_gui(struct rai_pd *x, int control_index) {
    int param_index = proc_instance_find_control(x->proc_instance, control_index);
    if (param_index < 0) return;
    RAI_NUMBER_T value = proc_instance_get_param(x->proc_instance, param_index);
    t_symbol *s = gensym_n("slider",control_index);
    if (s->s_thing) {
        t_atom s_desc = FLOAT(value);
        typedmess(s->s_thing, gensym("set"), 1, &s_desc);
    }
}
static void rai_pd_update_gui(struct rai_pd *x) {
    for(int i=0; i<proc_instance_nb_control(x->proc_instance); i++) {
        update_gui(x, i);
    }
}


/* Note that adding new objects to the canvas has to wait until after
   the patch is finished loading.  Otherwise it will mess up the
   numbering of objects recorded in the .pd file. */
static void rai_pd_create_gui(struct rai_pd *x, float x_coord, float y_coord) {

    t_canvas *canvas = glist_getcanvas(x->x_canvas);


    /* Create slider on current canvas if there is no receiver. */
    const struct proc_class_control *p = x->proc_class->info_control;
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
    struct proc_class *ri = rai_load_sp(filename->s_name);
    if (ri) {

        /* FIXME: typecheck.  Params and state can be anything, but
           in/out need to be float vectors. */

        int nb_in  = proc_class_param_list_size(ri->info_in);
        int nb_out = proc_class_param_list_size(ri->info_out);

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
            struct proc_instance *proc = proc_instance_new(ri, x->proc_instance);
            if (x->proc_instance) proc_instance_free(x->proc_instance);
            x->proc_instance = proc;
            if (x->proc_class) free(x->proc_class);
            x->proc_class = ri;
            x->proc_name = filename;

            /* Config synth voices */
            bzero(&x->voice, sizeof(x->voice));
            if (0 == rai_voice_init_from_proc(x->proc_instance, &x->voice)) {
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


static void rai_pd_param(struct rai_pd *x, t_symbol *name, t_float value);

static void rai_pd_dsp(struct rai_pd *x, t_signal **sp) {
    for (int i = 0; i < x->nb_pd_in;  i++) { x->pd_in[i]  = sp[i]->s_vec; }
    for (int i = 0; i < x->nb_pd_out; i++) { x->pd_out[i] = sp[i+x->nb_pd_in]->s_vec; }
    dsp_add(rai_pd_perform, 2, x, sp[0]->s_n);
    rai_pd_param(x, gensym("samplerate"), sp[0]->s_sr);
    rai_pd_param(x, gensym("timestep"), 1.0 / sp[0]->s_sr);
}

static void rai_pd_reset_state(struct rai_pd *x) {
    proc_instance_reset_state(x->proc_instance);
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

    /* Need file argument. */
    x->nb_pd_in  = 1;  x->pd_in  = NULL;
    x->nb_pd_out = 0;  x->pd_out = NULL;
    rai_pd_load(x, filename);
    if (!x->proc_instance) {
        // FIXME: pd_free(x); ???
        return NULL;
    }

    return x;
}

static void rai_pd_free(struct rai_pd *x) {
    proc_instance_free(x->proc_instance);
    free(x->proc_class);
    if (x->pd_in)  free(x->pd_in);
    if (x->pd_out) free(x->pd_out);
}

static void rai_pd_param(struct rai_pd *x, t_symbol *name, t_float value) {
    int param_index = proc_instance_find_param(x->proc_instance, name->s_name);
    proc_instance_set_param(x->proc_instance, param_index, value);
}

/* Controls are a subset of parameters exported to a GUI. */
static void rai_pd_control(struct rai_pd *x, t_float f_index, t_float value) {
    int index = f_index;
    int param_index = proc_instance_find_control(x->proc_instance, f_index);
    proc_instance_set_param(x->proc_instance, param_index, value);
    // FIXME: display ui_value = proc_class_control_interpolate(p, value)
}

static void rai_pd_cc_map(struct rai_pd *x, t_symbol *s, int argc, t_atom *argv) {
    int i;
    int nb_control = proc_instance_nb_control(x->proc_instance);
    for (int control_index=0;
         control_index < argc && control_index < nb_control;
         control_index++) {
        float f = atom_getfloat(&argv[control_index]);
        if ((f > 0) && (f < 128)) {
            x->cc_map[(int)f] = control_index;
        }
    }
}

static void rai_pd_post_cc_map(struct rai_pd *x) {
    for (int i = 0; i<128; i++) {
        post("%d -> %d", i, x->cc_map[i]);
    }
}

// the full stack has 4 levels of indirection:
// midi CC -> control_index -> param_index -> byte offset
static void rai_pd_cc(struct rai_pd *x, t_float cc_f, t_float val) {
    if ((cc_f >= 0) && (cc_f <= 127)) {
        int cc = cc_f;
        int control_index = x->cc_map[(int)cc];
        if (control_index >= 0) { // -1 means not mapped
            int param_index = proc_instance_find_control(x->proc_instance, control_index);
            float range_val = val * (1.0f / 127.0f);
            proc_instance_set_param(x->proc_instance, param_index, range_val);

            // FIXME: should we really do this from here??
            update_gui(x, control_index);
        }
        else {
            post("cc %d not mapped", cc);
        }
    }
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
    class_addmethod(rai_pd_class, (t_method)rai_pd_post_cc_map, gensym("post_cc_map"), A_GIMME, A_NULL);
    class_addmethod(rai_pd_class, (t_method)rai_pd_cc, gensym("cc"), A_FLOAT, A_FLOAT, A_NULL);
#if !HAVE_STATIC
    class_addmethod(rai_pd_class, (t_method)rai_pd_load, gensym("load"), A_SYMBOL, A_NULL);
    class_addmethod(rai_pd_class, (t_method)rai_pd_create_gui, gensym("create_gui"), A_DEFFLOAT, A_DEFFLOAT, A_NULL);
#endif
    CLASS_MAINSIGNALIN(rai_pd_class, struct rai_pd, x_f);
}
