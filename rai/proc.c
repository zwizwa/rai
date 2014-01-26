#include "proc.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>


int proc_class_param_nb_elements(const struct proc_class_param *pi) {
    int nb_elements = 1;
    for (int i=0; !proc_list_end(&(pi->dims[i])); i++) {
        nb_elements *= pi->dims[i];
    }
    return nb_elements;
}

int proc_class_param_list_size(const struct proc_class_param *pi) {
    int i;
    for (i = 0; !proc_list_end(&pi[i]); i++);
    return i;
}
static int proc_class_control_list_size(const struct proc_class_control *ci) {
    int i;
    for (i = 0; !proc_list_end(&ci[i]); i++);
    return i;
}

static int proc_min(int x, int y) { return x < y ? x : y; }

static int proc_type_sizeof(enum proc_type t) {
#define PROC_TYPE_CASE_SIZEOF(_t) case proc_type_##_t: return sizeof(_t);
    switch(t) {
        PROC_TYPES_FOR(PROC_TYPE_CASE_SIZEOF)
    default: return 0;
    }
}

int proc_class_param_alloc_size(const struct proc_class_param *pi) {
    int bytes = 0;
    for (int i = 0; !proc_list_end(&pi[i]); i++) {
        bytes += proc_class_param_nb_elements(&pi[i])
            *proc_type_sizeof(pi[i].type);
    }
    return bytes;
}

PROC_NUMBER_T proc_get_number(enum proc_type t, const void *p) {
#define PROC_CASE_GET_NUMBER(_PROC_T_) case proc_type_##_PROC_T_: return (PROC_NUMBER_T)*((_PROC_T_*)p);
    switch(t) {
        PROC_TYPES_FOR(PROC_CASE_GET_NUMBER)
    default:
        return (PROC_NUMBER_T)0;
    }
}
void proc_set_number(enum proc_type t, void *p, PROC_NUMBER_T val) {
#define PROC_CASE_SET_NUMBER(_PROC_T_) case proc_type_##_PROC_T_: *((_PROC_T_*)p) = (_PROC_T_)val; break;
    switch(t) {
        PROC_TYPES_FOR(PROC_CASE_SET_NUMBER)
    }
}


/* Initialize from prototype if available, else use defaults.  This is
   useful for state-preserving code updates.  Note that consistency of
   the structs cannot be verified here, only size. */
static void param_init(const struct proc_class_param *pi,
                       void *new, const void *old, int old_max_bytes) {
    int byte_offset = 0;
    for (int i = 0; !proc_list_end(&pi[i]); i++) {
        enum proc_type t = pi[i].type;
        int nb_elements =  proc_class_param_nb_elements(&pi[i]);
        int byte_size = proc_type_sizeof(t) * nb_elements;

        if (byte_size + byte_offset <= old_max_bytes) {
            memcpy(new + byte_offset, old + byte_offset, byte_size);
        }
        byte_offset += byte_size;
    }
}

struct proc_instance {
    /* Description */
    const struct proc_class *info;
    /* State variables */
    struct proc_si    * restrict state;
    struct proc_param * restrict param;
    struct proc_store * restrict store;
    /* Indexing information computed from struct proc_class. */
    int *param_offset;  // maps index in info.info_param to offset in storage buffer
    int *param_nb_el;   // nb of grid elements
    int nb_control;
    int nb_param;
    int size_state;
    int size_param;
    int size_store;
};

int proc_instance_nb_control(struct proc_instance *p) {
    return p->nb_control;
}
int proc_instance_nb_param(struct proc_instance *p) {
    return p->nb_param;
}


void proc_instance_reset_state(struct proc_instance *p) {
    param_init(p->info->info_state, p->state, p->info->init_state, p->size_state);
}
void proc_instance_reset_store(struct proc_instance *p) {
    param_init(p->info->info_store, p->store, p->info->init_store, p->size_store);
}
struct proc_instance *proc_instance_new(const struct proc_class *info,
                                        const struct proc_instance *proto) {
    struct proc_instance *p = malloc(sizeof(*p));
    p->info = info;

    /* Allocate and build indexing data. */
    p->nb_param   = proc_class_param_list_size(info->info_param);
    p->nb_control = proc_class_control_list_size(info->info_control);
    p->param_offset = malloc(p->nb_param * sizeof(p->param_offset[0]));
    p->param_nb_el  = malloc(p->nb_param * sizeof(p->param_nb_el[0]));

    /* Allocate in 1 chunk to improve locality. */
    p->size_state = proc_class_param_alloc_size(info->info_state);
    p->size_param = proc_class_param_alloc_size(info->info_param);
    p->size_store = proc_class_param_alloc_size(info->info_store);

    int buf_size = 2 * p->size_state + p->size_param + p->size_store;
    void *buf = calloc(1,buf_size);
    p->param    = buf; buf += p->size_param;
    p->state    = buf; buf += (2 * p->size_state);
    p->store    = buf;


    /* Init params & param meta data */
    for (int i = 0, byte_offset = 0; i < p->nb_param; i++) {
        const struct proc_class_param *pi = &info->info_param[i];
        p->param_offset[i] = byte_offset;
        p->param_nb_el[i]  = proc_class_param_nb_elements(pi);
        int byte_size = proc_type_sizeof(pi->type) * p->param_nb_el[i];
        byte_offset += byte_size;
        /* Params don't have initial values, but since we're stateful
           we have to initialize. */
        proc_instance_set_param(p, i, 0.0);
    }

    /* Initialize from initial values specified in code. */
    proc_instance_reset_state(p);
    proc_instance_reset_store(p);

    /* Copy from proto or init struct. */
    if (proto) {
        param_init(info->info_param, p->param, proto->param, proc_min(p->size_param, proto->size_param));
        param_init(info->info_store, p->store, proto->store, proc_min(p->size_store, proto->size_store));
        param_init(info->info_state, p->state, proto->state, proc_min(p->size_state, proto->size_state));
    }
    return p;
}
void proc_instance_free(struct proc_instance *p) {
    free(p->param);
    free(p->param_offset);
    free(p->param_nb_el);
    free(p);
}

void proc_instance_run(struct proc_instance *p,
                  struct proc_in  * restrict in,
                  struct proc_out * restrict out,
                  int n) {
    p->info->entry(p->state, in, p->param, out, p->store, n);
    if (n&1) {
        /* Make sure ping-pong buffer is ready next time. */
        memcpy(p->state, (void*)p->state + p->size_state, p->size_state);
    }
}

void proc_instance_preset_save(const struct proc_instance *p, const char *filename) {
    FILE *f = fopen(filename, "a");
    if (!f) return;
    struct proc_class_preset hdr = {
        .magic = "preset",
        .header_bytes = sizeof(hdr),
        .timestamp = time(NULL),
        .payload_bytes = proc_class_param_alloc_size(p->info->info_param),
    };
    fwrite(&hdr, 1, sizeof(hdr), f);
    fwrite(p->param, 1, hdr.payload_bytes, f);
    fclose(f);
}


void proc_instance_preset_load(const struct proc_instance *p, const char *filename, int index) {
    FILE *f = fopen(filename, "r");
    if (!f) return;
    struct proc_class_preset hdr;
    do {
        int nb_bytes = proc_class_param_alloc_size(p->info->info_param);
        if (sizeof(hdr) != fread(&hdr, 1, sizeof(hdr), f)) goto error;
        if (nb_bytes != hdr.payload_bytes) goto error;
        if (nb_bytes != fread(p->param, 1, nb_bytes, f)) goto error;
    } while(index--);
  error:
    fclose(f);
}

int proc_instance_find_param(struct proc_instance *p, const char *name) {
    for (int i = 0; !proc_list_end(&p->info->info_param[i]); i++) {
        if (!strcmp(p->info->info_param[i].name, name)) return i;
    }
    return -1;
}

int proc_instance_find_control(struct proc_instance *p, int c) {
    if ((c < 0) || (c >= p->nb_control)) return -1;
    return p->info->info_control[c].param;
}

/* Set will set the entire grid.  Get will only pick the first element. */
void proc_instance_set_param(struct proc_instance *p, int index, PROC_NUMBER_T val) {
    if ((index < 0) || (index >= p->nb_param)) {
        return;
    }
    void *param_buf = (void*)p->param + p->param_offset[index];
    enum proc_type t = p->info->info_param[index].type;

    int nb = p->param_nb_el[index];
    int base_bytes = proc_type_sizeof(t);
    for (int i=0; i<nb; i++) {
        proc_set_number(t, param_buf, val);
        param_buf += base_bytes;
    }
}
PROC_NUMBER_T proc_instance_get_param(struct proc_instance *p, int index) {
    if ((index < 0) || (index >= p->nb_param)) {
        return 0;
    }
    void *param_buf = (void*)p->param + p->param_offset[index];
    return proc_get_number(p->info->info_param[index].type, param_buf);
}


/* Simple round-robin voice allocator.
   Some extensions:
   - "pedal" action to set the decay of all voices
   - voice stealing based on envelope decay
*/

void proc_voice_init(struct proc_voice *v, int nb, float *gate, float *freq) {
    v->nb = nb;
    v->gate = gate;
    v->freq = freq;
    v->next = 0;
}


int proc_voice_init_from_proc(struct proc_instance *p, struct proc_voice *v) {
    const struct proc_class_param *ip = p->info->info_param;
    int gate_index = proc_instance_find_param(p, "voice_gate"); if (gate_index < 0) return -1;
    int freq_index = proc_instance_find_param(p, "voice_freq"); if (freq_index < 0) return -1;
    if (ip[gate_index].type != proc_type_float32_t) return -1;
    if (ip[freq_index].type != proc_type_float32_t) return -1;
    int nb =  p->param_nb_el[gate_index];
    if (nb != p->param_nb_el[freq_index]) return -1;
    proc_voice_init(v, nb,
                    (void*)p->param + p->param_offset[gate_index],
                    (void*)p->param + p->param_offset[freq_index]);
    return 0;
}
void proc_voice_on(struct proc_voice *v, float freq) {
    v->gate[v->next] = 1;
    v->freq[v->next] = freq;
    v->next = (v->next + 1) % v->nb;
}
void proc_voice_off(struct proc_voice *v, float freq) {
    /* Turn off 0 or 1 notes, start from oldest. */
    for (int i=1; i<=v->nb; i++) {
        int j = (v->nb + v->next - i) % v->nb;
        if (v->freq[j] == freq) {
            v->gate[j] = 0;
            break;
        }
    }
}
float proc_midi_to_freq(int midi) {
    // 69 -> 440
    float fmidi = midi-69;
    return 440 * pow(2, fmidi/12);
}


/* DEBUG */
static void proc_print_info_param(const char *tag,
                                  const struct proc_class_param *pi,
                                  proc_log log) {
    log("%s %d:\n", tag, proc_class_param_alloc_size(pi));
    for (int i = 0; !proc_list_end(&pi[i]); i++) {
        log("\t%d %s", i, pi[i].name);
        const word_t *dims = pi[i].dims;
        int size = 1;
        for (int d = 0; dims[d]; d++) {
            log(" %d", dims[d]);
            size *= dims[d];
        }
        log(" (size=%d)\n", size);
    }
}
static void proc_print_info_control(const char *tag,
                                    const struct proc_class_control *pi,
                                    proc_log log) {
    log("%s\n", tag);
    for (int i = 0; !proc_list_end(&pi[i]); i++) {
        log("\t%s(%s) [%f,%f]\n",
            pi[i].desc,
            pi[i].unit,
            pi[i].map.s0,
            pi[i].map.s1);
    }
}
void proc_print_info(const struct proc_class *ri, proc_log log) {
    if (!log) log = (proc_log)printf;
    proc_print_info_param("param", ri->info_param, log);
    proc_print_info_param("in",    ri->info_in, log);
    proc_print_info_param("out",   ri->info_out, log);
    proc_print_info_param("state", ri->info_state, log);

    proc_print_info_control("control", ri->info_control, log);
}

/* Maps v \in [0,1] to the control parameter's user feedback scale. */
float proc_class_control_interpolate(const struct proc_class_control_map *p, float v) {
    float out_v;
    switch(p->scale) {
    case proc_scale_lin:  out_v = p->s0 + (p->s1 - p->s0) * v;   break;
    case proc_scale_log:  out_v = p->s0 * pow(p->s1 / p->s0, v); break;
    case proc_scale_slog: out_v = p->s0 * pow(v / (1-v), p->s1); break;
    default:              out_v = v;                             break;
    }
    return out_v;
}
