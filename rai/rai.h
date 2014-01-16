#ifndef _RAI_H_
#define _RAI_H_

#include <stdbool.h>
#include <math.h>     // host side depends on math.h, but prim.h does not.
#include <stddef.h>

#define PROC_CMD_INFO -1

typedef float float32;
typedef int int32;
typedef unsigned long long u64;
typedef unsigned int u32;
typedef unsigned char u8;
typedef unsigned long word_t; // pointer-sized int
typedef unsigned long uintptr;
typedef float fload_guess; // FIXME: remove this hack

// Used for run-time loading
#define RAI_MAGIC_SIZE 16
#define RAI_MAGIC "rai/x86-64/1    "

#define RAI_VERSION_SIZE 16


/* The main approach for metadata is to keep the data dumb and
   non-reduntant.  Interpretation of the format can be abstracted in a
   library.

   rai_info is structured data, where all pointers (to strings and
   substructures) are represented by a u64 file offset.
*/


/* Lists in rai.h are implemented using sentinel-terminated arrays,
   where the sentinal is a 0-filled field the size of a pointer. */
static inline int rai_list_end(const void *x) {
    return *((const void**)x) == NULL;
}

typedef void (*rai_info_run)(float *state,
                             float **in,
                             float *param,
                             float **out,
                             float *store,
                             unsigned int nb_samples);
struct rai_info_param;
struct rai_info_control;

struct rai_info {
    u8 magic[RAI_MAGIC_SIZE];
    u8 version[RAI_VERSION_SIZE];
    rai_info_run entry;

    /* Node type info. */
    struct rai_info_param *info_state;
    struct rai_info_param *info_in;
    struct rai_info_param *info_param;
    struct rai_info_param *info_out;
    struct rai_info_param *info_store;

    /* param UI info. */
    struct rai_info_control *info_control;

    /* Misc info */
    u32 build_stamp;
    u32 __reserved;

};


/* param meta info */
enum rai_scale {
    rai_scale_lin = 0,  // s0 + (s1 - s1) * v
    rai_scale_log = 1,  // s1 * (s1 / s1) ^ v
    rai_scale_slog = 2, // s2 * (v/(1-v)) ^ s2   "squeezed log / stretched exp"
};
struct rai_info_control {
    const char *desc;
    const char *unit;
    int index;
    float s0; // minimum | center
    float s1; // maximum | exponent
    float range;
    enum rai_scale scale;
};

enum rai_type {
    rai_type_float32 = 0,  // == float32
    rai_type_uint32  = 1,
    rai_type_int32   = 2,
};

struct rai_info_param {
    const char *name;
    uintptr *dims;
    enum rai_type type;
};

struct rai_info_preset {
    u8 magic[RAI_MAGIC_SIZE];
    u32 header_bytes;
    u32 timestamp;
    u32 payload_bytes;
};

/* Maps v \in [0,1] to the control parameter's user feedback scale. */
static inline float rai_info_control_interpolate(const struct rai_info_control *p, float v) {
    float out_v;
    switch(p->scale) {
    case rai_scale_lin:  out_v = p->s0 + (p->s1 - p->s0) * v;   break;
    case rai_scale_log:  out_v = p->s0 * pow(p->s1 / p->s0, v); break;
    case rai_scale_slog: out_v = p->s0 * pow(v / (1-v), p->s1); break;
    default:             out_v = v;                             break;
    }
    return out_v;
}


/* Load .sp class */
struct rai_info *rai_load_bin(const char *filename);

/* Allocation size in nb floats */
int rai_info_size(const struct rai_info_param *rp);

/* Parameter offset and dimensions. */
bool rai_info_find(const struct rai_info_param *pi,
                   const char *name, int *offset, uintptr **dims);

/* Create proc instance. */
struct rai_proc {
    const struct rai_info *info;
    float *state;
    float *param;
    float *store;
};
static inline void rai_proc_run(struct rai_proc *p, float **in, float **out, int n) {
    p->info->entry(p->state, in, p->param, out, p->store, n);
}
struct rai_proc *rai_proc_new(const struct rai_info *info,
                              const struct rai_proc *proto);
void rai_proc_free(struct rai_proc *p);

void rai_proc_preset_save(const struct rai_proc *p, const char *filename);
void rai_proc_preset_load(const struct rai_proc *p, const char *filename, int index);


#ifndef PROC
#define PROC(x) proc_##x
#endif

#define PROC_NB_EL(x,t) ((sizeof(x) / sizeof(t)))
#define PROC_PARAM_DIM(name) PROC_NB_EL(((struct PROC(param) *)0)->name, float)

#define proc_size_param   PROC_NB_EL(struct proc_param, float)
#define proc_size_in      PROC_NB_EL(struct proc_in,    float*)
#define proc_size_out     PROC_NB_EL(struct proc_out,   float*)
#define proc_size_state   PROC_NB_EL(struct proc_si,    float)
#define proc_size_store   PROC_NB_EL(struct proc_store, float)

/* Synth stuff */
struct rai_voice {
    int nb;
    float *gate;
    float *freq;
    int next;
};
/* Simple round-robin voice allocator.
   Some extensions:
   - "pedal" action to set the decay of all voices
   - voice stealing based on envelope decay
*/

static inline void rai_voice_init(struct rai_voice *v, int nb, float *gate, float *freq) {
    v->nb = nb;
    v->gate = gate;
    v->freq = freq;
    v->next = 0;
}

static inline void rai_voice_on(struct rai_voice *v, float freq) {
    v->gate[v->next] = 1;
    v->freq[v->next] = freq;
    v->next = (v->next + 1) % v->nb;
}
static inline void rai_voice_off(struct rai_voice *v, float freq) {
    /* Turn off 0 or 1 notes, start from oldest. */
    for (int i=1; i<=v->nb; i++) {
        int j = (v->nb + v->next - i) % v->nb;
        if (v->freq[j] == freq) {
            v->gate[j] = 0;
            break;
        }
    }
}
static inline float rai_midi_to_freq(int midi) {
    // 69 -> 440
    float fmidi = midi-69;
    return 440 * pow(2, fmidi/12);
}




/* DEBUG */
typedef void (*rai_log)(const char *msg, ...);
void rai_print_info(const struct rai_info *ri, rai_log log);

#endif


