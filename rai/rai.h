#ifndef _RAI_H_
#define _RAI_H_

#include <stdbool.h>
#include <math.h>     // host side depends on math.h, but prim.h does not.
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define PROC_CMD_INFO -1

typedef float  float32_t;
typedef double float64_t;
typedef unsigned long word_t; // pointer-sized int

// Used for run-time loading
#define RAI_MAGIC_SIZE 16
#define RAI_MAGIC "rai/x86-64/1    "

#define RAI_VERSION_SIZE 16


/* Processor metadata.

   The idea is to keep the description dumb and non-reduntant.
   Interpretation of the format can be abstracted in a library.

   rai_info is structured data, where all pointers (to strings and
   substructures) are represented by a u64 file offset.

   // FIXME: separate .sp loading from in-memory access
*/


/* Lists in rai.h are implemented as sentinel-terminated arrays, where
   the sentinel is a 0-filled field the size of a native pointer. */

static inline int rai_list_end(const void *x) {
    return *((const void**)x) == NULL;
}

struct rai_info_param;
struct rai_info_control;

struct proc_si;
struct proc_in;
struct proc_param;
struct proc_out;
struct proc_store;

typedef void (*rai_info_run)(struct proc_si    * restrict state,  /* double buffer */
                             struct proc_in    * restrict in,
                             struct proc_param * restrict param,
                             struct proc_out   * restrict out,
                             struct proc_store * restrict store,
                             unsigned int nb_samples);

struct rai_info {
    uint8_t magic[RAI_MAGIC_SIZE];
    uint8_t version[RAI_VERSION_SIZE];
    rai_info_run entry;

    /* Node type info. */
    const struct rai_info_param *info_state;
    const struct rai_info_param *info_in;
    const struct rai_info_param *info_param;
    const struct rai_info_param *info_out;
    const struct rai_info_param *info_store;

    /* param UI info. */
    const struct rai_info_control *info_control;

    /* Initial values. */
    const struct proc_param *init_param;
    const struct proc_state *init_state;
    const struct proc_store *init_store;

    /* Misc info */
    uint32_t build_stamp;
    uint32_t __reserved;

};

/* Implementation note: Compile time (type) lists are implemented as
   "functor macros".  This makes it easier to construct a collection
   of types and functions that abstract over types. */

/* Iterate a macro m over types. */
#define RAI_TYPES_FOR(m)                        \
    m(float32_t)                                \
    m(uint32_t)                                 \
    m(int32_t)

enum rai_type {
#define RAI_DEFINE_TYPE(T) rai_type_##T,
    RAI_TYPES_FOR(RAI_DEFINE_TYPE)
};




/* To keep things simple, all casts are to/from a single numeric type. */
#define RAI_NUMBER_T double
RAI_NUMBER_T rai_get_number(enum rai_type t, const void *rai_src);
void         rai_set_number(enum rai_type t, void *rai_dst, RAI_NUMBER_T val);




struct rai_info_param {
    const char *name;
    const word_t *dims;
    enum rai_type type;
};

int rai_info_param_nb_elements(const struct rai_info_param *pi);
int rai_info_param_list_size(const struct rai_info_param *pi);
int rai_info_param_alloc_size(const struct rai_info_param *pi);

/* Params that have an associated rai_info_control are GUI worthy, and
   contain more meta info. */
enum rai_scale {
    rai_scale_lin = 0,  // s0 + (s1 - s1) * v
    rai_scale_log = 1,  // s1 * (s1 / s1) ^ v
    rai_scale_slog = 2, // s2 * (v/(1-v)) ^ s2   "squeezed log / stretched exp"
};
struct rai_info_control_map {
    double s0; // minimum | center
    double s1; // maximum | exponent
    double range;
    enum rai_scale scale;
};
struct rai_info_control {
    const char *desc;
    const char *unit;
    const struct rai_info_param *param;
    struct rai_info_control_map map;
};

struct rai_info_preset {
    uint8_t  magic[RAI_MAGIC_SIZE];
    uint32_t header_bytes;
    uint32_t timestamp;
    uint32_t payload_bytes;
};

/* Maps v \in [0,1] to the control parameter's user feedback scale. */
static inline float rai_info_control_interpolate(const struct rai_info_control_map *p, float v) {
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
struct rai_info *rai_load_sp(const char *filename);


/* Create proc instance. */
struct rai_proc {
    /* Description */
    const struct rai_info *info;
    /* State variables */
    struct proc_si    * restrict state;
    struct proc_param * restrict param;
    struct proc_store * restrict store;
    /* Indexing information computed from struct rai_info. */
    int *param_offset;  // maps index in info.info_param to offset in storage buffer
    int *param_nb_el;   // nb of grid elements
    int nb_control;
    int nb_param;
};
static inline void rai_proc_run(struct rai_proc *p,
                                struct proc_in  * restrict in,
                                struct proc_out * restrict out,
                                int n) {
    p->info->entry(p->state, in, p->param, out, p->store, n);
}
struct rai_proc *rai_proc_new(const struct rai_info *info,
                              const struct rai_proc *proto);
void rai_proc_free(struct rai_proc *p);

void rai_proc_preset_save(const struct rai_proc *p, const char *filename);
void rai_proc_preset_load(const struct rai_proc *p, const char *filename, int index);

void rai_proc_reset_state(struct rai_proc *p);
void rai_proc_reset_param(struct rai_proc *p);

int rai_proc_find_param(struct rai_proc *p, const char *name);
int rai_proc_find_control(struct rai_proc *p, int c);
void rai_proc_set_param(struct rai_proc *p, int index, RAI_NUMBER_T val);
RAI_NUMBER_T rai_proc_get_param(struct rai_proc *p, int index);

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
    uint32_t nb;
    float32_t *gate;
    float32_t *freq;
    uint32_t next;
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

// for convenience
int rai_voice_init_from_proc(struct rai_proc *p, struct rai_voice *v);
void rai_voice_off(struct rai_voice *v, float freq);
void rai_voice_on(struct rai_voice *v, float freq);
float rai_midi_to_freq(int midi);





/* DEBUG */
typedef void (*rai_log)(const char *msg, ...);
void rai_print_info(const struct rai_info *ri, rai_log log);

#ifdef __cplusplus
}
#endif


#endif


